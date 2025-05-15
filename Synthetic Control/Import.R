# Load necessary libraries
install.packages(c("Synth", "dplyr", "readr", "ggplot2", "gsynth"))
library(Synth)
library(dplyr)
library(readr)
library(ggplot2)
library(readxl)
library(tidyr)
library(gsynth)

# Set working directory
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  current_path <- rstudioapi::getActiveDocumentContext()$path
  if (nzchar(current_path)) {
    setwd(dirname(current_path))
  } else {
    warning("No active document. Working directory not changed.")
  }
}

# --------------------- Setup Data and Variables ---------------------

# Process data
df <- read_excel("Import.xlsx")
df_clean <- df %>%
  select(refYear, reporterISO, reporterDesc, primaryValue) %>%
  filter(!is.na(primaryValue))
full_data <- df_clean %>%
  group_by(refYear, reporterISO) %>%
  summarise(total_value = sum(primaryValue, na.rm = TRUE), .groups = "drop")
full_data <- full_data %>%
  complete(reporterISO, refYear = 1997:2021, fill = list(total_value = 0))

# Check structure
head(full_data)

# Do conversions necessary for synth
full_data <- as.data.frame(full_data)
full_data$reporterISONum <- as.numeric(factor(full_data$reporterISO))
treated_country <- "USA"
treated_country_ID <- as.numeric(factor(treated_country, levels = unique(full_data$reporterISO)))
full_data$refYear <- as.numeric(full_data$refYear)

# Add predictors
pred <- read_excel("TotalImport.xlsx")
pred_clean <- pred %>%
  select(refYear, reporterISO, reporterDesc, primaryValue) %>%
  filter(!is.na(primaryValue))
pred_data <- pred_clean %>%
  group_by(refYear, reporterISO) %>%
  summarise(total_val = sum(primaryValue, na.rm = TRUE), .groups = "drop")
pred_data <- pred_data %>%
  complete(reporterISO, refYear = 1997:2021, fill = list(total_val = 0))
gdp_pop <- read_excel("GDP_Pop_Data.xlsx")
gdp_pop_long <- gdp_pop %>%
  pivot_longer(
    cols = 5:ncol(gdp_pop),
    names_to = "year",
    values_to = "value"
  ) %>%
  mutate(
    year = as.numeric(substr(year, 1, 4))
  )
gdp_pop_wide <- gdp_pop_long %>%
  select(`Country Code`, Series.Code = `Series Code`, year, value) %>%
  pivot_wider(
    names_from = Series.Code,
    values_from = value
  )
gdp_pop_clean <- gdp_pop_wide %>%
  rename(
    reporterISO = `Country Code`,
    refYear = year,
    gdp_current_usd = `NY.GDP.MKTP.CD`,
    population_total = `SP.POP.TOTL`
  )

# Do conversions necessary for synth
pred_data <- as.data.frame(pred_data)
gdp_pop_clean <- as.data.frame(gdp_pop_clean)
full_data <- full_data %>%
  left_join(pred_data, by = c("refYear", "reporterISO"))
full_data <- full_data %>%
  left_join(gdp_pop_clean, by = c("reporterISO", "refYear"))
# Add log transformations of total import value and gdp
full_data$log_gdp_current_usd <- full_data$gdp_current_usd
full_data$log_total_val <- full_data$total_val
full_data <- full_data %>%
  mutate(gdp_current_usd = ifelse(gdp_current_usd > 0, log(gdp_current_usd), 0.0))
full_data <- full_data %>%
  mutate(total_val = ifelse(total_val > 0, log(total_val), 0.0))

# --------------------- Run Grid Search For Best Synth Model ---------------------

base_predictors <- c(
  "total_val",
  "population_total",
  "gdp_current_usd",
  "log_total_val",
  "log_gdp_current_usd"
)

predictor_sets <- list()
predictor_sets[[paste0("set", 1)]] <- "total_value"
index <- 2

for (k in 1:length(base_predictors)) {
  combos <- combn(base_predictors, k, simplify = FALSE)
  for (combo in combos) {
    predictor_sets[[paste0("set", index)]] <- combo
    index <- index + 1
  }
}

results <- list()

for (i in seq_along(predictor_sets)) {
  pred <- predictor_sets[[i]]
  
  dataprep.out <- dataprep(
    foo = full_data,
    dependent = "total_value",
    predictors = pred,
    unit.variable = "reporterISONum",
    unit.names.variable = "reporterISO",
    time.variable = "refYear",
    treatment.identifier = treated_country_ID,
    controls.identifier = setdiff(unique(full_data$reporterISO), treated_country),
    time.predictors.prior = 1997:2017,
    time.optimize.ssr = 1997:2017,
    time.plot = 1997:2021
  )
  
  synth.out <- synth(dataprep.out)
  
  actual <- dataprep.out$Y1plot
  synthetic <- dataprep.out$Y0plot %*% synth.out$solution.w
  gap <- actual - synthetic
  rmspe_pre <- sqrt(mean(gap[dataprep.out$tag$time.plot <= 2017]^2))
  
  results[[i]] <- list(
    predictors = pred,
    rmspe_pre = rmspe_pre,
    synth.out = synth.out,
    dataprep.out = dataprep.out,
    weights = synth.out$solution.w,
    synthetic = synthetic,
    actual = actual,
    gap = gap,
    time.plot = dataprep.out$tag$time.plot
  )
}

plot_synth_result <- function(result, title = "Synthetic Control Result") {
  plot(result$time.plot, result$actual, type = "l", col = "black", lwd = 2,
       ylim = range(c(result$actual, result$synthetic)), ylab = "Outcome", xlab = "Year", main = title)
  lines(result$time.plot, result$synthetic, col = "red", lwd = 2)
  abline(v = 2018, lty = 2)
  legend("topleft", legend = c("Actual", "Synthetic"), col = c("black", "red"), lty = 1, lwd = 2)
}

plot_all_gaps <- function(results) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(xpd = FALSE, mar = c(5, 4, 4, 12))
  x_range <- range(results[[1]]$time.plot)
  y_range <- range(sapply(results, function(x) x$gap))
  plot(NULL, xlim = x_range, ylim = y_range,
       xlab = "Year", ylab = "Gap (Actual - Synthetic)",
       main = "Gaps Over Time")
  for (i in seq_along(results)) {
    lines(results[[i]]$time.plot, results[[i]]$gap, col = i)
  }
  segments(x0 = x_range[1], x1 = x_range[2], y0 = 0, y1 = 0, lty = 2)
  segments(x0 = 2018, x1 = 2018, y0 = y_range[1], y1 = y_range[2], lty = 2)
  par(xpd = TRUE)
  legend("topright",
         inset = c(-0.35, 0),
         legend = paste0("Model ", seq_along(results)),
         col = seq_along(results),
         lty = 1,
         cex = 0.7,
         ncol = 2,
         bty = "n")
}

results_sorted <- results[order(sapply(results, function(x) x$rmspe_pre))]
plot_synth_result(results_sorted[[1]], title = "Best Model by RMSPE")
plot_all_gaps(results_sorted)

# --------------------- Run Placebo Tests For Best Model ---------------------

best_model <- results_sorted[[1]]
best_predictors <- best_model$predictors
control_units <- setdiff(unique(full_data$reporterISO), treated_country)

placebo_results <- list()

for (i in seq_along(control_units)) {
  placebo_id <- control_units[i]
  
  dataprep.out <- dataprep(
    foo = full_data,
    dependent = "total_value",
    predictors = best_predictors,
    unit.variable = "reporterISONum",
    unit.names.variable = "reporterISO",
    time.variable = "refYear",
    treatment.identifier = full_data$reporterISONum[full_data$reporterISO == placebo_id][1],
    controls.identifier = setdiff(unique(full_data$reporterISO), placebo_id),
    time.predictors.prior = 1997:2017,
    time.optimize.ssr = 1997:2017,
    time.plot = 1997:2021
  )
  
  synth.out <- tryCatch({
    synth(dataprep.out)
  }, error = function(e) {
    return(NULL)
  })
  
  if (!is.null(synth.out)) {
    actual <- dataprep.out$Y1plot
    synthetic <- dataprep.out$Y0plot %*% synth.out$solution.w
    gap <- actual - synthetic
    
    placebo_results[[placebo_id]] <- list(
      id = placebo_id,
      synth.out = synth.out,
      dataprep.out = dataprep.out,
      weights = synth.out$solution.w,
      synthetic = synthetic,
      actual = actual,
      gap = gap,
      time.plot = dataprep.out$tag$time.plot
    )
  }
}

plot_placebo_gaps <- function(treated_result, placebo_results) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(xpd = FALSE, mar = c(5, 4, 4, 8))
  y_range <- range(c(treated_result$gap, unlist(lapply(placebo_results, function(x) x$gap))))
  plot(NULL, xlim = range(treated_result$time.plot), ylim = y_range,
       xlab = "Year", ylab = "Gap (Actual - Synthetic)",
       main = "Placebo Test: Gaps Over Time")
  for (res in placebo_results) {
    lines(res$time.plot, res$gap, col = "lightgray", lty = 1)
  }
  lines(treated_result$time.plot, treated_result$gap, col = "red", lwd = 2)
  abline(v = 2018, lty = 2)
  abline(h = 0, lty = 2)
  par(xpd = TRUE)
  legend("topright", inset = c(-0.25, 0),
         legend = c("Treated Unit", "Placebo Units"),
         col = c("red", "lightgray"), lty = 1, lwd = c(2, 1), bty = "n")
}

plot_placebo_gaps(best_model, placebo_results)

compute_rmspe_ratio <- function(result, treat_year = 2018) {
  pre_idx <- which(result$time.plot < treat_year)
  post_idx <- which(result$time.plot >= treat_year)
  rmspe_pre <- sqrt(mean(result$gap[pre_idx]^2))
  rmspe_post <- sqrt(mean(result$gap[post_idx]^2))
  return(rmspe_post / rmspe_pre)
}

treated_ratio <- compute_rmspe_ratio(best_model)
placebo_ratios <- sapply(placebo_results, compute_rmspe_ratio)

hist(placebo_ratios, breaks = 20, col = "lightgray",
     main = "Distribution of RMSPE Ratios (Post/Pre)",
     xlab = "RMSPE Ratio", xlim = range(c(placebo_ratios, treated_ratio)))
abline(v = treated_ratio, col = "red", lwd = 2)
legend("topright", legend = c("Treated Unit"), col = "red", lwd = 2)

# --------------------- Run A More Flexible Synth Model ---------------------

full_data <- full_data %>%
  mutate(treated = ifelse(reporterISO == "USA" & refYear >= 2018, 1, 0))

out <- gsynth(
  total_value ~ treated,
  data = full_data,
  index = c("reporterISONum", "refYear"),
  force = "two-way",
  CV = TRUE,
  r = c(0, 5),
  se = TRUE,
  inference = "parametric"
)

plot(out)
