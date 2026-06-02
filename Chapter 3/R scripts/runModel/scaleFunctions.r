# The logistic regression models are performed on scaled variables (centred and divided by standard deviation, some also on the natural logarithm scale)

# These functions convert between scaled variables and unscaled variables

# Scaling data are described in the data_scale dataframe (sourced from data/ads_impute_scale.rds, and created during data cleaning)

convertToOriginal <- function(vec, var, data_scale = data_scale) {
  allowed_values <- data_scale$var
  if (!(var %in% allowed_values)) {
    stop("Argument 'var' must be one of: ", paste(allowed_values, collapse = ", "))
  }

  log_scale <- data_scale[data_scale$var == var, "log_scale"]
  sd_val <- data_scale[data_scale$var == var, "sd"]
  mean_val <- data_scale[data_scale$var == var, "means"]

  if (log_scale) {
    original <- exp(vec * sd_val + mean_val)
  } else {
    original <- vec * sd_val + mean_val
  }

  original
}

convertToScaled <- function(vec, var, data_scale = data_scale) {
  allowed_values <- data_scale$var
  if (!(var %in% allowed_values)) {
    stop("Argument 'var' must be one of: ", paste(allowed_values, collapse = ", "))
  }

  log_scale <- data_scale[data_scale$var == var, "log_scale"]
  sd_val <- data_scale[data_scale$var == var, "sd"]
  mean_val <- data_scale[data_scale$var == var, "means"]

  if (log_scale) {
    scaled <- (log(vec) - mean_val) / sd_val
  } else {
    scaled <- (vec - mean_val) / sd_val
  }

  scaled
}
