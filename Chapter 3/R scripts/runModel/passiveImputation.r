# These functions are for passive imputation of BMI and BMI-for-age-z-scores
# These functions are defined here to avoid excessive 'code in string form' in the method argument (character vector) passed to mice()

# Variables: VS_BL_WEIGHTs and are sourced from the current mice iteration
# BMIs here represents scaled BMI (used for modelling)
passivelyImputeBMIs <- function(data_scale, VS_BL_WEIGHTs = VS_BL_WEIGHTs, VS_BL_HEIGHTs = VS_BL_HEIGHTs, DM_AGEs = DM_AGEs) {
  DM_AGE <- convertToOriginal(DM_AGEs, "DM_AGE", data_scale = data_scale)
  VS_BL_WEIGHT <- convertToOriginal(VS_BL_WEIGHTs, "VS_BL_WEIGHT", data_scale = data_scale)
  VS_BL_HEIGHT <- convertToOriginal(VS_BL_HEIGHTs, "VS_BL_HEIGHT", data_scale = data_scale)

  VS_BL_WEIGHT <- ifelse(DM_AGE <= 5 & VS_BL_WEIGHT < 0.9, 0.9, VS_BL_WEIGHT)
  VS_BL_WEIGHT <- ifelse(DM_AGE <= 5 & VS_BL_WEIGHT > 58, 58, VS_BL_WEIGHT)

  VS_BL_HEIGHT <- ifelse(DM_AGE <= 5 & VS_BL_HEIGHT < 45, 45, VS_BL_HEIGHT)
  VS_BL_HEIGHT <- ifelse(DM_AGE <= 5 & VS_BL_HEIGHT > 120, 120, VS_BL_HEIGHT)

  VS_BMI <- VS_BL_WEIGHT / ((VS_BL_HEIGHT) / 100)^2
  VS_BMIs <- (VS_BMI - data_scale[data_scale$var == "VS_BMI", "means"]) / data_scale[data_scale$var == "VS_BMI", "sd"]

  VS_BMIs # return value
}

# Variables: DM_AGEs, VS_BL_WEIGHTs, VS_BL_HEIGHTs and DM_SEX are sourced from the current mice iteration
# This function calculates WFH z-score as well (function was named before expanding malnutrition definition to < 5)
passivelyImputeBMIzScore <- function(data_scale, DM_SEX = DM_SEX, DM_AGEs = DM_AGEs, VS_BL_WEIGHTs = VS_BL_WEIGHTs, VS_BL_HEIGHTs = VS_BL_HEIGHTs) {
  DM_AGE <- convertToOriginal(DM_AGEs, "DM_AGE", data_scale = data_scale)
  VS_BL_WEIGHT <- convertToOriginal(VS_BL_WEIGHTs, "VS_BL_WEIGHT", data_scale = data_scale)
  VS_BL_HEIGHT <- convertToOriginal(VS_BL_HEIGHTs, "VS_BL_HEIGHT", data_scale = data_scale)

  # these 'squeeze' adjustments are required to prevent the z-scores returning NA values (for under 5 children have imputed weight and height values that are )
  VS_BL_WEIGHT <- ifelse(DM_AGE <= 5 & VS_BL_WEIGHT < 0.9, 0.9, VS_BL_WEIGHT)
  VS_BL_WEIGHT <- ifelse(DM_AGE <= 5 & VS_BL_WEIGHT > 58, 58, VS_BL_WEIGHT)

  VS_BL_HEIGHT <- ifelse(DM_AGE <= 5 & VS_BL_HEIGHT < 45, 45, VS_BL_HEIGHT)
  VS_BL_HEIGHT <- ifelse(DM_AGE <= 5 & VS_BL_HEIGHT > 120, 120, VS_BL_HEIGHT)

  # use anthro::anthro_zscores() to calculate the BMI-for-age-z-score in under 5s
  # use anthro::anthro_zscores() to calculate the weight-for-height-z-score in under 5s
  VS_BMI_Z_u5 <- anthro::anthro_zscores(sex = 2 - DM_SEX, age = if_else(DM_AGE == 5, 59, DM_AGE * 12), is_age_in_month = TRUE, weight = VS_BL_WEIGHT, lenhei = VS_BL_HEIGHT)$zbmi
  VS_WFH_Z_u5 <- anthro::anthro_zscores(sex = 2 - DM_SEX, weight = VS_BL_WEIGHT, lenhei = VS_BL_HEIGHT)$zwfl

  # use anthroplus::anthroplus_zscores() to calculate the BMI-for-age-z-score in over 5s (and deal with edge case to avoid code crashing)
  VS_BMI_Z_o5 <- anthroplus::anthroplus_zscores(sex = 2 - DM_SEX, age = if_else(DM_AGE * 12 > 228, 228, if_else(DM_AGE >= 5 & DM_AGE < 5.1, 5.1 * 12, DM_AGE * 12)), weight_in_kg = VS_BL_WEIGHT, height_in_cm = VS_BL_HEIGHT)$zbfa

  temp <- data.frame(DM_SEX = DM_SEX, VS_BL_WEIGHT = VS_BL_WEIGHT, VS_BL_HEIGHT = VS_BL_HEIGHT, DM_AGE = DM_AGE, VS_BMI_Z_u5 = VS_BMI_Z_u5, VS_BMI_Z_o5 = VS_BMI_Z_o5, VS_WFH_Z_u5 = VS_WFH_Z_u5)
  temp <- temp %>% dplyr::mutate(ZZ_BMI_Z = if_else(DM_AGE * 12 <= 60, VS_WFH_Z_u5, VS_BMI_Z_o5))
  # saveRDS(temp, "data/temp_debug4.rds") # this was for debugging purposes

  temp$ZZ_BMI_Z # return value
}

# height must be between 45 and 120cm
# weight must be between 0.9 and 58kg
