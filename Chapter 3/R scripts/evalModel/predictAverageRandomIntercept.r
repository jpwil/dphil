# this function calculates the linear predictor for a particular dataset using the pooled model
# with the study-average intercept. The linear predictor is then scaled by a global shrinkage factor
# and displaced by the adjusted intercept term if needed.

# model must be passed as mipo class - mice package must be loaded
predictAverageRandomIntercept <- function(data, model, sf = 1, int_adj = 0) {
  pm <- summary(model) %>% mutate(term = as.character(term), term = if_else(term == "(Intercept)", "INT", term))
  data_predict <- data[, as.character(pm$term)]
  lp <- numeric(nrow(data))
  for (i in seq_along(lp)) {
    lp[i] <- sum(as.vector(unlist(data_predict[i, ])) * pm$estimate) * sf + int_adj # sf is a shrinkage factor, int_adj is the intercept adjustment
  }
  lp # return value
}
