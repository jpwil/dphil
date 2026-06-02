# this script defines a function that develops and evaluates a VL prognostic model with relapse as the outcome

# See Wynants 2018 et al. Does ignoring clustering in multicenter data influence the performance of prediction models? A simulation study.
# Statistical Methods in Medical Research 2018, Vol. 27(6) 1723–1736

# PREDICTIONS #

# The mixed eﬀects model yields three types of predictions:

# 1) conditional predictions
# 2) predictions for an individual in a center with an average random intercept
# 3) marginal predictions (similar to prediction obtained from fitting a standard logistic regression model)

# MODEL PERFORMAMCE #

# The choice of performance measure in multicenter validation data should depend on the use of the prediction model and the research question.
# There a two key aspects of predictive performance:

# DISCRIMINATION

# measured with the C-index for binary outcomes

# The C-index is the probability that for a randomly selected pair of an event and a non-event, the event has a higher predicted probability.
# It is calculated by comparing the number of pairs that are concordance, with the total number of pairs. There are two types of C-index:

# 1) Standard C-index: all pairs are compared: including pairs within the same cluster and pairs across clusters
# 2) Within-centre C-index: Only pairs of events and non-events within the same cluster are compared.
#    It is the average centre-speciic C-index, weighted by the number of pairs of events and nonevent per centre.

# NB1:  The cluster-specific C-index is undefined in clusters with no events.
# NB2:  For each imputated dataset, C-index estimates are evaluated by bootstrapping.
#       For each imputated dataset, the C-index estimates are ordered and pooled.
#       Overall quantiles of pooled data are used for the overall estimates (see MI Boot (pooled sample) from Schomaker 2018 et al).

# CALIBRATION

# measured with calibration slope, calibration-in-the-large (calibration intercept), overall observed/expected number

evalModel <- function(prog, boot = FALSE, data_eval = NULL) {
  ## load helper functions
  source("R/evalModel/evalCIndex.r") ## PERFORMANCE: C-INDEX (INCLUDING BOOTSTRAPPING)
  source("R/evalModel/evalCal.r") ## PERFORMANCE: CALIBRATION
  source("R/evalModel/predictAverageRandomIntercept.r") ## EVALUATE LINEAR PREDICTOR FOR AVERAGE RANDOM INTERCEPT (CALLED IN BOTH evalCIndex and MI_CAL)

  if (boot && is.null(data_eval)) {
    stop("When boot is TRUE, data_eval must be specified")
  }
  if (!boot && !is.null(data_eval)) {
    stop("When boot is FALSE, data_eval should not be specified")
  }
  if (boot && class(data_eval) != "mids") {
    stop("data_eval must be 'mids' class")
  }

  ## PERFORMANCE

  # The boot argument determines whether the model is evaluated in the same data used for development (apparent validation) or in different data
  # When boot = FALSE: the model is only evaluated in the data used for development
  # When boot = TRUE: the model is evaluated in the both data used for development (i.e. bootstrapped) AND the original (i.e. non-bootstrapped) data
  #                   this allows estimation of optimism

  # Evaluate C-index
  prog <- safeEvalCIout(prog = prog, boot = boot, data_eval = data_eval)

  # Evaluate calibration
  prog <- safeEvalCal(prog = prog, boot = boot, data_eval = data_eval)

  prog # return value
}

# source("R/evalModel/evalCal.r")
# source("R/evalModel/evalCIndex_new.r")
# source("R/evalModel/predictAverageRandomIntercept.r")

# df <- readRDS("results/202512141236_WITH_PD/originalModel_4.rds")
# data_eval <- df$mice$result_grp
# boot <- TRUE
# prog <- readRDS("results/202512141721_WITH_PD_BOOT_TEST/bootModel_init1.rds")

# prog1 <- safeEvalCal(prog = prog, boot = FALSE, data_eval = NULL)
# prog2 <- safeEvalCal(prog = prog, boot = boot, data_eval = data_eval)

# prog1 %>% names()
# prog1$evalCI_new$result %>% names()
# prog1$evalCI_new$result$RE
# prog1$evalCI_new$result$FE_cond

# prog2 %>% names()
# prog2$evalCI_new$result %>% names()
# prog2$evalCI_new$result$RE
# prog2$evalCI_new$result$FE_cond
