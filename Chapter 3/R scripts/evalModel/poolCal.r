###############################
## pool performance measures ##
###############################

safePoolCal <- function(prog) {
  warnings_list <- character() # initialise empty vector to store warnings
  result <- NULL
  time_start <- Sys.time()

  tryCatch(
    {
      withCallingHandlers(
        {
          result <- poolCal(prog)
          time <- list(time_start = time_start, time_end = Sys.time())
          prog$poolCal <- list(result = result, warnings = warnings_list, error = NULL, time = time) # Return the result if successful
          prog # return value
        },
        warning = function(w) {
          # Capture warnings in the warnings_list vector
          cat("processing warning now")
          warnings_list <<- c(warnings_list, w$message)
          invokeRestart("muffleWarning") # Prevent the warning from being displayed
        }
      )
    },
    error = function(e) {
      # Handle the error
      message("An error occurred: ", e$message)
      result <- NULL # Set result to NULL in case of error
      time <- list(time_start = time_start, time_end = Sys.time())
      prog$poolCal <- list(result = result, warnings = warnings_list, error = e$message, time = time)
      prog # return value
    },
    finally = {}
  )
}

poolCal <- function(prog) {
  alpha <- 0.05 # for confidence intervals

  calITL <- prog$evalCal$result$calITL
  calSlope <- prog$evalCal$result$calSlope

  poolCITL <- array(numeric(), dim = c(5))
  poolSlope <- array(numeric(), dim = c(5))

  dimnames(poolCITL) <- list(c("estimate", "variance", "df", "ci_l", "ci_u"))
  dimnames(poolSlope) <- list(c("estimate", "variance", "df", "ci_l", "ci_u"))

  pool_temp <- mice::pool.scalar(
    Q = calITL[, 1],
    U = calITL[, 2]^2,
    n = Inf
  )

  poolCITL[1] <- pool_temp$qbar
  poolCITL[2] <- pool_temp$t
  poolCITL[3] <- pool_temp$df
  poolCITL[4] <- pool_temp$qbar - qt(1 - alpha / 2, pool_temp$df) * sqrt(pool_temp$t)
  poolCITL[5] <- pool_temp$qbar + qt(1 - alpha / 2, pool_temp$df) * sqrt(pool_temp$t)

  pool_temp <- mice::pool.scalar(
    Q = calSlope[, 1],
    U = calSlope[, 2]^2,
    n = Inf
  )

  poolSlope[1] <- pool_temp$qbar
  poolSlope[2] <- pool_temp$t
  poolSlope[3] <- pool_temp$df
  poolSlope[4] <- pool_temp$qbar - qt(1 - alpha / 2, pool_temp$df) * sqrt(pool_temp$t)
  poolSlope[5] <- pool_temp$qbar + qt(1 - alpha / 2, pool_temp$df) * sqrt(pool_temp$t)

  list(poolCITL = poolCITL, poolSlope = poolSlope) # return value
}
