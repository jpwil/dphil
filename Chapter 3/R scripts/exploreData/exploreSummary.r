# explore model summary files

library(tidyverse)
library(mice)
library(lme4)

files <- dir("results-share")
files[[36]]

sum <- readRDS(paste0("results-share/", files[36]))
length(sum)

df <- do.call(
  rbind,
  lapply(sum, function(x) {
    data.frame(
      terms = paste0(x$finalVariables$term, collapse = ", "),
      seed = if (!is.null(x$call$seed)) x$call$seed else NA,
      cindex = x$evalCI$result$conditionalAP[[4]],
      stringsAsFactors = FALSE
    )
  })
)

sum[[2]]$modelPooled
sum[[2]]$finalVariables

df %>% count(terms)
df %>% count(terms, cindex)

df %>% ggplot() +
  geom_histogram(
    aes(x = cindex, fill = terms),
    position = "dodge"
  ) +
  theme_minimal()
