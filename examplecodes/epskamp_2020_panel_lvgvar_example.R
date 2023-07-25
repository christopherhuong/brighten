# Required packages (install with install.packages):
library("qgraph")
library("dplyr")
library("devtools")
library("psych")
library("graphicalVAR")

# Install psychonetrics:
# devtools::install_github("sachaepskamp/psychonetrics")

# Load psychonetrics:
library("psychonetrics")

# Estimation algorithm:
alpha <- 0.01
adjust <- "none"
searchstrategy <- "modelsearch" # Can also be "none" or "stepup"

# Load LISS summary statistics and required objects:
load("LISSSummaryStatistics.RData")

# Form model:
model <- dlvm1(
  covs = covMat, # Covariance matrix to use
  nobs = nobs, # Number of observations to use
  vars = varMat, # The design matrix, with a row indicating a variable and a column a wave of measurements. Note that NA indicates missing variables
  lambda = lambda, # Measurement model
  within_latent = "ggm", # Model within-subject contemporaneous as GGM
  within_residual = "chol", # Model within-subject residuals as Cholesky
  between_latent = "ggm", # Model between-subject latent as GGM
  between_residual = "chol", # Model between-subject residuals as Cholesky
  latents = latents # Names of the latent variables to use
  )

# Run model:
model <- model %>% runmodel

# There is a numeric issue with the between-subjects residual of "feelgood_myself":
model %>% parameters

# Fix the problematic parameter to zero:
model <- model %>% 
  fixpar("lowertri_epsilon_between","feelgood_myself","feelgood_myself")

# Run again:
model <- model %>% runmodel

# Check fit:
model %>% print
model %>% fit

# Estimation algorithm (prune step):
model_prune <- model %>% 
  runmodel %>% 
  prune(alpha = alpha, adjust = adjust, recursive = FALSE)      

# Search strategy
if (searchstrategy == "stepup"){
  model_prune <- model_prune %>%  stepup(alpha = alpha, criterion = "bic")
} else if (searchstrategy == "modelsearch"){
  model_prune <- model_prune %>%  modelsearch(prunealpha = alpha, addalpha = alpha)
}

# Compare:
comp <- compare(
  original = model,
  pruned = model_prune
  )

# Differences:
comp$AIC[1] - comp$AIC[2]
comp$BIC[1] - comp$BIC[2]

# Check fit:
model_prune %>% print
model_prune %>% fit

# Save model:
saveRDS(model_prune, file="panelResults.RDS")
