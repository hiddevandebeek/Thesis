##### LIBRARIES
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
library(MASS)
library(tidyverse)
library(parallel)
library(pROC)
library(caTools)
source("scripts/shared/functions.R")
source("scripts/hpc/beta_10_predictors.R")

##### RUN CODE
# 10 variables, covariance
### Initialize variables
set.seed(28)
p      <- 1                           # proportion of correlated predictors
npred  <- 10                           # number of predictors
z      <- 0.2                         # z 
n      <- 10000000                    # Number of samples in the dataset
beta_values_10predictors_covariance <- read.csv("data/datasets/beta_values/10predictors_covariance.csv")

mu0 <- c(rep(1,npred))  
sigma0 <- scenario()
data_experiment <- mvrnorm(n, mu = mu0, Sigma = sigma0) 
colnames(data_experiment) <- c(paste0("predictor", 1:(ncol(data_experiment))))
saveRDS(data_experiment, "data/datasets/full_data/10pred/covar/data_predictors.rds")
data_experiment <- as.data.frame(data_experiment)

# Part 1
## 1 end predictor: normal
b <- beta_values_10predictors_covariance$AUC_0.6[1]
L <-  calculate_linear_predictor10(b, data_experiment, "1normal")
L <- - mean(L) + L
y_0.6 <- ifelse(runif(n) < plogis(L), 1, 0)

b <- beta_values_10predictors_covariance$AUC_0.75[1]
L <-  calculate_linear_predictor10(b, data_experiment, "1normal")
L <- - mean(L) + L
y_0.75 <- ifelse(runif(n) < plogis(L), 1, 0)

b <- beta_values_10predictors_covariance$AUC_0.9[1]
L <-  calculate_linear_predictor10(b, data_experiment, "1normal")
L <- - mean(L) + L
y_0.9 <- ifelse(runif(n) < plogis(L), 1, 0)
y <- cbind(y_0.6, y_0.75, y_0.9)
saveRDS(y, "data/datasets/full_data/10pred/covar/1normal.rds")

## 1 end predictor: interaction
b <- beta_values_10predictors_covariance$AUC_0.6[2]
L <-  calculate_linear_predictor10(b, data_experiment, "1interaction")
L <- - mean(L) + L
y_0.6 <- ifelse(runif(n) < plogis(L), 1, 0)

b <- beta_values_10predictors_covariance$AUC_0.75[2]
L <-  calculate_linear_predictor10(b, data_experiment, "1interaction")
L <- - mean(L) + L
y_0.75 <- ifelse(runif(n) < plogis(L), 1, 0)

b <- beta_values_10predictors_covariance$AUC_0.9[2]
L <-  calculate_linear_predictor10(b, data_experiment, "1interaction")
L <- - mean(L) + L
y_0.9 <- ifelse(runif(n) < plogis(L), 1, 0)
y <- cbind(y_0.6, y_0.75, y_0.9)
saveRDS(y, "data/datasets/full_data/10pred/covar/1interaction.rds")

## 2 end predictor: normal + normal
b <- beta_values_10predictors_covariance$AUC_0.6[3]
L <-  calculate_linear_predictor10(b, data_experiment, "2normal")
L <- - mean(L) + L
y_0.6 <- ifelse(runif(n) < plogis(L), 1, 0)

b <- beta_values_10predictors_covariance$AUC_0.75[3]
L <-  calculate_linear_predictor10(b, data_experiment, "2normal")
L <- - mean(L) + L
y_0.75 <- ifelse(runif(n) < plogis(L), 1, 0)

b <- beta_values_10predictors_covariance$AUC_0.9[3]
L <-  calculate_linear_predictor10(b, data_experiment, "2normal")
L <- - mean(L) + L
y_0.9 <- ifelse(runif(n) < plogis(L), 1, 0)
y <- cbind(y_0.6, y_0.75, y_0.9)
saveRDS(y, "data/datasets/full_data/10pred/covar/2normal.rds")

## 2 end predictor: normal + interaction
b <- beta_values_10predictors_covariance$AUC_0.6[4]
L <-  calculate_linear_predictor10(b, data_experiment, "2normalinteraction")
L <- - mean(L) + L
y_0.6 <- ifelse(runif(n) < plogis(L), 1, 0)

b <- beta_values_10predictors_covariance$AUC_0.75[4]
L <-  calculate_linear_predictor10(b, data_experiment, "2normalinteraction")
L <- - mean(L) + L
y_0.75 <- ifelse(runif(n) < plogis(L), 1, 0)

b <- beta_values_10predictors_covariance$AUC_0.9[4]
L <-  calculate_linear_predictor10(b, data_experiment, "2normalinteraction")
L <- - mean(L) + L
y_0.9 <- ifelse(runif(n) < plogis(L), 1, 0)
y <- cbind(y_0.6, y_0.75, y_0.9)
saveRDS(y, "data/datasets/full_data/10pred/covar/2normalinteraction.rds")

## 2 end predictor: interaction + interaction
b <- beta_values_10predictors_covariance$AUC_0.6[5]
L <-  calculate_linear_predictor10(b, data_experiment, "2interaction")
L <- - mean(L) + L
y_0.6 <- ifelse(runif(n) < plogis(L), 1, 0)

b <- beta_values_10predictors_covariance$AUC_0.75[5]
L <-  calculate_linear_predictor10(b, data_experiment, "2interaction")
L <- - mean(L) + L
y_0.75 <- ifelse(runif(n) < plogis(L), 1, 0)

b <- beta_values_10predictors_covariance$AUC_0.9[5]
L <-  calculate_linear_predictor10(b, data_experiment, "2interaction")
L <- - mean(L) + L
y_0.9 <- ifelse(runif(n) < plogis(L), 1, 0)
y <- cbind(y_0.6, y_0.75, y_0.9)
saveRDS(y, "data/datasets/full_data/10pred/covar/2interaction.rds")

# Part 2: ANTI
## Covariance
## 1 end predictor: normal anti
b <- beta_values_10predictors_covariance$AUC_0.6[1]
L <-  calculate_linear_predictor10(b, data_experiment, "sparse")
L <- - mean(L) + L
y_0.6 <- ifelse(runif(n) < plogis(L), 1, 0)

b <- beta_values_10predictors_covariance$AUC_0.75[1]
L <-  calculate_linear_predictor10(b, data_experiment, "sparse")
L <- - mean(L) + L
y_0.75 <- ifelse(runif(n) < plogis(L), 1, 0)

b <- beta_values_10predictors_covariance$AUC_0.9[1]
L <-  calculate_linear_predictor10(b, data_experiment, "sparse")
L <- - mean(L) + L
y_0.9 <- ifelse(runif(n) < plogis(L), 1, 0)
y <- cbind(y_0.6, y_0.75, y_0.9)
saveRDS(y, "data/datasets/full_data/10pred/covar/1normal_anti.rds")

## 1 end predictor: interaction anti
b <- beta_values_10predictors_covariance$AUC_0.6[2]
L <-  calculate_linear_predictor10(b, data_experiment, "sparse")
L <- - mean(L) + L
y_0.6 <- ifelse(runif(n) < plogis(L), 1, 0)

b <- beta_values_10predictors_covariance$AUC_0.75[2]
L <-  calculate_linear_predictor10(b, data_experiment, "sparse")
L <- - mean(L) + L
y_0.75 <- ifelse(runif(n) < plogis(L), 1, 0)

b <- beta_values_10predictors_covariance$AUC_0.9[2]
L <-  calculate_linear_predictor10(b, data_experiment, "sparse")
L <- - mean(L) + L
y_0.9 <- ifelse(runif(n) < plogis(L), 1, 0)
y <- cbind(y_0.6, y_0.75, y_0.9)
saveRDS(y, "data/datasets/full_data/10pred/covar/1interaction_anti.rds")

## 2 end predictor: normal + normal anti
b <- beta_values_10predictors_covariance$AUC_0.6[3]
L <-  calculate_linear_predictor10(b, data_experiment, "minimal")
L <- - mean(L) + L
y_0.6 <- ifelse(runif(n) < plogis(L), 1, 0)

b <- beta_values_10predictors_covariance$AUC_0.75[3]
L <-  calculate_linear_predictor10(b, data_experiment, "minimal")
L <- - mean(L) + L
y_0.75 <- ifelse(runif(n) < plogis(L), 1, 0)

b <- beta_values_10predictors_covariance$AUC_0.9[3]
L <-  calculate_linear_predictor10(b, data_experiment, "minimal")
L <- - mean(L) + L
y_0.9 <- ifelse(runif(n) < plogis(L), 1, 0)
y <- cbind(y_0.6, y_0.75, y_0.9)
saveRDS(y, "data/datasets/full_data/10pred/covar/2normal_anti.rds")

## 2 end predictor: normal + interaction anti
b <- beta_values_10predictors_covariance$AUC_0.6[4]
L <-  calculate_linear_predictor10(b, data_experiment, "minimal")
L <- - mean(L) + L
y_0.6 <- ifelse(runif(n) < plogis(L), 1, 0)

b <- beta_values_10predictors_covariance$AUC_0.75[4]
L <-  calculate_linear_predictor10(b, data_experiment, "minimal")
L <- - mean(L) + L
y_0.75 <- ifelse(runif(n) < plogis(L), 1, 0)

b <- beta_values_10predictors_covariance$AUC_0.9[4]
L <-  calculate_linear_predictor10(b, data_experiment, "minimal")
L <- - mean(L) + L
y_0.9 <- ifelse(runif(n) < plogis(L), 1, 0)
y <- cbind(y_0.6, y_0.75, y_0.9)
saveRDS(y, "data/datasets/full_data/10pred/covar/2normalinteraction_anti.rds")

## 2 end predictor: interaction + interaction anti
b <- beta_values_10predictors_covariance$AUC_0.6[5]
L <-  calculate_linear_predictor10(b, data_experiment, "minimal")
L <- - mean(L) + L
y_0.6 <- ifelse(runif(n) < plogis(L), 1, 0)

b <- beta_values_10predictors_covariance$AUC_0.75[5]
L <-  calculate_linear_predictor10(b, data_experiment, "minimal")
L <- - mean(L) + L
y_0.75 <- ifelse(runif(n) < plogis(L), 1, 0)

b <- beta_values_10predictors_covariance$AUC_0.9[5]
L <-  calculate_linear_predictor10(b, data_experiment, "minimal")
L <- - mean(L) + L
y_0.9 <- ifelse(runif(n) < plogis(L), 1, 0)
y <- cbind(y_0.6, y_0.75, y_0.9)
saveRDS(y, "data/datasets/full_data/10pred/covar/2interaction_anti.rds")