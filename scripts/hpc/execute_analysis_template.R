##### LIBRARIES
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
library(parallel)
library(tidyverse)
library(rms)
library(MASS)
library(caTools)
source("scripts/shared/functions.R")

##### SPECIFY PARAMETERS
scenario <- "1normal"
pred <- paste0("5pred")

##### RUN CODE
taskid <- Sys.getenv('SLURM_ARRAY_TASK_ID')
data <- readRDS(paste0("data/datasets/full_data/", pred, "/covar/data_predictors.rds"))
y <- readRDS(paste0("data/datasets/full_data/", pred, "/covar/", paste0(scenario), ".rds"))
data_temp <- as.data.frame(cbind(data, y))

if (taskid == 1){
  test <- compare_models_05_mclapply(data = data_temp, scenario = scenario, true_auc = 0.6, perc_ones = 0.5, cl = 10)
  saveRDS(test, file = paste0("data/analysis/", pred, "/covar/", paste0(scenario), "/auc06_ev05.rds"))
}
if (taskid == 2){
  test <- compare_models_05_mclapply(data = data_temp, scenario = scenario, true_auc = 0.6, perc_ones = 0.2, cl = 10)
  saveRDS(test, file = paste0("data/analysis/", pred, "/covar/", paste0(scenario), "/auc06_ev02.rds"))
}
if (taskid == 3){
  test <- compare_models_05_mclapply(data = data_temp, scenario = scenario, true_auc = 0.6, perc_ones = 0.05, cl = 4)
  saveRDS(test, file = paste0("data/analysis/", pred, "/covar/", paste0(scenario), "/auc06_ev005.rds"))
}
if (taskid == 4){
  test <- compare_models_05_mclapply(data = data_temp, scenario = scenario, true_auc = 0.75, perc_ones = 0.5, cl = 10)
  saveRDS(test, file = paste0("data/analysis/", pred, "/covar/", paste0(scenario), "/auc075_ev05.rds"))
}
if (taskid == 5){
  test <- compare_models_05_mclapply(data = data_temp, scenario = scenario, true_auc = 0.75, perc_ones = 0.2, cl = 10)
  saveRDS(test, file = paste0("data/analysis/", pred, "/covar/", paste0(scenario), "/auc075_ev02.rds"))
}
if (taskid == 6){
  test <- compare_models_05_mclapply(data = data_temp, scenario = scenario, true_auc = 0.75, perc_ones = 0.05, cl = 10)
  saveRDS(test, file = paste0("data/analysis/", pred, "/covar/", paste0(scenario), "/auc075_ev005.rds"))
}
if (taskid == 7){
  test <- compare_models_05_mclapply(data = data_temp, scenario = scenario, true_auc = 0.9, perc_ones = 0.5, cl = 10)
  saveRDS(test, file = paste0("data/analysis/", pred, "/covar/", paste0(scenario), "/auc09_ev05.rds"))
}
if (taskid == 8){
  test <- compare_models_05_mclapply(data = data_temp, scenario = scenario, true_auc = 0.9, perc_ones = 0.2, cl = 10)
  saveRDS(test, file = paste0("data/analysis/", pred, "/covar/", paste0(scenario), "/auc09_ev02.rds"))
}
if (taskid == 9){
  test <- compare_models_05_mclapply(data = data_temp, scenario = scenario, true_auc = 0.9, perc_ones = 0.05, cl = 10)
  saveRDS(test, file = paste0("data/analysis/", pred, "/covar/", paste0(scenario), "/auc09_ev005.rds"))
}

y <- readRDS(paste0("data/datasets/full_data/", pred, "/covar/", paste0(scenario), "_anti.rds"))
data_temp <- as.data.frame(cbind(data, y))

if (taskid == 10){
  test <- compare_models_05_mclapply(data = data_temp, scenario = scenario, true_auc = 0.6, perc_ones = 0.5, cl = 10, anti = T)
  saveRDS(test, file = paste0("data/analysis/", pred, "/covar/", paste0(scenario), "/auc06_ev05_anti.rds"))
}
if (taskid == 11){
  test <- compare_models_05_mclapply(data = data_temp, scenario = scenario, true_auc = 0.6, perc_ones = 0.2, cl = 10, anti = T)
  saveRDS(test, file = paste0("data/analysis/", pred, "/covar/", paste0(scenario), "/auc06_ev02_anti.rds"))
}
if (taskid == 12){
  test <- compare_models_05_mclapply(data = data_temp, scenario = scenario, true_auc = 0.6, perc_ones = 0.05, cl = 10, anti = T)
  saveRDS(test, file = paste0("data/analysis/", pred, "/covar/", paste0(scenario), "/auc06_ev005_anti.rds"))
}
if (taskid == 13){
  test <- compare_models_05_mclapply(data = data_temp, scenario = scenario, true_auc = 0.75, perc_ones = 0.5, cl = 10, anti = T)
  saveRDS(test, file = paste0("data/analysis/", pred, "/covar/", paste0(scenario), "/auc075_ev05_anti.rds"))
}
if (taskid == 14){
  test <- compare_models_05_mclapply(data = data_temp, scenario = scenario, true_auc = 0.75, perc_ones = 0.2, cl = 10, anti = T)
  saveRDS(test, file = paste0("data/analysis/", pred, "/covar/", paste0(scenario), "/auc075_ev02_anti.rds"))
}
if (taskid == 15){
  test <- compare_models_05_mclapply(data = data_temp, scenario = scenario, true_auc = 0.75, perc_ones = 0.05, cl = 10, anti = T)
  saveRDS(test, file = paste0("data/analysis/", pred, "/covar/", paste0(scenario), "/auc075_ev005_anti.rds"))
}
if (taskid == 16){
  test <- compare_models_05_mclapply(data = data_temp, scenario = scenario, true_auc = 0.9, perc_ones = 0.5, cl = 10, anti = T)
  saveRDS(test, file = paste0("data/analysis/", pred, "/covar/", paste0(scenario), "/auc09_ev05_anti.rds"))
}
if (taskid == 17){
  test <- compare_models_05_mclapply(data = data_temp, scenario = scenario, true_auc = 0.9, perc_ones = 0.2, cl = 10, anti = T)
  saveRDS(test, file = paste0("data/analysis/", pred, "/covar/", paste0(scenario), "/auc09_ev02_anti.rds"))
}
if (taskid == 18){
  test <- compare_models_05_mclapply(data = data_temp, scenario = scenario, true_auc = 0.9, perc_ones = 0.05, cl = 10, anti = T)
  saveRDS(test, file = paste0("data/analysis/", pred, "/covar/", paste0(scenario), "/auc09_ev005_anti.rds"))
}

