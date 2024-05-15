##### LIBRARIES
library(parallel)
library(tidyverse)
library(rms)
library(MASS)
library(caTools)
source("/home/julius_bs/hvandebeek/analysis/R/send_hpc/scripts/functions.R")

##### RUN CODE
scenario_temp <- "normal"
scenariopaste <- paste0("1normal")
pred <- paste0("5pred")
var <- paste0("covar")
taskid <- Sys.getenv('SLURM_ARRAY_TASK_ID')

data <- readRDS(paste0("/home/julius_bs/hvandebeek/analysis/data/datasets/full_data/", pred, "/", var,"/data_predictors.rds"))
y <- readRDS(paste0("/home/julius_bs/hvandebeek/analysis/data/datasets/full_data/", pred, "/", var, "/", scenariopaste, ".rds"))
data_temp <- as.data.frame(cbind(data, y))

if (taskid == 1){
  test1 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.6, perc_ones = 0.5, cl = 10)
  saveRDS(test1, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc06_ev05.rds"))
}
if (taskid == 2){
  test2 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.6, perc_ones = 0.2, cl = 10)
  saveRDS(test2, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc06_ev02.rds"))
}
if (taskid == 3){
  test3 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.6, perc_ones = 0.05, cl = 4)
  saveRDS(test3, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc06_ev005.rds"))
}
if (taskid == 4){
  test4 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.75, perc_ones = 0.5, cl = 10)
  saveRDS(test4, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc075_ev05.rds"))
}
if (taskid == 5){
  test5 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.75, perc_ones = 0.2, cl = 10)
  saveRDS(test5, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc075_ev02.rds"))
}
if (taskid == 6){
  test6 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.75, perc_ones = 0.05, cl = 10)
  saveRDS(test6, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc075_ev005.rds"))
}
if (taskid == 7){
  test7 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.9, perc_ones = 0.5, cl = 10)
  saveRDS(test7, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc09_ev05.rds"))
}
if (taskid == 8){
  test8 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.9, perc_ones = 0.2, cl = 10)
  saveRDS(test8, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc09_ev02.rds"))
}
if (taskid == 9){
  test9 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.9, perc_ones = 0.05, cl = 10)
  saveRDS(test9, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc09_ev005.rds"))
}

y <- readRDS(paste0("/home/julius_bs/hvandebeek/analysis/data/datasets/full_data/", pred, "/", var, "/", scenariopaste, "_anti.rds"))
data_temp <- as.data.frame(cbind(data, y))

if (taskid == 19){
  test1 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.6, perc_ones = 0.5, cl = 10, anti = T)
  saveRDS(test1, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc06_ev05_anti.rds"))
}
if (taskid == 20){
  test2 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.6, perc_ones = 0.2, cl = 10, anti = T)
  saveRDS(test2, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc06_ev02_anti.rds"))
}
if (taskid == 21){
  test3 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.6, perc_ones = 0.05, cl = 10, anti = T)
  saveRDS(test3, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc06_ev005_anti.rds"))
}
if (taskid == 22){
  test4 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.75, perc_ones = 0.5, cl = 10, anti = T)
  saveRDS(test4, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc075_ev05_anti.rds"))
}
if (taskid == 23){
  test5 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.75, perc_ones = 0.2, cl = 10, anti = T)
  saveRDS(test5, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc075_ev02_anti.rds"))
}
if (taskid == 24){
  test6 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.75, perc_ones = 0.05, cl = 10, anti = T)
  saveRDS(test6, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc075_ev005_anti.rds"))
}
if (taskid == 25){
  test7 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.9, perc_ones = 0.5, cl = 10, anti = T)
  saveRDS(test7, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc09_ev05_anti.rds"))
}
if (taskid == 26){
  test8 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.9, perc_ones = 0.2, cl = 10, anti = T)
  saveRDS(test8, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc09_ev02_anti.rds"))
}
if (taskid == 27){
  test9 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.9, perc_ones = 0.05, cl = 10, anti = T)
  saveRDS(test9, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc09_ev005_anti.rds"))
}

##### PART 2
var <- paste0("uncor")
data <- readRDS(paste0("/home/julius_bs/hvandebeek/analysis/data/datasets/full_data/", pred, "/", var,"/data_predictors.rds"))
y <- readRDS(paste0("/home/julius_bs/hvandebeek/analysis/data/datasets/full_data/", pred, "/", var, "/", scenariopaste, ".rds"))
data_temp <- as.data.frame(cbind(data, y))

if (taskid == 10){
  test10 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.6, perc_ones = 0.5, covar = F, cl = 10)
  saveRDS(test10, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc06_ev05.rds"))
}
if (taskid == 11){
  test11 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.6, perc_ones = 0.2, covar = F, cl = 10)
  saveRDS(test11, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc06_ev02.rds"))
}
if (taskid == 12){
  test12 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.6, perc_ones = 0.05, covar = F, cl = 10)
  saveRDS(test12, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc06_ev005.rds"))
}
if (taskid == 13){
  test13 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.75, perc_ones = 0.5, covar = F, cl = 10)
  saveRDS(test13, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc075_ev05.rds"))
}
if (taskid == 14){
  test14 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.75, perc_ones = 0.2, covar = F, cl = 10)
  saveRDS(test14, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc075_ev02.rds"))
}
if (taskid == 15){
  test15 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.75, perc_ones = 0.05, covar = F, cl = 10)
  saveRDS(test15, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc075_ev005.rds"))
}
if (taskid == 16){
  test16 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.9, perc_ones = 0.5, covar = F, cl = 10)
  saveRDS(test16, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc09_ev05.rds"))
}
if (taskid == 17){
  test17 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.9, perc_ones = 0.2, covar = F, cl = 10)
  saveRDS(test17, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc09_ev02.rds"))
}
if (taskid == 18){
  test18 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.9, perc_ones = 0.05, covar = F, cl = 10)
  saveRDS(test18, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc09_ev005.rds"))
}

y <- readRDS(paste0("/home/julius_bs/hvandebeek/analysis/data/datasets/full_data/", pred, "/", var, "/", scenariopaste, "_anti.rds"))
data_temp <- as.data.frame(cbind(data, y))

if (taskid == 28){
  test10 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.6, perc_ones = 0.5, covar = F, cl = 10, anti = T)
  saveRDS(test10, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc06_ev05_anti.rds"))
}
if (taskid == 29){
  test11 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.6, perc_ones = 0.2, covar = F, cl = 10, anti = T)
  saveRDS(test11, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc06_ev02_anti.rds"))
}
if (taskid == 30){
  test12 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.6, perc_ones = 0.05, covar = F, cl = 10, anti = T)
  saveRDS(test12, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc06_ev005_anti.rds"))
}
if (taskid == 31){
  test13 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.75, perc_ones = 0.5, covar = F, cl = 10, anti = T)
  saveRDS(test13, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc075_ev05_anti.rds"))
}
if (taskid == 32){
  test14 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.75, perc_ones = 0.2, covar = F, cl = 10, anti = T)
  saveRDS(test14, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc075_ev02_anti.rds"))
}
if (taskid == 33){
  test15 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.75, perc_ones = 0.05, covar = F, cl = 10, anti = T)
  saveRDS(test15, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc075_ev005_anti.rds"))
}
if (taskid == 34){
  test16 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.9, perc_ones = 0.5, covar = F, cl = 10, anti = T)
  saveRDS(test16, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc09_ev05_anti.rds"))
}
if (taskid == 35){
  test17 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.9, perc_ones = 0.2, covar = F, cl = 10, anti = T)
  saveRDS(test17, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc09_ev02_anti.rds"))
}
if (taskid == 36){
  test18 <- compare_models_05_mclapply(data = data_temp, scenario = scenario_temp, true_auc = 0.9, perc_ones = 0.05, covar = F, cl = 10, anti = T)
  saveRDS(test18, file = paste0("/home/julius_bs/hvandebeek/analysis/data/analysis/", pred, "/", var, "/", scenariopaste, "/auc09_ev005_anti.rds"))
}