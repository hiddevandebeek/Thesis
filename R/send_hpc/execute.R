##### LIBRARIES
library(parallel)
library(caret)
library(tidyverse)
library(rms)
source("functions.R")
source("data.R")

##### RUN CODE

slurm_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
to_do    <- c(1:100)
start <- to_do[slurm_id]
stop <- to_do[slurm_id]
data <- data_temp
test <- compare_models_05(data, perc_ones = 0.5)
saveRDS(test, file = "data/analysis/test.rds")