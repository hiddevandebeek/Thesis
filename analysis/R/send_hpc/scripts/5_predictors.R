##### LIBRARIES
library(MASS)
library(tidyverse)
library(parallel)
library(pROC)
library(caTools)
source("/home/julius_bs/hvandebeek/analysis/R/send_hpc/scripts/functions.R")

## Covariance
### Initialize variables
taskid <- Sys.getenv('SLURM_ARRAY_TASK_ID')
set.seed(28)                 # Set random seed for reproducibility
p      <- 1                  # proportion of correlated predictors
npred  <- 5                  # number of predictors
z      <- 0.2                # z 
n      <- 1000000            # Number of samples in the dataset

if (taskid == 1){
# structures
mu <- c(rep(1,npred))       # vector: means
sigma <- scenario()     

### Experiment
data_experiment <- mvrnorm(n, mu = mu, Sigma = sigma) %>%
  as.data.frame()
colnames(data_experiment) <- c(paste0("predictor", 1:(ncol(data_experiment))))
a_values <- seq(from = 0.10, to = 1.6, by = 0.001)

# Calculate and store "a" values for different scenarios
a_value_1end_normal <-calculate_auc5("normal")
a_value_1end_interaction <- calculate_auc5("interaction")
a_value_2end_normal <- calculate_auc5("2 normal")
a_value_2end_normalinteraction <- calculate_auc5("1 interaction & 1 normal")
a_value_2end_interaction <- calculate_auc5("2 interaction")

# Create a data frame to store all the results
a_values_5predictors_covariance <- data.frame(
  Scenario = c(
    "1 end predictor: normal",
    "1 end predictor: interaction",
    "2 end predictors: 2 normal",
    "2 end predictors: 1 interaction & 1 normal",
    "2 end predictors: 2 interaction"
  ),
  AUC_0.6 = c(
    a_value_1end_normal$Coefficient[1],
    a_value_1end_interaction$Coefficient[1],
    a_value_2end_normal$Coefficient[1],
    a_value_2end_normalinteraction$Coefficient[1],
    a_value_2end_interaction$Coefficient[1]
  ),
  AUC_0.75 = c(
    a_value_1end_normal$Coefficient[2],
    a_value_1end_interaction$Coefficient[2],
    a_value_2end_normal$Coefficient[2],
    a_value_2end_normalinteraction$Coefficient[2],
    a_value_2end_interaction$Coefficient[2]
  ),
  AUC_0.9 = c(
    a_value_1end_normal$Coefficient[3],
    a_value_1end_interaction$Coefficient[3],
    a_value_2end_normal$Coefficient[3],
    a_value_2end_normalinteraction$Coefficient[3],
    a_value_2end_interaction$Coefficient[3]
  )
)

# Save the data frame to a file (e.g., CSV)
write.csv(a_values_5predictors_covariance, file = "/home/julius_bs/hvandebeek/analysis/data/datasets/a_values/5predictors_covariance.csv", row.names = FALSE)
}

if (taskid ==2){
## No Covariance
### Initialize variables
set.seed(28)                 # Set random seed for reproducibility    
p <- 0                       # proportion 

# structures
mu <- c(rep(1,npred))        # vector: means
sigma <- scenario()   

### Experiment
data_experiment <- mvrnorm(n, mu = mu, Sigma = sigma) %>%
  as.data.frame()
colnames(data_experiment) <- c(paste0("predictor", 1:(ncol(data_experiment))))

# provide a_values
a_values <- seq(from = 0.10, to = 1.6, by = 0.001)

# Calculate and store "a" values for different scenarios
a_value_1end_normal <- calculate_auc5("normal")
a_value_1end_interaction <- calculate_auc5("interaction")
a_value_2end_normal <- calculate_auc5("2 normal")
a_value_2end_normalinteraction <- calculate_auc5("1 interaction & 1 normal")
a_value_2end_interaction <- calculate_auc5("2 interaction")

# Create a data frame to store all the results
a_values_5predictors_uncorrelated <- data.frame(
  Scenario = c(
    "1 end predictor: normal",
    "1 end predictor: interaction",
    "2 end predictors: 2 normal",
    "2 end predictors: 1 interaction & 1 normal",
    "2 end predictors: 2 interaction"
  ),
  AUC_0.6 = c(
    a_value_1end_normal$Coefficient[1],
    a_value_1end_interaction$Coefficient[1],
    a_value_2end_normal$Coefficient[1],
    a_value_2end_normalinteraction$Coefficient[1],
    a_value_2end_interaction$Coefficient[1]
  ),
  AUC_0.75 = c(
    a_value_1end_normal$Coefficient[2],
    a_value_1end_interaction$Coefficient[2],
    a_value_2end_normal$Coefficient[2],
    a_value_2end_normalinteraction$Coefficient[2],
    a_value_2end_interaction$Coefficient[2]
  ),
  AUC_0.9 = c(
    a_value_1end_normal$Coefficient[3],
    a_value_1end_interaction$Coefficient[3],
    a_value_2end_normal$Coefficient[3],
    a_value_2end_normalinteraction$Coefficient[3],
    a_value_2end_interaction$Coefficient[3]
  )
)

# Save the data frame to a file (e.g., CSV)
write.csv(a_values_5predictors_uncorrelated, file = "/home/julius_bs/hvandebeek/analysis/data/datasets/a_values/5predictors_uncorrelated.csv", row.names = FALSE)
}