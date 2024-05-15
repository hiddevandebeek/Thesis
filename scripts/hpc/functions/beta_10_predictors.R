## Covariance
### Initialize variables
set.seed(28)                 # Set random seed for reproducibility
p      <- 1                  # proportion of correlated predictors
npred  <- 10                  # number of predictors
z      <- 0.2                # z 
n      <- 1000000            # Number of samples in the dataset

# structures
mu <- c(rep(1,npred))       # vector: means
sigma <- scenario()     

### Experiment
data_experiment <- mvrnorm(n, mu = mu, Sigma = sigma) %>%
  as.data.frame()
colnames(data_experiment) <- c(paste0("predictor", 1:(ncol(data_experiment))))
beta_values <- seq(from = 0.05, to = 1.2, by = 0.001)

# Calculate and store "a" values for different scenarios
beta_value_1end_normal <- calculate_auc10("1normal")
beta_value_1end_interaction <- calculate_auc10("1interaction")
beta_value_2end_normal <- calculate_auc10("2normal")
beta_value_2end_normalinteraction <- calculate_auc10("2normalinteraction")
beta_value_2end_interaction <- calculate_auc10("2interaction")

# Create a data frame to store all the results
beta_values_10predictors_covariance <- data.frame(
  Scenario = c(
    "1 end predictor: 1normal",
    "1 end predictor: 1interaction",
    "2 end predictors: 2normal",
    "2 end predictors: 2normalinteraction",
    "2 end predictors: 2interaction"
  ),
  AUC_0.6 = c(
    beta_value_1end_normal$Coefficient[1],
    beta_value_1end_interaction$Coefficient[1],
    beta_value_2end_normal$Coefficient[1],
    beta_value_2end_normalinteraction$Coefficient[1],
    beta_value_2end_interaction$Coefficient[1]
  ),
  AUC_0.75 = c(
    beta_value_1end_normal$Coefficient[2],
    beta_value_1end_interaction$Coefficient[2],
    beta_value_2end_normal$Coefficient[2],
    beta_value_2end_normalinteraction$Coefficient[2],
    beta_value_2end_interaction$Coefficient[2]
  ),
  AUC_0.9 = c(
    beta_value_1end_normal$Coefficient[3],
    beta_value_1end_interaction$Coefficient[3],
    beta_value_2end_normal$Coefficient[3],
    beta_value_2end_normalinteraction$Coefficient[3],
    beta_value_2end_interaction$Coefficient[3]
  )
)

# Save the data frame to a file (e.g., CSV)
write.csv(beta_values_10predictors_covariance, file = "data/datasets/beta_values/10predictors_covariance.csv", row.names = FALSE)
