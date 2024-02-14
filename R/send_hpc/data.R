# Set random seed for reproducibility
set.seed(28)
p      <- 1                             # proportion of correlated predictors
npred  <- 5                             # number of predictors
z      <- 0.2                           # z 
n      <- 200000                       # Number of samples in the dataset
a_values_5predictors_covariance <- read.csv("data/simulation/a_values/5predictors_covariance.csv")

mu0 <- c(rep(1,npred))  
sigma0 <- scenario()

data_experiment <- mvrnorm(n, mu = mu0, Sigma = sigma0) %>%
  as.data.frame()
colnames(data_experiment) <- c(paste0("predictor", 1:(ncol(data_experiment))))

# Simulate binary y to have Prob(y=1) = 1/[1+exp(-L)]
a <- a_values_5predictors_covariance$AUC_0.6[1]
L <-  calculate_linear_predictor5(a, data_experiment, "normal")
L <- - mean(L) + L
y <- ifelse(runif(n) < plogis(L), 1, 0)
data_experiment_temp <- cbind(data_experiment, y)

