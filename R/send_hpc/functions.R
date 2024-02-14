sample_data <- function(data, size, perc_ones) {
  
  # Separate data into two subsets based on y
  data_0 <- filter(data, y == 0)
  data_1 <- filter(data, y == 1)
  
  # Calculate the number of samples required from each subset
  num_ones <- round(size * perc_ones)
  num_zeros <- size - num_ones
  
  # Sample from each subset
  sampled_0 <- sample_n(data_0, min(num_zeros, nrow(data_0)))
  sampled_1 <- sample_n(data_1, min(num_ones, nrow(data_1)))
  
  # Combine the sampled subsets
  sampled_data <- rbind(sampled_0, sampled_1)
  
  # Shuffle the rows
  sampled_data <- sampled_data[sample(nrow(sampled_data)), ]
  
  return(sampled_data)
}
scenario <- function(){
  # set up correlations
  corr0  <-  matrix(0, npred, npred)         # matrix: set up for cov matrix, 0 on diagonals
  corr0[1:npred*p, 1:npred*p] = z            # class 0 
  diag(corr0) = 0
  # covariance structures
  sigma0 <-  diag(npred)  + corr0            # matrix: cov matrix of class 0 
  return(sigma0)
}
calculate_linear_predictor5 <- function(a, data, scenario) {
  if (scenario == "normal") {
    return(a * (data$predictor1 + data$predictor2 + data$predictor3 + data$predictor4 + 0.5*data$predictor5))
  } else if (scenario == "interaction") {
    return(a * (data$predictor1 + data$predictor2 + data$predictor3 + data$predictor4 + 0.5*data$predictor1*data$predictor2))
  } else if (scenario == "2 normal") {
    return(a * (data$predictor1 + data$predictor2 + data$predictor3 + 0.5*data$predictor4 + 0.5*data$predictor5))
  } else if (scenario == "1 interaction & 1 normal") {
    return(a * (data$predictor1 + data$predictor2 + data$predictor3 + 0.5*data$predictor1*data$predictor2 + 0.5*data$predictor4))
  } else if (scenario == "2 interaction") {
    return(a * (data$predictor1 + data$predictor2 + data$predictor3 + 0.5*data$predictor1*data$predictor2 + 0.5*data$predictor2*data$predictor3))
  }
}
calculate_and_plot_auc5 <- function(plot_title) {
  scenario <- tolower(unlist(strsplit(plot_title, ":"))[2])
  scenario <- gsub("^[[:space:]]+", "", scenario) # Remove leading spaces
  
  # Parallel setup
  no_cores <- detectCores() - 1  # Leave one core free for system processes
  cl <- makeCluster(no_cores)
  clusterExport(cl, c("calculate_linear_predictor5", "data_experiment", "n", "plogis", "glm", "predict", "colAUC", "binomial"))
  clusterEvalQ(cl, library(pROC))
  
  # Splitting a_values for parallel computation
  a_values_split <- split(a_values, cut(a_values, no_cores))
  
  # Parallel computation
  auc_results <- parLapply(cl, a_values_split, function(a_chunk) {
    auc_chunk <- numeric(length(a_chunk))
    for (i in seq_along(a_chunk)) {
      a <- a_chunk[i]
      
      # Calculate L based on the DGM (data generating mechanism)
      L <- calculate_linear_predictor5(a, data_experiment, scenario)
      L <- -mean(L) + L
      
      # Generate data based on the DGM
      y <- ifelse(runif(n) < plogis(L), 1, 0)
      
      data_experiment$y <- y
      
      model_normal <- "y ~ predictor1 + predictor2 + predictor3 + predictor4 + predictor5"
      model_interaction <- "y ~ predictor1 + predictor2 + predictor3 + predictor4 + predictor1:predictor2"
      model_2interaction <- "y ~ predictor1 + predictor2 + predictor3 + predictor1:predictor2 + predictor2:predictor3"
      
      # Fit a logistic regression model with the correct DGM
      if (scenario == "normal") {
        model <- glm(model_normal, data = data_experiment, family = binomial(link = "logit"))
      } else if (scenario == "interaction") {
        model <- glm(model_interaction, data = data_experiment, family = binomial(link = "logit"))
      } else if (scenario == "2 normal") {
        model <- glm(model_normal, data = data_experiment, family = binomial(link = "logit"))
      } else if (scenario == "1 interaction & 1 normal") {
        model <- glm(model_interaction, data = data_experiment, family = binomial(link = "logit"))
      } else if (scenario == "2 interaction") {
        model <- glm(model_2interaction, data = data_experiment, family = binomial(link = "logit"))
      }
      
      predictions <- predict(model, newdata = data_experiment, type = "response")
      auc_chunk[i] <- colAUC(predictions, data_experiment$y, plotROC = F)
    }
    return(auc_chunk)
  })
  
  stopCluster(cl)
  
  # Combine results
  auc_values <- unlist(auc_results)
  
  # Sort auc_values and a_values
  sorted_data <- data.frame(a = a_values, auc = auc_values)
  sorted_data <- sorted_data[order(sorted_data$auc), ]
  
  # Find "a" values corresponding to AUC thresholds
  a_0.6 <- sorted_data$a[which.max(sorted_data$auc >= 0.6)]
  a_0.75 <- sorted_data$a[which.max(sorted_data$auc >= 0.75)]
  a_0.9 <- sorted_data$a[which.max(sorted_data$auc >= 0.9)]
  
  # Create a table with the desired "a" values
  result_table <- data.frame(
    AUC = c(0.6, 0.75, 0.9),
    Coefficient = c(a_0.6, a_0.75, a_0.9)
  )
  
  return(list(values = result_table))
}
compare_models_05 <- function(data, scenario, perc_ones, n_iter = 100, sample_range = c(50, 100), sample_steps = 50, B = 40) {
  # Initialize an empty data frame to store results
  results_table <- data.frame(
    Iteration = integer(),
    SampleSize = integer(),
    Model = character(),
    AIC = numeric(),
    BIC = numeric(),
    AUC = numeric(),
    AUC_val = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (size in seq(sample_range[1], sample_range[2], by = sample_steps)) {
    for (i in 1:n_iter) {
      sampled_data <- sample_data(data, size, perc_ones)
      model_names <- c("model_minimal", "model_sparse", "model_sparse2", "model_normal", "model_interaction", "model_interactionsparse1", "model_interactionsparse2", "model_2interaction")
      
      for (model_name in model_names) {
        result <- tryCatch({
          model_formula <- switch(model_name,
                                  model_minimal = y ~ predictor1 + predictor2 + predictor3,
                                  model_sparse = y ~ predictor1 + predictor2 + predictor3 + predictor4,
                                  model_sparse2 = y ~ predictor1 + predictor2 + predictor3 + predictor5,
                                  model_normal = y ~ predictor1 + predictor2 + predictor3 + predictor4 + predictor5,
                                  model_interaction = y ~ predictor1 + predictor2 + predictor3 + predictor4 + predictor1:predictor2,
                                  model_interactionsparse1 = y ~ predictor1 + predictor2 + predictor3 + predictor1:predictor2,
                                  model_interactionsparse2 = y ~ predictor1 + predictor2 + predictor3 + predictor2:predictor3,
                                  model_2interaction = y ~ predictor1 + predictor2 + predictor3 + predictor1:predictor2 + predictor2:predictor3
          )
          model <- lrm(model_formula, data = sampled_data, x = TRUE, y = TRUE)
          model_validate <- validate(model, method = "boot", B = B)
          auc_val <- 0.5 * model_validate[1, 5] + 0.5
          
          list(AIC = AIC(model), BIC = BIC(model), AUC = model$stats[6], AUC_val = auc_val)
        }, error = function(e) {
          list(AIC = NA, BIC = NA, AUC = NA, AUC_val = NA)
        })
        
        # Add results to the table
        results_table <- rbind(results_table, data.frame(
          Iteration = i,
          SampleSize = size,
          Model = model_name,
          AIC = result$AIC,
          BIC = result$BIC,
          AUC = result$AUC,
          AUC_val = result$AUC_val
        ))
      }
    }
  }
  
  return(results_table)
}
compare_models_05_mclapply <- function(data, scenario, perc_ones, n_iter = 100, sample_range = c(50, 100), sample_steps = 50, B = 40) {
  # Define the sequence of sample sizes
  sample_sizes <- seq(sample_range[1], sample_range[2], by = sample_steps)
  
  # Define model names
  model_names <- c("model_minimal", "model_sparse", "model_sparse2", "model_normal", "model_interaction", "model_interactionsparse1", "model_interactionsparse2", "model_2interaction")
  
  # Function to perform model comparison for a given sample size and iteration
  run_model_comparison <- function(size, i) {
    sampled_data <- sample_data(data, size, perc_ones)
    results <- lapply(model_names, function(model_name) {
      result <- tryCatch({
        model_formula <- switch(model_name,
                                model_minimal = y ~ predictor1 + predictor2 + predictor3,
                                model_sparse = y ~ predictor1 + predictor2 + predictor3 + predictor4,
                                model_sparse2 = y ~ predictor1 + predictor2 + predictor3 + predictor5,
                                model_normal = y ~ predictor1 + predictor2 + predictor3 + predictor4 + predictor5,
                                model_interaction = y ~ predictor1 + predictor2 + predictor3 + predictor4 + predictor1:predictor2,
                                model_interactionsparse1 = y ~ predictor1 + predictor2 + predictor3 + predictor1:predictor2,
                                model_interactionsparse2 = y ~ predictor1 + predictor2 + predictor3 + predictor2:predictor3,
                                model_2interaction = y ~ predictor1 + predictor2 + predictor3 + predictor1:predictor2 + predictor2:predictor3)
        model <- lrm(model_formula, data = sampled_data, x = TRUE, y = TRUE)
        model_validate <- validate(model, method = "boot", B = B)
        auc_val <- 0.5 * model_validate[1, 5] + 0.5
        list(AIC = AIC(model), BIC = BIC(model), AUC = model$stats[6], AUC_val = auc_val)
      }, error = function(e) {
        list(AIC = NA, BIC = NA, AUC = NA, AUC_val = NA)
      })
      
      c(Iteration = i, SampleSize = size, Model = model_name, result)
    })
    do.call(rbind, results)
  }
  
  # Create tasks for each combination of sample size and iteration
  tasks <- expand.grid(size = sample_sizes, i = 1:n_iter)
  
  # Use mclapply to run tasks in parallel
  no_cores <- detectCores() - 1
  results_list <- mclapply(seq_len(nrow(tasks)), function(k) {
    with(tasks[k, ], run_model_comparison(size, i))
  }, mc.cores = no_cores)
  
  # Combine all results into a single data frame
  results_table <- do.call(rbind, results_list)
  return(results_table)
}