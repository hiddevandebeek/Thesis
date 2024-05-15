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
calculate_linear_predictor5 <- function(beta, data, scenario) {
  data <- as.data.frame(data)
  if (scenario == "1normal") {
    return(beta * (data$predictor1 + data$predictor2 + data$predictor3 + data$predictor4 + 0.5*data$predictor5))
  } else if (scenario == "1interaction") {
    return(beta * (data$predictor1 + data$predictor2 + data$predictor3 + data$predictor4 + 0.5*data$predictor1*data$predictor2))
  } else if (scenario == "2normal") {
    return(beta * (data$predictor1 + data$predictor2 + data$predictor3 + 0.5*data$predictor4 + 0.5*data$predictor5))
  } else if (scenario == "2normalinteraction") {
    return(beta * (data$predictor1 + data$predictor2 + data$predictor3 + 0.5*data$predictor1*data$predictor2 + 0.5*data$predictor4))
  } else if (scenario == "2interaction") {
    return(beta * (data$predictor1 + data$predictor2 + data$predictor3 + 0.5*data$predictor1*data$predictor2 + 0.5*data$predictor2*data$predictor3))
  } else if (scenario == "sparse") {
    return(beta * (data$predictor1 + data$predictor2 + data$predictor3 + data$predictor4))
  } else if (scenario == "minimal") {
    return(beta * (data$predictor1 + data$predictor2 + data$predictor3))
  }
}
calculate_linear_predictor10 <- function(beta, data, scenario) {
  data <- as.data.frame(data)
  if (scenario == "1normal") {
    return(beta * (data$predictor1 + data$predictor2 + data$predictor3 + data$predictor4 + 0.5*data$predictor5 + data$predictor6 + data$predictor7 + data$predictor8 + data$predictor9 + data$predictor10))
  } else if (scenario == "1interaction") {
    return(beta * (data$predictor1 + data$predictor2 + data$predictor3 + data$predictor4 + 0.5*data$predictor1*data$predictor2 + data$predictor6 + data$predictor7 + data$predictor8 + data$predictor9 + data$predictor10))
  } else if (scenario == "2normal") {
    return(beta * (data$predictor1 + data$predictor2 + data$predictor3 + 0.5*data$predictor4 + 0.5*data$predictor5 + data$predictor6 + data$predictor7 + data$predictor8 + data$predictor9 + data$predictor10))
  } else if (scenario == "2normalinteraction") {
    return(beta * (data$predictor1 + data$predictor2 + data$predictor3 + 0.5*data$predictor1*data$predictor2 + 0.5*data$predictor4 + data$predictor6 + data$predictor7 + data$predictor8 + data$predictor9 + data$predictor10))
  } else if (scenario == "2interaction") {
    return(beta * (data$predictor1 + data$predictor2 + data$predictor3 + 0.5*data$predictor1*data$predictor2 + 0.5*data$predictor2*data$predictor3 + data$predictor6 + data$predictor7 + data$predictor8 + data$predictor9 + data$predictor10))
  } else if (scenario == "sparse") {
    return(beta * (data$predictor1 + data$predictor2 + data$predictor3 + data$predictor4 + data$predictor6 + data$predictor7 + data$predictor8 + data$predictor9 + data$predictor10))
  } else if (scenario == "minimal") {
    return(beta * (data$predictor1 + data$predictor2 + data$predictor3 + data$predictor6 + data$predictor7 + data$predictor8 + data$predictor9 + data$predictor10))
  }
}
calculate_auc5 <- function(scenario) {
  # Parallel setup
  no_cores <- 10
  cl <- makeCluster(no_cores)
  clusterExport(cl, c("calculate_linear_predictor5", "data_experiment", "n", "plogis", "glm", "predict", "colAUC", "binomial"))
  clusterEvalQ(cl, library(pROC))
  
  # Splitting beta_values for parallel computation
  beta_values_split <- split(beta_values, cut(beta_values, no_cores))
  
  # Parallel computation
  auc_results <- parLapply(cl, beta_values_split, function(beta_chunk) {
    auc_chunk <- numeric(length(beta_chunk))
    for (i in seq_along(beta_chunk)) {
      beta <- beta_chunk[i]
      
      # Calculate L based on the DGM (data generating mechanism)
      L <- calculate_linear_predictor5(beta, data_experiment, scenario)
      L <- -mean(L) + L
      
      # Generate data based on the DGM
      y <- ifelse(runif(n) < plogis(L), 1, 0)
      
      data_experiment$y <- y
      
      model_normal <- "y ~ predictor1 + predictor2 + predictor3 + predictor4 + predictor5"
      model_interaction <- "y ~ predictor1 + predictor2 + predictor3 + predictor4 + predictor1:predictor2"
      model_2interaction <- "y ~ predictor1 + predictor2 + predictor3 + predictor1:predictor2 + predictor2:predictor3"
      
      # Fit a logistic regression model with the correct DGM
      if (scenario == "1normal") {
        model <- glm(model_normal, data = data_experiment, family = binomial(link = "logit"))
      } else if (scenario == "1interaction") {
        model <- glm(model_interaction, data = data_experiment, family = binomial(link = "logit"))
      } else if (scenario == "2normal") {
        model <- glm(model_normal, data = data_experiment, family = binomial(link = "logit"))
      } else if (scenario == "2normalinteraction") {
        model <- glm(model_interaction, data = data_experiment, family = binomial(link = "logit"))
      } else if (scenario == "2interaction") {
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
  
  # Sort auc_values and beta_values
  sorted_data <- data.frame(beta = beta_values, auc = auc_values)
  sorted_data <- sorted_data[order(sorted_data$auc), ]
  
  # Find "beta" values corresponding to AUC thresholds
  beta_0.6 <- sorted_data$beta[which.max(sorted_data$auc >= 0.6)]
  beta_0.75 <- sorted_data$beta[which.max(sorted_data$auc >= 0.75)]
  beta_0.9 <- sorted_data$beta[which.max(sorted_data$auc >= 0.9)]
  
  # Create a table with the desired "" values
  result_table <- data.frame(
    AUC = c(0.6, 0.75, 0.9),
    Coefficient = c(beta_0.6, beta_0.75, beta_0.9)
  )
  
  return(list(values = result_table)$values)
}
calculate_auc10 <- function(scenario) {
  # Parallel setup
  no_cores <- 10
  cl <- makeCluster(no_cores)
  clusterExport(cl, c("calculate_linear_predictor10", "data_experiment", "n", "plogis", "glm", "predict", "colAUC", "binomial"))
  clusterEvalQ(cl, library(pROC))
  
  # Splitting beta_values for parallel computation
  beta_values_split <- split(beta_values, cut(beta_values, no_cores))
  
  # Parallel computation
  auc_results <- parLapply(cl, beta_values_split, function(beta_chunk) {
    auc_chunk <- numeric(length(beta_chunk))
    for (i in seq_along(beta_chunk)) {
      beta <- beta_chunk[i]
      
      # Calculate L based on the DGM (data generating mechanism)
      L <- calculate_linear_predictor10(beta, data_experiment, scenario)
      L <- -mean(L) + L
      
      # Generate data based on the DGM
      y <- ifelse(runif(n) < plogis(L), 1, 0)
      
      data_experiment$y <- y
      model_normal <- "y ~ predictor1 + predictor2 + predictor3 + predictor4 + predictor5 + predictor6 + predictor7 + predictor8 + predictor9 + predictor10"
      model_interaction <- "y ~ predictor1 + predictor2 + predictor3 + predictor4 + predictor1:predictor2 + predictor6 + predictor7 + predictor8 + predictor9 + predictor10"
      model_2interaction <- "y ~ predictor1 + predictor2 + predictor3 + predictor1:predictor2 + predictor2:predictor3 + predictor6 + predictor7 + predictor8 + predictor9 + predictor10"
      
      # Fit a logistic regression model with the correct DGM
      if (scenario == "1normal") {
        model <- glm(model_normal, data = data_experiment, family = binomial(link = "logit"))
      } else if (scenario == "1interaction") {
        model <- glm(model_interaction, data = data_experiment, family = binomial(link = "logit"))
      } else if (scenario == "2normal") {
        model <- glm(model_normal, data = data_experiment, family = binomial(link = "logit"))
      } else if (scenario == "2normalinteraction") {
        model <- glm(model_interaction, data = data_experiment, family = binomial(link = "logit"))
      } else if (scenario == "2interaction") {
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
  
  # Sort auc_values and beta_values
  sorted_data <- data.frame(beta = beta_values, auc = auc_values)
  sorted_data <- sorted_data[order(sorted_data$auc), ]
  
  # Find "beta" values corresponding to AUC thresholds
  beta_0.6 <- sorted_data$beta[which.max(sorted_data$auc >= 0.6)]
  beta_0.75 <- sorted_data$beta[which.max(sorted_data$auc >= 0.75)]
  beta_0.9 <- sorted_data$beta[which.max(sorted_data$auc >= 0.9)]
  
  # Create a table with the desired "beta" values
  result_table <- data.frame(
    AUC = c(0.6, 0.75, 0.9),
    Coefficient = c(beta_0.6, beta_0.75, beta_0.9)
  )
  
  return(list(values = result_table)$values)
}
determine.seed <- function(true_auc, scenario, perc_ones, seed, covar, anti) {
  if (true_auc == 0.6) {
    seed <- seed + 10 + perc_ones*100
  } else if (true_auc == 0.75) {
    seed <- seed + 20 + perc_ones*100
  } else if (true_auc == 0.9) {
    seed <- seed + 30 + perc_ones*100
    }
  
  if (scenario == "1normal") {
    seed <- seed + 100
  } else if (scenario == "1interaction") {
    seed <- seed + 200
  } else if (scenario == "2normal") {
    seed <- seed + 300
  } else if (scenario == "2normalinteraction") {
    seed <- seed + 400
  } else if (scenario == "2interaction") {
    seed <- seed + 500
  }
  
  set.seed(seed)
  
  if (covar == F){
    seed <- seed + sample(1:100, 1)
  }
  
  if (anti == T){
    seed <- seed + sample(1:99, 1)
  }
  
  return(seed)
}
compare_models_05_mclapply <- function(data, true_auc, scenario, perc_ones, n_iter = 2000, sample_range = c(50, 1000), sample_steps = 50, B = 80, seed = 28, iteration = 0, covar = T, anti = F, cl = (detectCores()-1)) {
  
  # Make sure that the data is in the correct format
  if (true_auc == 0.6) {
    y <- data$y_0.6
  } else if (true_auc == 0.75) {
    y <- data$y_0.75
  } else if (true_auc == 0.9) {
    y <- data$y_0.9
  }
  data <- data.frame(data[1:5], y)
  
  # set seed for reproducibility
  seed <- determine.seed(true_auc, scenario, perc_ones, seed, covar, anti)
  seed <- seed + iteration
  
  # Define the sequence of sample sizes
  sample_sizes <- seq(sample_range[1], sample_range[2], by = sample_steps)
  
  # Define model names
  model_names <- c("model_minimal", "model_sparse", "model_sparse2", "model_normal", "model_interaction", "model_interactionsparse1", "model_interactionsparse2", "model_2interaction")
  
  # Function to perform model comparison for a given sample size and iteration
  run_model_comparison <- function(size, i) {
    set.seed(i+size+seed)
    train_test_data <- sample_data(data, (size*11), perc_ones)
    train_test_data$id <- 1:nrow(train_test_data)
    train_data <- sample_data(train_test_data, size, perc_ones)
    test_data <- train_test_data[!train_test_data$id %in% train_data$id, ]
    train_data$id <- NULL
    test_data$id <- NULL
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
        model <- lrm(model_formula, data = train_data, x = TRUE, y = TRUE)
        model_validate <- validate(model, method = "boot", B = B)
        auc_boot <- 0.5 * model_validate[1, 5] + 0.5
        predicted_probs <- predict(model, newdata = test_data, type = "fitted")
        auc_test <- colAUC(predicted_probs, test_data$y, plotROC = FALSE)
        list(AIC = AIC(model), BIC = BIC(model), AUC = as.double(model$stats[6]), AUC_boot = auc_boot, AUC_test = auc_test)
      }, error = function(e) {
        list(AIC = NA, BIC = NA, AUC = NA, AUC_boot = NA, AUC_test = NA)
      })
      
      c(Iteration = i, SampleSize = size, Model = model_name, result)
    })
    do.call(rbind, results)
  }
  
  # Create tasks for each combination of sample size and iteration
  tasks <- expand.grid(size = sample_sizes, i = 1:n_iter)
  
  # Use mclapply to run tasks in parallel
  results_list <- mclapply(seq_len(nrow(tasks)), function(k) {
    with(tasks[k, ], run_model_comparison(size, i))
  }, mc.cores = cl)
  
  # Combine all results into a single data frame
  results_table <- do.call(rbind, results_list) %>%
    data.frame()
  results_table$Scenario <- scenario
  return(results_table)
}
compare_models_10_mclapply <- function(data, true_auc, scenario, perc_ones, n_iter = 2000, sample_range = c(50, 1000), sample_steps = 50, B = 80, seed = 123, iteration = 0, covar = T, anti = F, cl = (detectCores()-1)) {
  
  # Make sure that the data is in the correct format
  if (true_auc == 0.6) {
    y <- data$y_0.6
  } else if (true_auc == 0.75) {
    y <- data$y_0.75
  } else if (true_auc == 0.9) {
    y <- data$y_0.9
  }
  data <- data.frame(data[1:10], y)
  
  # set seed for reproducibility
  seed <- determine.seed(true_auc, scenario, perc_ones, seed, covar, anti)
  seed <- seed + iteration
  
  # Define the sequence of sample sizes
  sample_sizes <- seq(sample_range[1], sample_range[2], by = sample_steps)
  
  # Define model names
  model_names <- c("model_minimal", "model_sparse", "model_sparse2", "model_normal", "model_interaction", "model_interactionsparse1", "model_interactionsparse2", "model_2interaction")
  
  # Function to perform model comparison for a given sample size and iteration
  run_model_comparison <- function(size, i) {
    set.seed(i+size+seed)
    train_test_data <- sample_data(data, (size*11), perc_ones)
    train_test_data$id <- 1:nrow(train_test_data)
    train_data <- sample_data(train_test_data, size, perc_ones)
    test_data <- train_test_data[!train_test_data$id %in% train_data$id, ]
    train_data$id <- NULL
    test_data$id <- NULL
    results <- lapply(model_names, function(model_name) {
      result <- tryCatch({
        model_formula <- switch(model_name,
                                model_minimal = y ~ predictor1 + predictor2 + predictor3 + predictor6 + predictor7 + predictor8 + predictor9 + predictor10 ,
                                model_sparse = y ~ predictor1 + predictor2 + predictor3 + predictor4 + predictor6 + predictor7 + predictor8 + predictor9 + predictor10,
                                model_sparse2 = y ~ predictor1 + predictor2 + predictor3 + predictor5 + predictor6 + predictor7 + predictor8 + predictor9 + predictor10,
                                model_normal = y ~ predictor1 + predictor2 + predictor3 + predictor4 + predictor5 + predictor6 + predictor7 + predictor8 + predictor9 + predictor10,
                                model_interaction = y ~ predictor1 + predictor2 + predictor3 + predictor4 + predictor1:predictor2 + predictor6 + predictor7 + predictor8 + predictor9 + predictor10,
                                model_interactionsparse1 = y ~ predictor1 + predictor2 + predictor3 + predictor1:predictor2 + predictor6 + predictor7 + predictor8 + predictor9 + predictor10,
                                model_interactionsparse2 = y ~ predictor1 + predictor2 + predictor3 + predictor2:predictor3 + predictor6 + predictor7 + predictor8 + predictor9 + predictor10,
                                model_2interaction = y ~ predictor1 + predictor2 + predictor3 + predictor1:predictor2 + predictor2:predictor3 + predictor6 + predictor7 + predictor8 + predictor9 + predictor10)
        model <- lrm(model_formula, data = train_data, x = TRUE, y = TRUE)
        model_validate <- validate(model, method = "boot", B = B)
        auc_boot <- 0.5 * model_validate[1, 5] + 0.5
        predicted_probs <- predict(model, newdata = test_data, type = "fitted")
        auc_test <- colAUC(predicted_probs, test_data$y, plotROC = FALSE)
        list(AIC = AIC(model), BIC = BIC(model), AUC = as.double(model$stats[6]), AUC_boot = auc_boot, AUC_test = auc_test)
      }, error = function(e) {
        list(AIC = NA, BIC = NA, AUC = NA, AUC_boot = NA, AUC_test = NA)
      })
      
      c(Iteration = i, SampleSize = size, Model = model_name, result)
    })
    do.call(rbind, results)
  }
  
  # Create tasks for each combination of sample size and iteration
  tasks <- expand.grid(size = sample_sizes, i = 1:n_iter)
  
  # Use mclapply to run tasks in parallel
  results_list <- mclapply(seq_len(nrow(tasks)), function(k) {
    with(tasks[k, ], run_model_comparison(size, i))
  }, mc.cores = cl)
  
  # Combine all results into a single data frame
  results_table <- do.call(rbind, results_list) %>%
    data.frame()
  results_table$Scenario <- scenario
  return(results_table)
}
plot_win_percentage_by_sample_size <- function(data, models, correct_model) {
  winning_models <- data %>%
    filter(Model %in% models) %>%
    group_by(Iteration, SampleSize) %>%
    dplyr::summarize(
      AIC = Model[which.min(AIC)],
      BIC = Model[which.min(BIC)],
      AUC = Model[which.max(AUC)],
      AUC_boot = Model[which.max(AUC_boot)],
      .groups = "drop"
    )
  
  model_win_percentage_by_sample_size <- winning_models %>%
    pivot_longer(cols = c("AIC", "BIC", "AUC", "AUC_boot"),
                 names_to = "Metric", values_to = "Winning_Model") %>%
    mutate(
      Metric = factor(Metric, levels = c("AIC", "BIC", "AUC", "AUC_boot")),
      Winning_Model = as.character(Winning_Model)
    ) %>%
    group_by(SampleSize, Metric, Winning_Model) %>%
    dplyr::summarise(Wins = n(), .groups = "drop") %>%
    group_by(SampleSize, Metric) %>%
    mutate(
      Total_Iterations = 2000, # Fixed total iterations
      Failed_Iterations = Total_Iterations - sum(Wins),
      Win_Percentage = (Wins / Total_Iterations) * 100,
      Failed_Percentage = (Failed_Iterations / Total_Iterations) * 100
    ) %>%
    ungroup()
  
  df_model_correct <- model_win_percentage_by_sample_size %>%
    filter(Winning_Model == correct_model) %>%
    mutate(SampleSize = as.numeric(SampleSize))
  
  df_failed_percentage_mean <- model_win_percentage_by_sample_size %>%
    group_by(SampleSize) %>%
    dplyr::summarize(Mean_Failed_Percentage = mean(Failed_Percentage), .groups = "drop") %>%
    mutate(Metric = "Mean_Failed_Percentage", Win_Percentage = Mean_Failed_Percentage) %>%
    select(SampleSize, Metric, Win_Percentage) %>%
    mutate(SampleSize = unlist(SampleSize))
  
  df_combined <- bind_rows(df_model_correct %>% select(SampleSize, Metric, Win_Percentage),
                           df_failed_percentage_mean)
  
  # Manual names mapping
  manual_names <- c("AIC", "BIC", "AUC", "AUC_boot", "Mean_Failed_Percentage")
  names_map <- c("AIC" = "AIC", "BIC" = "BIC", "AUC" = "AUC", 
                 "AUC_boot" = "Bootstrapped AUC", 
                 "Mean_Failed_Percentage" = "Failed Iterations")
  
  # Update 'Metric' in df_combined to use manual names for the legend
  df_combined$Metric <- factor(df_combined$Metric, levels = manual_names, labels = names_map[manual_names])
  
  # Define colors and linetypes for each metric name
  color_values <- c("#BC3C29FF", "#0072B5FF", "#E18727FF", "#20854EFF", "black") 
  linetype_values <- c("solid", "solid", "solid", "solid", "dashed")
  
  # Plot
  p <- ggplot(df_combined, aes(x = SampleSize, y = Win_Percentage, color = Metric, linetype = Metric)) +
    geom_line() +
    theme_minimal(base_size = 18) +
    labs(
      x = "Sample Size",
      y = "Percentage (%)",
      color = "Metric",
      linetype = "Metric"
    ) +
    scale_y_continuous(limits = c(0, 100)) +
    scale_x_continuous(limits = c(0, 1000)) +
    scale_color_manual(values = color_values) +
    scale_linetype_manual(values = linetype_values) +
    theme(legend.position = "right") +
    theme_apa() +     
    theme(axis.text.x = element_text(size = 18, family="STIX Two Text"),
          axis.text.y = element_text(size = 18, family="STIX Two Text"),
          axis.title.x = element_text(size = 18, family="STIX Two Text"),
          axis.title.y = element_text(size = 18, family="STIX Two Text"),
          legend.text = element_text(size = 18, family="STIX Two Text"), 
          legend.title = element_text(size = 18, family="STIX Two Text")) +
    coord_cartesian(expand = T)
  return(p)
}
plot_average_auc_by_model_and_sample_size <- function(data, models, correct_model, min = NULL, max = NULL) {
  # First, identify the winning model based on metrics
  winning_models <- data %>%
    filter(Model %in% models) %>%
    group_by(Iteration, SampleSize) %>%
    summarise(
      AIC = Model[which.min(AIC)],
      BIC = Model[which.min(BIC)],
      AUC = Model[which.max(AUC)],
      AUC_boot = Model[which.max(AUC_boot)],
      .groups = "drop"
    )
  
  avg_auc_data <- winning_models %>%
    pivot_longer(cols = c("AIC", "BIC", "AUC", "AUC_boot"),
                 names_to = "Metric", values_to = "Winning_Model") %>%
    left_join(data, by = c("Winning_Model" = "Model", "Iteration", "SampleSize")) %>%
    select(Iteration, SampleSize, Metric, AUC_test) %>%
    mutate(SampleSize = as.numeric(SampleSize)) %>%
    mutate(SampleSize = unlist(SampleSize)) %>%
    mutate(AUC_test = as.numeric(AUC_test)) %>%
    mutate(AUC_test = unlist(AUC_test)) %>%
    group_by(SampleSize, Metric) %>%
    summarise(Average_AUC = mean(AUC_test, na.rm = TRUE), .groups = "drop")
  
  # Manual names mapping
  manual_names <- c("AIC", "BIC", "AUC", "AUC_boot", "Mean_Failed_Percentage")
  names_map <- c("AIC" = "AIC", "BIC" = "BIC", "AUC" = "AUC", 
                 "AUC_boot" = "Bootstrapped AUC", 
                 "Mean_Failed_Percentage" = "Failed Iterations")
  
  # Update 'Metric' in df_combined to use manual names for the legend
  avg_auc_data$Metric <- factor(avg_auc_data$Metric, levels = manual_names, labels = names_map[manual_names])
  
  # Define colors and linetypes for each metric name
  color_values <- c("#BC3C29FF", "#0072B5FF", "#E18727FF", "#20854EFF", "black") 
  linetype_values <- c("solid", "solid", "solid", "solid", "dashed")
  
  # Plot
  p <- ggplot(avg_auc_data, aes(x = SampleSize, y = Average_AUC, color = Metric, group = Metric)) +
    geom_line() +
    theme_minimal(base_size = 18) +
    labs(
      x = "Sample Size",
      y = "Average AUC (Test)",
      color = "Metric"
    ) +
    scale_y_continuous(breaks = seq(from = min, to = max, by = 0.01),
                       limits = c(min, max)) +  # Assuming AUC is between 0 and 1
    scale_x_continuous(limits = c(0, 1000)) +
    scale_color_manual(values = color_values) +
    scale_linetype_manual(values = linetype_values) +
    theme_apa() +    
    theme(
      legend.position = "right",
      axis.text.x = element_text(size = 18, family="STIX Two Text"),
      axis.text.y = element_text(size = 18, family="STIX Two Text"),
      axis.title.x = element_text(size = 18, family="STIX Two Text"),
      axis.title.y = element_text(size = 18, family="STIX Two Text"),
      legend.text = element_text(size = 18, family="STIX Two Text"),
      legend.title = element_text(size = 18, family="STIX Two Text")
    ) +
    coord_cartesian(expand = T)
  return(p)
}
win_percentage <- function(data, models, correct_model, AUC, EV) {
  winning_models <- data %>%
    filter(Model %in% models) %>%
    group_by(Iteration, SampleSize) %>%
    dplyr::summarize(
      AIC = Model[which.min(AIC)],
      BIC = Model[which.min(BIC)],
      AUC = Model[which.max(AUC)],
      AUC_boot = Model[which.max(AUC_boot)],
      .groups = "drop"
    )
  
  model_win_percentage_by_sample_size <- winning_models %>%
    pivot_longer(cols = c("AIC", "BIC", "AUC", "AUC_boot"),
                 names_to = "Metric", values_to = "Winning_Model") %>%
    mutate(
      Metric = factor(Metric, levels = c("AIC", "BIC", "AUC", "AUC_boot")),
      Winning_Model = as.character(Winning_Model)
    ) %>%
    group_by(SampleSize, Metric, Winning_Model) %>%
    dplyr::summarise(Wins = n(), .groups = "drop") %>%
    group_by(SampleSize, Metric) %>%
    mutate(
      Total_Iterations = 2000, # Fixed total iterations
      Failed_Iterations = Total_Iterations - sum(Wins),
      Win_Percentage = (Wins / Total_Iterations) * 100,
      Failed_Percentage = (Failed_Iterations / Total_Iterations) * 100
    ) %>%
    ungroup()
  
  df_model_correct <- model_win_percentage_by_sample_size %>%
    filter(Winning_Model == correct_model) %>%
    mutate(SampleSize = as.numeric(SampleSize))
  
  df_failed_percentage_mean <- model_win_percentage_by_sample_size %>%
    group_by(SampleSize) %>%
    dplyr::summarize(Mean_Failed_Percentage = mean(Failed_Percentage), .groups = "drop") %>%
    mutate(Metric = "Mean_Failed_Percentage", Win_Percentage = Mean_Failed_Percentage) %>%
    select(SampleSize, Metric, Win_Percentage) %>%
    mutate(SampleSize = unlist(SampleSize))
  
  df_combined <- bind_rows(df_model_correct %>% select(SampleSize, Metric, Win_Percentage),
                           df_failed_percentage_mean)
  
  #cbind AUC and EV
  df_combined  <- cbind(df_combined, EV, AUC)
 
  return(df_combined)
}
percentage_plot <- function(data, axis_y = "AUC", axis_x = "EV"){
  df_combined <- data
  df_combined$EV <- factor(df_combined$EV)
  df_combined$AUC <- factor(df_combined$AUC)
  
  df_combined[[axis_y]] <- factor(df_combined[[axis_y]])
  df_combined[[axis_x]] <- factor(df_combined[[axis_x]])
  
  # Manual names mapping
  manual_names <- c("AIC", "BIC", "AUC", "AUC_boot", "Mean_Failed_Percentage")
  names_map <- c("AIC" = "AIC", "BIC" = "BIC", "AUC" = "AUC", 
                 "AUC_boot" = "Bootstrapped AUC", 
                 "Mean_Failed_Percentage" = "Failed Iterations")
  
  # Update 'Metric' in df_combined to use manual names for the legend
  df_combined$Metric <- factor(df_combined$Metric, levels = manual_names, labels = names_map[manual_names])
  
  # Define colors and linetypes for each metric name
  color_values <- c("#BC3C29FF", "#0072B5FF", "#E18727FF", "#20854EFF", "black") 
  linetype_values <- c("solid", "solid", "solid", "solid", "dashed")
  
  ev_names <- setNames(c("Event rate = 50%", "Event rate = 20%", "Event rate = 5%"), levels(df_combined$EV))
  auc_names <- setNames(c("AUC = 0.6", "AUC = 0.75", "AUC = 0.9"), levels(df_combined$AUC))
  
  # Plot
  p <- ggplot(df_combined, aes(x = SampleSize, y = Win_Percentage, color = Metric, linetype = Metric)) +
    geom_line() +
    facet_grid(reformulate(axis_x, axis_y), labeller = labeller(EV = ev_names, AUC = auc_names)) +
    theme_minimal(base_size = 18) +
    labs(
      x = "Sample Size",
      y = "Percentage (%)",
      color = "Metric",
      linetype = "Metric"
    ) +
    scale_y_continuous(limits = c(0, 100)) +
    scale_x_continuous(limits = c(0, 1000)) +
    scale_color_manual(values = color_values) +
    scale_linetype_manual(values = linetype_values) +
    theme(legend.position = "right") +
    theme_apa() +     
    theme(axis.text.x = element_text(size = 18, family="STIX Two Text"),
          axis.text.y = element_text(size = 18, family="STIX Two Text"),
          axis.title.x = element_text(size = 18, family="STIX Two Text"),
          axis.title.y = element_text(size = 18, family="STIX Two Text"),
          legend.text = element_text(size = 18, family="STIX Two Text"), 
          legend.title = element_text(size = 18, family="STIX Two Text"),
          strip.text.x = element_text(size = 18, family="STIX Two Text"),  # Increased size for x strip text
          strip.text.y = element_text(size = 18, family="STIX Two Text")
          ) +
    coord_cartesian(expand = T)
  return(p)
}
average_auc <- function(data, models, AUC, EV) {
  # First, identify the winning model based on metrics
  winning_models <- data %>%
    filter(Model %in% models) %>%
    group_by(Iteration, SampleSize) %>%
    summarise(
      AIC = Model[which.min(AIC)],
      BIC = Model[which.min(BIC)],
      AUC = Model[which.max(AUC)],
      AUC_boot = Model[which.max(AUC_boot)],
      .groups = "drop"
    )
  
  avg_auc_data <- winning_models %>%
    pivot_longer(cols = c("AIC", "BIC", "AUC", "AUC_boot"),
                 names_to = "Metric", values_to = "Winning_Model") %>%
    left_join(data, by = c("Winning_Model" = "Model", "Iteration", "SampleSize")) %>%
    select(Iteration, SampleSize, Metric, AUC_test) %>%
    mutate(SampleSize = as.numeric(SampleSize)) %>%
    mutate(SampleSize = unlist(SampleSize)) %>%
    mutate(AUC_test = as.numeric(AUC_test)) %>%
    mutate(AUC_test = unlist(AUC_test)) %>%
    group_by(SampleSize, Metric) %>%
    summarise(Average_AUC = mean(AUC_test, na.rm = TRUE), .groups = "drop")
  
  #cbind AUC and EV
  avg_auc_data <- cbind(avg_auc_data, EV, AUC)
  
  return(avg_auc_data)
}
auc_plot <- function(data, axis_y = "AUC", axis_x = "EV"){
  avg_auc_data <- data
  avg_auc_data$EV <- factor(avg_auc_data$EV)
  avg_auc_data$AUC <- factor(avg_auc_data$AUC)
  
  avg_auc_data[[axis_y]] <- factor(avg_auc_data[[axis_y]])
  avg_auc_data[[axis_x]] <- factor(avg_auc_data[[axis_x]])
  
  # Manual names mapping
  manual_names <- c("AIC", "BIC", "AUC", "AUC_boot", "Mean_Failed_Percentage")
  names_map <- c("AIC" = "AIC", "BIC" = "BIC", "AUC" = "AUC", 
                 "AUC_boot" = "Bootstrapped AUC", 
                 "Mean_Failed_Percentage" = "Failed Iterations")
  
  # Update 'Metric' in df_combined to use manual names for the legend
  avg_auc_data$Metric <- factor(avg_auc_data$Metric, levels = manual_names, labels = names_map[manual_names])
  
  # Define colors and linetypes for each metric name
  color_values <- c("#BC3C29FF", "#0072B5FF", "#E18727FF", "#20854EFF", "black") 
  linetype_values <- c("solid", "solid", "solid", "solid", "dashed")
  
  ev_names <- setNames(c("Event Fraction = 0.05", "Event Fraction = 0.2", "Event Fraction = 0.5"), levels(avg_auc_data$EV))
  auc_names <- setNames(c("AUC = 0.6", "AUC = 0.75", "AUC = 0.9"), levels(avg_auc_data$AUC))
  
  # Plot
  p <- ggplot(avg_auc_data, aes(x = SampleSize, y = Average_AUC, color = Metric, group = Metric)) +
    geom_line() +
    facet_grid2(reformulate(axis_x, axis_y), scales = "free_y", labeller = labeller(EV = ev_names, AUC = auc_names)) +
    theme_minimal(base_size = 18) +
    labs(
      x = "Sample Size",
      y = "Average AUC (Test)",
      color = "Metric"
    ) +
    scale_x_continuous(limits = c(0, 1000)) +
    scale_color_manual(values = color_values) +
    scale_linetype_manual(values = linetype_values) +
    theme_apa() +  
    coord_cartesian(expand = F) +
    theme(
      legend.position = "right",
      axis.text.x = element_text(size = 18, family="STIX Two Text"),
      axis.text.y = element_text(size = 18, family="STIX Two Text"),
      axis.title.x = element_text(size = 18, family="STIX Two Text"),
      axis.title.y = element_text(size = 18, family="STIX Two Text"),
      legend.text = element_text(size = 18, family="STIX Two Text"),
      legend.title = element_text(size = 18, family="STIX Two Text"),
      strip.text.x = element_text(size = 18, family="STIX Two Text"),  # Increased size for x strip text
      strip.text.y = element_text(size = 18, family="STIX Two Text")   # Increased size for y strip text
    ) +
    coord_cartesian(expand = T)
  return(p)
}
  
