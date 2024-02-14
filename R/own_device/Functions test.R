compare_models_05 <- function(data, scenario, perc_ones, n_iter = 200, sample_range = c(50, 300), sample_steps = 50, B = 40) {
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
  
  # Samplerange/steps = points in grid
  # Iterations*points in grid = total calculations for parallel division
  # give each core the task to sample and calculate sample statistics
  
  
  
  # Create 2 dimensional combination of iterations and samplesize, where each combination gets an id, this id also functions as seed.
  # Give id to parallel computing core
  for (size in seq(sample_range[1], sample_range[2], by = sample_steps)) {
    for (i in 1:n_iter) {
      sampled_data <- sample_data(data, size, perc_ones)
      model_names <- c("model_normal", "model_sparse", "model_interaction", "model_2interaction")
      
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
compare_models_10 <- function(data, scenario, perc_ones, n_iter = 50, sample_range = c(50, 100), sample_steps = 50, B = 40) {
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
      model_names <- c("model_normal", "model_sparse", "model_interaction", "model_2interaction")
      
      for (model_name in model_names) {
        result <- tryCatch({
          model_formula <- switch(model_name,
                                  model_normal = y ~ predictor1 + predictor2 + predictor3 + predictor4 + predictor5 + predictor6 + predictor7 + predictor8 + predictor9 + predictor10,
                                  model_sparse = y ~ predictor1 + predictor2 + predictor3 + predictor4 + predictor5 + predictor6 + predictor7 + predictor8 + predictor9,
                                  model_interaction = y ~ predictor1 + predictor2 + predictor3 + predictor4 + predictor5 + predictor6 + predictor7 + predictor8 + predictor9 + predictor1:predictor2,
                                  model_2interaction = y ~ predictor1 + predictor2 + predictor3 + predictor5 + predictor6 + predictor7 + predictor8 + predictor1:predictor2 + predictor2:predictor3)
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

matrix <- compare_models_05(data, perc_ones = 0.5)

plot_winning_models <- function(data) {
  # Step 1: Identify the winning model for each metric per iteration
  winning_models <- data %>%
    group_by(Iteration, SampleSize) %>%
    summarise(
      AIC_winner = Model[which.min(AIC)],
      BIC_winner = Model[which.min(BIC)],
      AUC_winner = Model[which.max(AUC)],
      AUC_val_winner = Model[which.max(AUC_val)]
    ) %>%
    ungroup()
  
  # Step 2: Calculate the percentage of iterations each model wins for each metric, grouped by SampleSize
  calculate_percentage <- function(winning_models, metric) {
    winning_models %>%
      count(SampleSize, !!sym(metric)) %>%
      group_by(SampleSize) %>%
      mutate(Percentage = n / sum(n) * 100) %>%
      ungroup() %>%
      select(SampleSize, Model = !!sym(metric), Percentage)
  }
  
  # Applying the function for each metric
  percentages_aic <- calculate_percentage(winning_models, "AIC_winner")
  percentages_bic <- calculate_percentage(winning_models, "BIC_winner")
  percentages_auc <- calculate_percentage(winning_models, "AUC_winner")
  percentages_auc_val <- calculate_percentage(winning_models, "AUC_val_winner")
  
  # Combining the results for easier visualization or analysis
  all_percentages <- bind_rows(
    percentages_aic %>% mutate(Metric = "AIC"),
    percentages_bic %>% mutate(Metric = "BIC"),
    percentages_auc %>% mutate(Metric = "AUC"),
    percentages_auc_val %>% mutate(Metric = "AUC_val")
  )
  
  # Plotting
  ggplot(all_percentages, aes(x = SampleSize, y = Percentage, color = Model)) +
    geom_line() +
    geom_point() +
    facet_wrap(~ Metric, scales = "free_y") +
    labs(title = "Winning Model Percentage by Sample Size and Metric",
         x = "Sample Size",
         y = "Winning Percentage (%)",
         color = "Model") +
    theme_minimal() +
    theme(legend.position = "bottom")
}
plot_winning_models_two <- function(data, model1, model2) {
  # Filter for the two specified models
  filtered_data <- data %>%
    filter(Model %in% c(model1, model2))

  # Step 1: Identify the winning model for each metric per iteration
  winning_models <- filtered_data %>%
    group_by(Iteration) %>%
    summarise(
      AIC_winner = Model[which.min(AIC)],
      BIC_winner = Model[which.min(BIC)],
      AUC_winner = Model[which.max(AUC)],
      AUC_val_winner = Model[which.max(AUC_val)]
    )
  
  # Step 2: Calculate the percentage of iterations each model wins for each metric, grouped by SampleSize
  calculate_percentage <- function(winning_models, metric) {
    winning_models %>%
      count(SampleSize, !!sym(metric)) %>%
      group_by(SampleSize) %>%
      summarise(Percentage = mean(!!sym(metric) == model1) * 100, .groups = 'drop')  # Compare only model1 wins
  }
  
  # Applying the function for each metric
  percentages_aic <- calculate_percentage(winning_models, "AIC_winner")
  percentages_bic <- calculate_percentage(winning_models, "BIC_winner")
  percentages_auc <- calculate_percentage(winning_models, "AUC_winner")
  percentages_auc_val <- calculate_percentage(winning_models, "AUC_val_winner")
  
  # Combining the results for easier visualization or analysis
  all_percentages <- bind_rows(
    percentages_aic %>% mutate(Metric = "AIC"),
    percentages_bic %>% mutate(Metric = "BIC"),
    percentages_auc %>% mutate(Metric = "AUC"),
    percentages_auc_val %>% mutate(Metric = "AUC_val")
  )
  
  # Plotting
  ggplot(all_percentages, aes(x = SampleSize, y = Percentage)) +
    geom_line() +
    geom_point() +
    facet_wrap(~ Metric, scales = "free_y") +
    labs(title = paste("Winning Percentage of", model1, "over", model2, "by Sample Size and Metric"),
         x = "Sample Size",
         y = "Winning Percentage (%)") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

plot_winning_models(matrix)
plot_winning_models_two(matrix, "model_normal", "model_sparse")


data <- matrix
model1 <- "model_sparse"
model2 <- "model_normal"
