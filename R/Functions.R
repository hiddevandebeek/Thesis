library(ggplot2)
library(caTools)
library(plotly)
library(MASS)
library(matlib)
library(pROC)
library(parallel)
library(CalibrationCurves)
library(caret)
library(tidyverse)
library(rms)

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


compare_models_parallel <- function(data, n_iter, samplesize, perc_ones, B, model1_formula, model2_formula) {
  # Function to be run in each parallel process
  model1_formula <- as.formula(model1_formula)
  model2_formula <- as.formula(model2_formula)
  
  # function
  run_iteration <- function(i) {
    nonconvergence_counter <- 0
    sampled_data <- sample_data(data, samplesize, perc_ones)
    sampled_data$y <- factor(sampled_data$y, levels = c(0, 1))
    
    # Function to fit model and handle errors/warnings
    fit_model <- function(formula, data) {
      tryCatch({
        fitted_model <- lrm(formula, data = data, x = TRUE, y = TRUE)
        # Check if the model is valid, add your own checks here
        if (is.null(fitted_model) || !("lrm" %in% class(fitted_model))) {
          nonconvergence_counter <<- nonconvergence_counter + 1
          return(NULL)
        } else {
          return(fitted_model)
        }
      }, error = function(e) {
        nonconvergence_counter <<- nonconvergence_counter + 1
        return(NULL)
      }, warning = function(w) {
        nonconvergence_counter <<- nonconvergence_counter + 1
        return(NULL)
      })
    }
    
    # Fit Model 1
    model1 <- fit_model(model1_formula, sampled_data)
    
    # Proceed with Model 2 only if Model 1 was successful
    if (!is.null(model1)) {
      model2 <- fit_model(model2_formula, sampled_data)
    } else {
      model2 <- NULL
    }
    
    # Validate models if they are successfully fitted
    model1.validate <- if (!is.null(model1)) rms::validate(model1, method = "boot", B = B) else NULL
    model2.validate <- if (!is.null(model2)) rms::validate(model2, method = "boot", B = B) else NULL
    
    # Compute winners only if both models are valid
    if (!is.null(model1) && !is.null(model2)) {
      aic_winner <- ifelse(AIC(model1) < AIC(model2), 1, 2)
      bic_winner <- ifelse(BIC(model1) < BIC(model2), 1, 2)
      auc_winner <- ifelse(model1$stats[6] > model2$stats[6], 1, 2)
      aucvalidated_winner <- ifelse((0.5 * model1.validate[1, 5] + 0.5) > (0.5 * model2.validate[1, 5] + 0.5), 1, 2)
    } else {
      aic_winner <- NA
      bic_winner <- NA
      auc_winner <- NA
      aucvalidated_winner <- NA
    }
    
    return(c(aic_winner, bic_winner, auc_winner, aucvalidated_winner, nonconvergence_counter))
  }
  
  # Determine the number of cores and split the work
  no_cores <- detectCores() - 3  # Optimal number of cores
  results <- mclapply(1:n_iter, run_iteration, mc.cores = no_cores)
  
  # Process results
  results_matrix <- matrix(unlist(results), ncol = 5, byrow = TRUE)
  model1_aic_count <- sum(results_matrix[, 1] == 1, na.rm = TRUE)
  model1_bic_count <- sum(results_matrix[, 2] == 1, na.rm = TRUE)
  model1_auc_count <- sum(results_matrix[, 3] == 1, na.rm = TRUE)
  model1_aucvalidated_count <- sum(results_matrix[, 4] == 1, na.rm = TRUE)
  nonconvergence_count <- sum(results_matrix[, 5] == 1, na.rm = TRUE)
  
  # Calculate percentages
  calculate_percentage <- function(count) (count / (n_iter - nonconvergence_count)) * 100
  model1_aic_percent <- calculate_percentage(model1_aic_count)
  model1_bic_percent <- calculate_percentage(model1_bic_count)
  model1_auc_percent <- calculate_percentage(model1_auc_count)
  model1_aucvalidated_percent <- calculate_percentage(model1_aucvalidated_count)
  model2_aic_percent <- 100 - model1_aic_percent
  model2_bic_percent <- 100 - model1_bic_percent
  model2_auc_percent <- 100 - model1_auc_percent
  model2_aucvalidated_percent <- 100 - model1_aucvalidated_percent
  nonconvergence_percent <- (nonconvergence_count / n_iter) * 100
  
  # Create and return a table of the results
  results_table <- data.frame(
    Model = c("Model 1", "Model 2"),
    AIC_Percentage = c(model1_aic_percent, model2_aic_percent),
    BIC_Percentage = c(model1_bic_percent, model2_bic_percent),
    AUC_Percentage = c(model1_auc_percent, model2_auc_percent),
    AUCValidated_Percentage = c(model1_aucvalidated_percent, model2_aucvalidated_percent),
    nonconvergence_percent = c(nonconvergence_percent)
  )
  return(results_table)
}

create_plot <- function(results_list) {
  # Combine all results into a single data frame
  combined_results <- bind_rows(results_list, .id = "Sample_Size")
  
  # Convert from wide to long format
  combined_results_long <- combined_results %>%
    mutate(Sample_Size = as.numeric(gsub("compare.*\\.", "", Sample_Size))) %>%
    pivot_longer(cols = c("AIC_Percentage", "BIC_Percentage", "AUC_Percentage", "AUCValidated_Percentage", "nonconvergence_percent"), names_to = "Criterion", values_to = "Percentage") %>%
    filter(Model == "Model 1") %>%
    mutate(Criterion = factor(Criterion, levels = c("AIC_Percentage", "BIC_Percentage", "AUC_Percentage", "AUCValidated_Percentage", "nonconvergence_percent"), labels = c("AIC", "BIC", "AUC", "AUC with optimism correction", "Nonconvergence")))
  
  # Create a minimalistic plot
  plot_minimalistic <- ggplot(combined_results_long, aes(x = Sample_Size, y = Percentage, color = Criterion)) +
    geom_line() +  # Default line width
    labs(x = "Sample Size",
         y = "Percentage (%)") +
    scale_y_continuous(limits = c(0, 100)) +
    scale_x_continuous(limits = c(0, 1000)) +
    scale_color_brewer(palette = "Set1") +
    theme_apa(box = TRUE)
  return(plot_minimalistic)
}
