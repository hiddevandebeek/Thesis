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


create_visualization <- function(dataframe, scenario, n_iter = c(50,2000)){
  
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
calculate_linear_predictor10 <- function(a, data, scenario) {
  if (scenario == "normal") {
    return(a * (data$predictor1 + data$predictor2 + data$predictor3 + data$predictor4 + 0.5*data$predictor5 + data$predictor6 + data$predictor7 + data$predictor8 + data$predictor9 + data$predictor10))
  } else if (scenario == "interaction") {
    return(a * (data$predictor1 + data$predictor2 + data$predictor3 + data$predictor4 + data$predictor5 + data$predictor6 + data$predictor7 + data$predictor8 + data$predictor9 + 0.5*data$predictor1*data$predictor2))
  } else if (scenario == "2 normal") {
    return(a * (data$predictor1 + data$predictor2 + data$predictor3 + 0.5*data$predictor4 + 0.5*data$predictor5 + data$predictor6 + data$predictor7 + data$predictor8 + data$predictor9 + data$predictor10))
  } else if (scenario == "1 interaction & 1 normal") {
    return(a * (data$predictor1 + data$predictor2 + data$predictor3 + 0.5*data$predictor4 + data$predictor5 + data$predictor6 + data$predictor7 + data$predictor8 + 0.5*data$predictor1*data$predictor2 + data$predictor9))
  } else if (scenario == "2 interaction") {
    return(a * (data$predictor1 + data$predictor2 + data$predictor3 + data$predictor4 + data$predictor5 + data$predictor6 + data$predictor7 + data$predictor8 + 0.5*data$predictor1*data$predictor2 + 0.5*data$predictor2*data$predictor3))
  }
}
calculate_linear_predictor20 <- function(a, data, scenario) {
  if (scenario == "normal") {
    return(a * (data$predictor1 + data$predictor2 + data$predictor3 + data$predictor4 + 0.5*data$predictor5 + data$predictor6 + data$predictor7 + data$predictor8 + data$predictor9 + data$predictor10 + data$predictor11 + data$predictor12 + data$predictor13 + data$predictor14 + data$predictor15 + data$predictor16 + data$predictor17 + data$predictor18 + data$predictor19 + data$predictor20))
  } else if (scenario == "interaction") {
    return(a * (data$predictor1 + data$predictor2 + data$predictor3 + data$predictor4 + data$predictor5 + data$predictor6 + data$predictor7 + data$predictor8 + data$predictor9 + data$predictor10 + data$predictor11 + data$predictor12 + data$predictor13 + data$predictor14 + data$predictor15 + data$predictor16 + data$predictor17 + data$predictor18 + data$predictor19 + 0.5*data$predictor1*data$predictor2))
  } else if (scenario == "2 normal") {
    return(a * (data$predictor1 + data$predictor2 + data$predictor3 + 0.5*data$predictor4 + 0.5*data$predictor5 + data$predictor6 + data$predictor7 + data$predictor8 + data$predictor9 + data$predictor10 + data$predictor11 + data$predictor12 + data$predictor13 + data$predictor14 + data$predictor15 + data$predictor16 + data$predictor17 + data$predictor18 + data$predictor19 + data$predictor20))
  } else if (scenario == "1 interaction & 1 normal") {
    return(a * (data$predictor1 + data$predictor2 + data$predictor3 + 0.5*data$predictor4 + data$predictor5 + data$predictor6 + data$predictor7 + data$predictor8 + data$predictor9 + data$predictor10 + data$predictor11 + data$predictor12 + data$predictor13 + data$predictor14 + data$predictor15 + data$predictor16 + data$predictor17 + data$predictor18 + 0.5*data$predictor1*data$predictor2 + data$predictor19))
  } else if (scenario == "2 interaction") {
    return(a * (data$predictor1 + data$predictor2 + data$predictor3 + data$predictor4 + data$predictor5 + data$predictor6 + data$predictor7 + data$predictor8 + data$predictor9 + data$predictor10 + data$predictor11 + data$predictor12 + data$predictor13 + data$predictor14 + data$predictor15 + data$predictor16 + data$predictor17 + data$predictor18 + 0.5*data$predictor1*data$predictor2 + 0.5*data$predictor2*data$predictor3)) 
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
calculate_and_plot_auc10 <- function(plot_title) {
  scenario <- tolower(unlist(strsplit(plot_title, ":"))[2])
  scenario <- gsub("^[[:space:]]+", "", scenario) # Remove leading spaces
  
  # Parallel setup
  no_cores <- detectCores() - 1  # Leave one core free for system processes
  cl <- makeCluster(no_cores)
  clusterExport(cl, c("calculate_linear_predictor10", "data_experiment", "n", "plogis", "glm", "predict", "colAUC", "binomial"))
  clusterEvalQ(cl, library(pROC))
  
  # Splitting a_values for parallel computation
  a_values_split <- split(a_values, cut(a_values, no_cores))
  
  # Parallel computation
  auc_results <- parLapply(cl, a_values_split, function(a_chunk) {
    auc_chunk <- numeric(length(a_chunk))
    for (i in seq_along(a_chunk)) {
      a <- a_chunk[i]
      
      # Calculate L based on the DGM (data generating mechanism)
      L <- calculate_linear_predictor10(a, data_experiment, scenario)
      L <- -mean(L) + L
      
      # Generate data based on the DGM
      y <- ifelse(runif(n) < plogis(L), 1, 0)
      
      data_experiment$y <- y
      
      model_normal <- "y ~ predictor1 + predictor2 + predictor3 + predictor4 + predictor5 + predictor6 + predictor7 + predictor8 + predictor9 + predictor10"
      model_interaction <- "y ~ predictor1 + predictor2 + predictor3 + predictor4 + predictor5 + predictor6 + predictor7 + predictor8 + predictor9 + predictor1:predictor2"
      model_2interaction <- "y ~ predictor1 + predictor2 + predictor3 + predictor4 + predictor5 + predictor6 + predictor7 + predictor8 + predictor1:predictor2 + predictor2:predictor3"
      
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
  
  # Create the plot
  plot <- create_auc_plot(a_values, auc_values, plot_title)
  plot <- plot +
    geom_point(data = result_table, aes(x = Coefficient, y = AUC), color = "red", size = 3)
  
  return(list(values = result_table, plot = plot))
}
calculate_and_plot_auc20 <- function(plot_title) {
  scenario <- tolower(unlist(strsplit(plot_title, ":"))[2])
  scenario <- gsub("^[[:space:]]+", "", scenario) # Remove leading spaces
  
  # Parallel setup
  no_cores <- detectCores() - 1  # Leave one core free for system processes
  cl <- makeCluster(no_cores)
  clusterExport(cl, c("calculate_linear_predictor10", "data_experiment", "n", "plogis", "glm", "predict", "colAUC", "binomial"))
  clusterEvalQ(cl, library(pROC))
  
  # Splitting a_values for parallel computation
  a_values_split <- split(a_values, cut(a_values, no_cores))
  
  # Parallel computation
  auc_results <- parLapply(cl, a_values_split, function(a_chunk) {
    auc_chunk <- numeric(length(a_chunk))
    for (i in seq_along(a_chunk)) {
      a <- a_chunk[i]
      
      # Calculate L based on the DGM (data generating mechanism)
      L <- calculate_linear_predictor10(a, data_experiment, scenario)
      L <- -mean(L) + L
      
      # Generate data based on the DGM
      y <- ifelse(runif(n) < plogis(L), 1, 0)
      
      data_experiment$y <- y
    
      model_normal <- "y ~ predictor1 + predictor2 + predictor3 + predictor4 + predictor5 + predictor6 + predictor7 + predictor8 + predictor9 + predictor10 + predictor11 + predictor12 + predictor13 + predictor14 + predictor15 + predictor16 + predictor17 + predictor18 + predictor19 + predictor20"
      model_interaction <- "y ~ predictor1 + predictor2 + predictor3 + predictor4 + predictor5 + predictor6 + predictor7 + predictor8 + predictor9 + predictor10 + predictor11 + predictor12 + predictor13 + predictor14 + predictor15 + predictor16 + predictor17 + predictor18 + predictor19 + predictor1:predictor2"
      model_2interaction <- "y ~ predictor1 + predictor2 + predictor3 + predictor4 + predictor5 + predictor6 + predictor7 + predictor8 + predictor9 + predictor10 + predictor11 + predictor12 + predictor13 + predictor14 + predictor15 + predictor16 + predictor17 + predictor18 + predictor1:predictor2 + predictor2:predictor3"
      
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
  
  # Create the plot
  plot <- create_auc_plot(a_values, auc_values, plot_title)
  plot <- plot +
    geom_point(data = result_table, aes(x = Coefficient, y = AUC), color = "red", size = 3)
  
  return(list(values = result_table, plot = plot))
}

