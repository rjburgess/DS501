# server.R

# Install and load necessary packages
#pkgs <- c("caTools", "class", "ggplot2", "dplyr", "e1071", "rpart", "randomForest")
#for (pkg in pkgs) {
 # if (!requireNamespace(pkg, quietly = TRUE)) {
 #   install.packages(pkg)
#  }
#  library(pkg, character.only = TRUE)
#}

library(caTools)
library(class)
library(ggplot2)
library(dplyr)
library(e1071)
library(rpart)
library(randomForest)

shinyServer(function(input, output) {
  
  dataset <- reactive({
    df <- read.csv("Social_Network_Ads.csv")
    df_processed <- df[3:5]
    df_processed$Purchased <- factor(df_processed$Purchased, levels = c(0, 1))
    colnames(df_processed)[1:2] <- c("Age", "EstimatedSalary")
    return(df_processed)
  })
  
  output$rawDataOutput <- renderTable({
    dataset()
  })
  
  split_data <- reactive({
    df <- dataset()
    
    seed <- as.numeric(input$seed_value)
    if (is.na(seed)) {
      seed <- 123
    }
    set.seed(seed)
    
    split <- sample.split(df$Purchased, SplitRatio = input$split_ratio)
    
    training_set_orig <- subset(df, split == TRUE)
    test_set_orig <- subset(df, split == FALSE)
    
    center_params <- colMeans(training_set_orig[, c("Age", "EstimatedSalary")])
    scale_params <- apply(training_set_orig[, c("Age", "EstimatedSalary")], 2, sd)
    
    training_set_scaled <- training_set_orig
    training_set_scaled[, c("Age", "EstimatedSalary")] <- scale(training_set_orig[, c("Age", "EstimatedSalary")],
                                                                center = center_params,
                                                                scale = scale_params)
    
    test_set_scaled <- test_set_orig
    test_set_scaled[, c("Age", "EstimatedSalary")] <- scale(test_set_orig[, c("Age", "EstimatedSalary")],
                                                            center = center_params,
                                                            scale = scale_params)
    
    full_data_for_ranges <- dataset()
    # MODIFIED: Plotting ranges to match Python code's X_set min/max with +/- 10 and +/- 1000
    x_range_plot <- c(min(full_data_for_ranges$Age) - 10, max(full_data_for_ranges$Age) + 10)
    y_range_plot <- c(min(full_data_for_ranges$EstimatedSalary) - 1000, max(full_data_for_ranges$EstimatedSalary) + 1000)
    
    list(training_set_scaled = training_set_scaled,
         test_set_scaled = test_set_scaled,
         training_set_orig = training_set_orig,
         test_set_orig = test_set_orig,
         center_params = center_params,
         scale_params = scale_params,
         x_range_plot = x_range_plot,
         y_range_plot = y_range_plot)
  })
  
  observeEvent(input$calculate_seed_button, {
    seed_val <- as.numeric(input$seed_value)
    if (is.na(seed_val)) {
      output$calculated_seed_output <- renderText({ "Please enter a valid number for the seed." })
    } else {
      result <- seed_val * 5
      output$calculated_seed_output <- renderText({ paste("Seed value multiplied by 5:", result) })
    }
  })
  
  observeEvent(input$predict_new_button, {
    new_age <- input$new_age
    new_salary <- input$new_salary
    
    if (is.na(new_age) || is.na(new_salary) || new_age <= 0 || new_salary < 0) {
      output$new_prediction_output <- renderText("Please enter valid positive numbers for Age and Estimated Salary.")
      return()
    }
    
    data_sets <- split_data()
    model_info <- model_results() # Get all model info
    
    # Check if a model was successfully built or if it's KNN (which doesn't have a 'model_obj')
    if (is.null(model_info$model_obj) && model_info$algorithm != "KNN") {
      output$new_prediction_output <- renderText("Please train a model first or select an algorithm where fitting was successful.")
      return()
    }
    
    new_data_orig_df <- data.frame(Age = new_age, EstimatedSalary = new_salary)
    
    new_data_scaled_df <- new_data_orig_df
    new_data_scaled_df$Age <- (new_data_scaled_df$Age - data_sets$center_params["Age"]) / data_sets$scale_params["Age"]
    new_data_scaled_df$EstimatedSalary <- (new_data_scaled_df$EstimatedSalary - data_sets$center_params["EstimatedSalary"]) / data_sets$scale_params["EstimatedSalary"]
    
    predicted_class <- NULL
    prediction_reliability_msg <- ""
    
    if (model_info$algorithm == "KNN") {
      predicted_class <- knn(train = data_sets$training_set_scaled[, c("Age", "EstimatedSalary")],
                             test = new_data_scaled_df,
                             cl = data_sets$training_set_scaled$Purchased,
                             k = input$k_value)
      predicted_class <- factor(as.numeric(as.character(predicted_class)), levels = c(0, 1))
    } else {
      if (!is.null(model_info$model_status_message)) { # If model fitting had issues
        prediction_reliability_msg <- paste(" (Warning: Model had issues - '", model_info$model_status_message, "'. Prediction may be unreliable.)")
      }
      
      if (model_info$algorithm == "LogisticRegression") {
        # Robustness for Logistic Regression predictions for new data point
        # Ensure model_info$model_obj is not NULL before predicting
        if (!is.null(model_info$model_obj)) {
          prob <- predict(model_info$model_obj, newdata = new_data_scaled_df[, c("Age", "EstimatedSalary")], type = "response")
          prob[is.na(prob) | is.infinite(prob)] <- 0.5 # Handle NA/NaN/Inf probabilities
          predicted_class <- factor(ifelse(prob > 0.5, 1, 0), levels = c(0, 1))
        } else { # This means Logistic Regression failed and lr_model_temp was set to NULL
          predicted_class <- factor(0, levels = c(0, 1)) # Default to 0
          prediction_reliability_msg <- " (Warning: Logistic Regression model failed to fit. Defaulting to 'No Purchase'.)"
        }
        
      } else { # Other algorithms with proper model_obj
        predicted_class_raw <- predict(model_info$model_obj, newdata = new_data_scaled_df, type = "class")
        predicted_class <- factor(as.numeric(as.character(predicted_class_raw)), levels = c(0, 1))
      }
    }
    
    if (!is.null(predicted_class)) {
      output$new_prediction_output <- renderText({
        class_text <- if (as.character(predicted_class) == "1") {
          "will PURCHASE the product."
        } else {
          "will NOT PURCHASE the product."
        }
        paste0("Predicted: User with Age ", new_age, " and Salary ", new_salary, " ", class_text, prediction_reliability_msg)
      })
    } else {
      output$new_prediction_output <- renderText("Prediction could not be made. Ensure a model is selected and parameters are valid.")
    }
  })
  
  model_results <- reactive({
    req(input$algorithm_choice)
    data_sets <- split_data()
    training_set_scaled <- data_sets$training_set_scaled
    test_set_scaled <- data_sets$test_set_scaled
    
    current_algo <- input$algorithm_choice
    model_obj <- NULL
    model_status_message <- NULL # To provide feedback on model issues
    
    results <- switch(current_algo,
                      "KNN" = {
                        k_val <- input$k_value
                        y_pred_raw <- knn(train = training_set_scaled[, c("Age", "EstimatedSalary")],
                                          test = test_set_scaled[, c("Age", "EstimatedSalary")],
                                          cl = training_set_scaled$Purchased,
                                          k = k_val,
                                          prob = TRUE)
                        y_pred <- factor(as.numeric(as.character(y_pred_raw)), levels = c(0, 1))
                        list(y_pred = y_pred, model_obj = NULL, title_suffix = paste0('K-NN (k=', k_val, ')')) # KNN has no model_obj for predict
                      },
                      "SVM" = {
                        svm_model <- svm(Purchased ~ Age + EstimatedSalary,
                                         data = training_set_scaled,
                                         kernel = input$svm_kernel,
                                         cost = input$svm_cost,
                                         gamma = if(input$svm_kernel %in% c("radial", "polynomial", "sigmoid")) input$svm_gamma else 1/ncol(training_set_scaled[, c("Age", "EstimatedSalary")]),
                                         degree = if(input$svm_kernel == "polynomial") input$svm_degree else 3)
                        y_pred_raw <- predict(svm_model, newdata = test_set_scaled[, c("Age", "EstimatedSalary")])
                        y_pred <- factor(as.numeric(as.character(y_pred_raw)), levels = c(0, 1))
                        list(y_pred = y_pred, model_obj = svm_model, title_suffix = paste0('SVM (Kernel: ', input$svm_kernel, ', Cost: ', input$svm_cost, ')'))
                      },
                      "NaiveBayes" = {
                        nb_model <- naiveBayes(Purchased ~ Age + EstimatedSalary, data = training_set_scaled)
                        y_pred_raw <- predict(nb_model, newdata = test_set_scaled[, c("Age", "EstimatedSalary")], type = "class")
                        y_pred <- factor(as.numeric(as.character(y_pred_raw)), levels = c(0, 1))
                        list(y_pred = y_pred, model_obj = nb_model, title_suffix = 'Naive Bayes')
                      },
                      "DecisionTree" = {
                        dt_model <- rpart(Purchased ~ Age + EstimatedSalary, data = training_set_scaled,
                                          method = "class", control = rpart.control(cp = input$dt_cp))
                        y_pred_raw <- predict(dt_model, newdata = test_set_scaled[, c("Age", "EstimatedSalary")], type = "class")
                        y_pred <- factor(as.numeric(as.character(y_pred_raw)), levels = c(0, 1))
                        list(y_pred = y_pred, model_obj = dt_model, title_suffix = paste0('Decision Tree (cp: ', input$dt_cp, ')'))
                      },
                      "RandomForest" = {
                        rf_model <- randomForest(Purchased ~ Age + EstimatedSalary, data = training_set_scaled,
                                                 ntree = input$rf_ntree,
                                                 mtry = input$rf_mtry)
                        y_pred_raw <- predict(rf_model, newdata = test_set_scaled[, c("Age", "EstimatedSalary")], type = "class")
                        y_pred <- factor(as.numeric(as.character(y_pred_raw)), levels = c(0, 1))
                        list(y_pred = y_pred, model_obj = rf_model, title_suffix = paste0('Random Forest (Trees: ', input$rf_ntree, ', mtry: ', input$rf_mtry, ')'))
                      },
                      "LogisticRegression" = {
                        lr_model_temp <- NULL # Temporary holder for glm model
                        y_pred_temp <- factor(rep(0, nrow(test_set_scaled)), levels = c(0, 1)) # Default in case of severe error
                        
                        tryCatch({
                          # Attempt to fit the GLM model
                          lr_model_temp <- glm(Purchased ~ Age + EstimatedSalary, data = training_set_scaled, family = binomial)
                          
                          # Check for convergence issues or infinite coefficients (perfect separation)
                          if (!lr_model_temp$converged) {
                            model_status_message <- "Model did not converge."
                          }
                          if (any(is.infinite(coef(lr_model_temp))) || any(is.na(coef(lr_model_temp)))) {
                            if (is.null(model_status_message)) { # Only set if no other message
                              model_status_message <- "Perfect separation detected (infinite coefficients)."
                            } else {
                              model_status_message <- paste(model_status_message, " Perfect separation detected (infinite coefficients).")
                            }
                          }
                          
                          # Get predictions on the test set
                          prob_pred <- predict(lr_model_temp, newdata = test_set_scaled[, c("Age", "EstimatedSalary")], type = "response")
                          # Handle any NA/NaN/Inf in probabilities by setting them to 0.5 (will default to class 0)
                          prob_pred[is.na(prob_pred) | is.infinite(prob_pred)] <- 0.5
                          y_pred_temp <- factor(ifelse(prob_pred > 0.5, 1, 0), levels = c(0, 1))
                          
                        }, error = function(e) {
                          # Catch any fatal errors during glm fitting
                          model_status_message <- paste("Error during fitting:", e$message)
                          lr_model_temp <- NULL # Set model_obj to NULL if fitting completely fails
                        })
                        
                        list(y_pred = y_pred_temp,
                             model_obj = lr_model_temp, # Pass the model, even if it has problems (unless fatal error)
                             title_suffix = 'Logistic Regression',
                             model_status_message = model_status_message)
                      },
                      {
                        list(y_pred = factor(), cm = matrix(), model_obj = NULL, title_suffix = "N/A", model_status_message = NULL)
                      }
    )
    
    # Ensure y_pred only contains 0 or 1 levels for table
    cm <- table(test_set_scaled$Purchased, results$y_pred)
    
    list(y_pred = results$y_pred,
         cm = cm,
         model_obj = results$model_obj,
         algorithm = current_algo,
         title_suffix = results$title_suffix,
         model_status_message = results$model_status_message) # Pass message out
  })
  
  output$confusionMatrixPlot <- renderPlot({
    req(model_results())
    cm_table <- model_results()$cm
    
    cm_df <- as.data.frame(cm_table)
    names(cm_df) <- c("Actual", "Predicted", "Count")
    
    ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Count)) +
      geom_tile(color = "white", linewidth = 1) +
      geom_text(aes(label = Count), color = "black", size = 6) +
      scale_fill_gradient(low = "#e0f2f7", high = "#007ea7", name = "Count") +
      labs(title = "Confusion Matrix",
           x = "Predicted Class",
           y = "Actual Class") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0))
  })
  
  # New output for model status messages
  output$model_status_text <- renderText({
    results <- model_results()
    if (!is.null(results$model_status_message)) {
      paste("Model Status:", results$model_status_message)
    } else {
      "" # No message if model is fine
    }
  })
  
  
  generate_plot_data <- function(plot_set_orig, training_set_scaled, center_params, scale_params,
                                 full_data_orig_x_range, full_data_orig_y_range,
                                 algorithm_type, model_object, k_val_knn) {
    
    # MODIFIED: Step sizes for grid to match Python code's request.
    # Note: Python's step=0.25 for EstimatedSalary range (e.g., 100000) creates
    # an extremely dense grid (millions of points) which is computationally
    # expensive and can crash R Shiny. A practical step of 100 for Estimated Salary
    # is used to keep the app responsive, but Age's 0.25 is matched.
    X1_range_orig <- seq(full_data_orig_x_range[1], full_data_orig_x_range[2], by = 0.25) # Matched Python's 0.25
    X2_range_orig <- seq(full_data_orig_y_range[1], full_data_orig_y_range[2], by = 100)  # Practical step for Salary
    
    grid_set_orig <- expand.grid(Age = X1_range_orig, EstimatedSalary = X2_range_orig)
    
    grid_set_scaled <- grid_set_orig
    grid_set_scaled$Age <- (grid_set_scaled$Age - center_params["Age"]) / scale_params["Age"]
    grid_set_scaled$EstimatedSalary <- (grid_set_scaled$EstimatedSalary - center_params["EstimatedSalary"]) / scale_params["EstimatedSalary"]
    
    y_grid <- switch(algorithm_type,
                     "KNN" = {
                       knn_pred <- knn(train = training_set_scaled[, c("Age", "EstimatedSalary")],
                                       test = grid_set_scaled,
                                       cl = training_set_scaled$Purchased,
                                       k = k_val_knn)
                       factor(as.numeric(as.character(knn_pred)), levels = c(0, 1)) # Explicitly convert to 0/1 factor
                     },
                     "SVM" = {
                       svm_pred <- predict(model_object, newdata = grid_set_scaled)
                       factor(as.numeric(as.character(svm_pred)), levels = c(0, 1)) # Explicitly convert to 0/1 factor
                     },
                     "NaiveBayes" = {
                       nb_pred <- predict(model_object, newdata = grid_set_scaled, type = "class")
                       factor(as.numeric(as.character(nb_pred)), levels = c(0, 1)) # Explicitly convert to 0/1 factor
                     },
                     "DecisionTree" = {
                       dt_pred <- predict(model_object, newdata = grid_set_scaled, type = "class")
                       factor(as.numeric(as.character(dt_pred)), levels = c(0, 1)) # Explicitly convert to 0/1 factor
                     },
                     "RandomForest" = {
                       rf_pred <- predict(model_object, newdata = grid_set_scaled, type = "class")
                       factor(as.numeric(as.character(rf_pred)), levels = c(0, 1)) # Explicitly convert to 0/1 factor
                     },
                     "LogisticRegression" = {
                       if (is.null(model_object)) { # If model fitting failed completely (e.g., error in glm)
                         factor(rep(0, nrow(grid_set_scaled)), levels = c(0,1)) # Default all red
                       } else {
                         prob_grid <- predict(model_object, newdata = grid_set_scaled, type = "response")
                         
                         # Visualization hack for Logistic Regression with perfect separation:
                         # If probabilities are pushed to extreme 0 or 1, slightly clamp them
                         # to allow a visual boundary to appear on the plot.
                         prob_grid[prob_grid < 1e-6] <- 1e-6
                         prob_grid[prob_grid > (1 - 1e-6)] <- 1 - 1e-6
                         
                         # Ensure no NAs/Infs remain after clamping
                         prob_grid[is.na(prob_grid) | is.infinite(prob_grid)] <- 0.5
                         factor(ifelse(prob_grid > 0.5, 1, 0), levels = c(0, 1))
                       }
                     },
                     {
                       factor(rep(0, nrow(grid_set_orig)), levels = c(0,1))
                     }
    )
    
    # FINAL FIX: Directly assign y_grid as it is already a factor with levels 0 and 1
    grid_set_orig$Prediction <- y_grid
    
    original_points <- plot_set_orig
    colnames(original_points) <- c("Age", "EstimatedSalary", 'ActualClass')
    
    list(grid_data = grid_set_orig,
         original_points = original_points)
  }
  
  output$trainingPlot <- renderPlot({
    results <- model_results()
    data_sets <- split_data()
    
    plot_data <- generate_plot_data(plot_set_orig = data_sets$training_set_orig,
                                    training_set_scaled = data_sets$training_set_scaled,
                                    center_params = data_sets$center_params,
                                    scale_params = data_sets$scale_params,
                                    full_data_orig_x_range = data_sets$x_range_plot,
                                    full_data_orig_y_range = data_sets$y_range_plot,
                                    algorithm_type = results$algorithm,
                                    model_object = results$model_obj,
                                    k_val_knn = input$k_value)
    
    ggplot() +
      geom_tile(data = plot_data$grid_data,
                aes(x = Age, y = EstimatedSalary, fill = Prediction),
                alpha = 1) +
      geom_point(data = plot_data$original_points,
                 aes(x = Age, y = EstimatedSalary, color = ActualClass),
                 size = 3, shape = 19) +
      scale_fill_manual(values = c("0" = "red", "1" = "green"),
                        name = "Predicted Class",
                        na.translate = FALSE) + # Add na.translate = FALSE
      scale_color_manual(values = c("0" = "red", "1" = "green"),
                         name = "Actual Class") +
      labs(title = paste(results$title_suffix, ' (Training Set, Seed:', input$seed_value, ')'),
           x = 'Age',
           y = 'Estimated Salary') +
      theme_minimal() +
      coord_cartesian(xlim = data_sets$x_range_plot,
                      ylim = data_sets$y_range_plot)
  })
  
  output$testPlot <- renderPlot({
    results <- model_results()
    data_sets <- split_data()
    
    plot_data <- generate_plot_data(plot_set_orig = data_sets$test_set_orig,
                                    training_set_scaled = data_sets$training_set_scaled,
                                    center_params = data_sets$center_params,
                                    scale_params = data_sets$scale_params,
                                    full_data_orig_x_range = data_sets$x_range_plot,
                                    full_data_orig_y_range = data_sets$y_range_plot,
                                    algorithm_type = results$algorithm,
                                    model_object = results$model_obj,
                                    k_val_knn = input$k_value)
    
    ggplot() +
      geom_tile(data = plot_data$grid_data,
                aes(x = Age, y = EstimatedSalary, fill = Prediction),
                alpha = 1) +
      geom_point(data = plot_data$original_points,
                 aes(x = Age, y = EstimatedSalary, color = ActualClass),
                 size = 3, shape = 19) +
      scale_fill_manual(values = c("0" = "red", "1" = "green"),
                        name = "Predicted Class",
                        na.translate = FALSE) + # Add na.translate = FALSE
      scale_color_manual(values = c("0" = "red", "1" = "green"),
                         name = "Actual Class") +
      labs(title = paste(results$title_suffix, ' (Test Set, Seed:', input$seed_value, ')'),
           x = 'Age',
           y = 'Estimated Salary') +
      theme_minimal() +
      coord_cartesian(xlim = data_sets$x_range_plot,
                      ylim = data_sets$y_range_plot)
  })
})
