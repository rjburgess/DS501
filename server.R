# This is the main server.R file for the app I am building

# I commented these out when I upload to shiny. I was getting an error and I 
# thought that it was because these are already install on shainy.io

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
  
# function is reactive so I automatically re-executes
# I store the csv in the same directory as the ui.R and server.R
  dataset <- reactive({
    df <- read.csv("Social_Network_Ads.csv")
    # I get rid of the first 2 columns (user id and gender)
    # I guess later I can use gender as a variable if I want
    new_df <- df[3:5]
    # I need to make purcharse the category value
    new_df$Purchased <- factor(new_df$Purchased, levels = c(0, 1))
    #colnames(new_df)[1:2] <- c("Age", "EstimatedSalary")
    return(new_df)
  })
  
  # this code outputs the columns from the Social_Network_Ads above
  # I comment out the code that I could have used that displays all the columns (id and gender)
  #dataset1 <- reactive({
  #  df <- read.csv("Social_Network_Ads.csv")
  #})
  
  output$rawDataOutput <- renderTable({
    #dataset1()
    dataset()
  })
  
  # this code splits dataset into training and test based on the split_ratio
  split_data <- reactive({
    df <- dataset()
    
    # set seed for smae results each time
    
    seed <- as.numeric(input$seed_value)
    set.seed(seed)
    
    split <- sample.split(df$Purchased, SplitRatio = input$split_ratio)
    
    training_set_split <- subset(df, split == TRUE)
    test_set_split <- subset(df, split == FALSE)
    
    # find mean for future feature scaling
    
    center_params <- colMeans(training_set_split[, c("Age", "EstimatedSalary")])
    
    # find sd for future feature scaling
    
    scale_params <- apply(training_set_split[, c("Age", "EstimatedSalary")], 2, sd)
    
    #use the scale funtion for feature scaling
    
    training_set_scaled <- training_set_split
    training_set_scaled[, c("Age", "EstimatedSalary")] <- scale(training_set_split[, c("Age", "EstimatedSalary")],
                                                                center = center_params,
                                                                scale = scale_params)
    
    test_set_scaled <- test_set_split
    test_set_scaled[, c("Age", "EstimatedSalary")] <- scale(test_set_split[, c("Age", "EstimatedSalary")],
                                                            center = center_params,
                                                            scale = scale_params)
    
    # Previously the graphs were plotting the scale features. I needed to so the plot would have the correct ranges of the real input csv data
    plot_range <- dataset()
    x_range_plot <- c(min(plot_range$Age) - 10, max(plot_range$Age) + 10)
    y_range_plot <- c(min(plot_range$EstimatedSalary) - 1000, max(plot_range$EstimatedSalary) + 1000)
    
    # This create a list of all parameters created so they can be access later if needed
    list(training_set_scaled = training_set_scaled,
         test_set_scaled = test_set_scaled,
         training_set_split = training_set_split,
         test_set_split = test_set_split,
         center_params = center_params,
         scale_params = scale_params,
         x_range_plot = x_range_plot,
         y_range_plot = y_range_plot)
  })
  
  # this the code to run inference 
  observeEvent(input$run_inference, {
    new_age <- input$inf_age
    new_salary <- input$inf_salary
    
    if (is.na(new_age) || is.na(new_salary) || new_age <= 0 || new_salary < 0) {
      output$inf_prediction_output <- renderText("Age and Salary must be greater than Zero - Redo")
      return()
    }
    
    # I need the data sets and the model info to run infernce
    data_sets <- split_data()
    model_info <- model_results() 
    
    # For some reason KNN is lazy and does not build a model so I need to call the training data for inferencing, 
    if (is.null(model_info$model_obj) && model_info$algorithm != "KNN") {
      output$inf_prediction_output <- renderText("Wait until model is trained before hitting the inference button")
      return()
    }
    
    # create the dataframe for inferencing 
    new_data_orig_df <- data.frame(Age = new_age, EstimatedSalary = new_salary)
    
    # do feature scaling on the data like we did in training
    new_data_scaled_df <- new_data_orig_df
    new_data_scaled_df$Age <- (new_data_scaled_df$Age - data_sets$center_params["Age"]) / data_sets$scale_params["Age"]
    new_data_scaled_df$EstimatedSalary <- (new_data_scaled_df$EstimatedSalary - data_sets$center_params["EstimatedSalary"]) / data_sets$scale_params["EstimatedSalary"]
    
    #predicted_class <- NULL
    prediction_reliability_msg <- ""
    
    # Need to run KNN again
    
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
        if (!is.null(model_info$model_obj)) {
          prob <- predict(model_info$model_obj, newdata = new_data_scaled_df[, c("Age", "EstimatedSalary")], type = "response")
          prob[is.na(prob) | is.infinite(prob)] <- 0.5 # 
          predicted_class <- factor(ifelse(prob > 0.5, 1, 0), levels = c(0, 1))
        } else { 
          predicted_class <- factor(0, levels = c(0, 1))
          prediction_reliability_msg <- "Logistic Regression Failed)"
        }
        
        # this is for everything beside KNN and LR
      } else { 
        predicted_class_raw <- predict(model_info$model_obj, newdata = new_data_scaled_df, type = "class")
        predicted_class <- factor(as.numeric(as.character(predicted_class_raw)), levels = c(0, 1))
      }
    }
    
    if (!is.null(predicted_class)) {
      output$inf_prediction_output <- renderText({
        class_text <- if (as.character(predicted_class) == "1") {
          "will PURCHASE the product."
        } else {
          "will NOT PURCHASE the product."
        }
        paste0("User Age ", new_age, " and Salary ", new_salary, " ", class_text, prediction_reliability_msg)
      })
    } else {
      output$inf_prediction_output <- renderText("Prediction Invalid.")
    }
  })
  
  # this will train the model as inputs are changed
  model_results <- reactive({
    req(input$algorithm_choice)
    data_sets <- split_data()
    training_set_scaled <- data_sets$training_set_scaled
    test_set_scaled <- data_sets$test_set_scaled
    
    current_algo <- input$algorithm_choice
    model_obj <- NULL
    model_status_message <- NULL # I will print this message to the screen for each model
    
    results <- switch(current_algo,
                      "KNN" = {
                        k_val <- input$k_value
                        y_pred_raw <- knn(train = training_set_scaled[, c("Age", "EstimatedSalary")],
                                          test = test_set_scaled[, c("Age", "EstimatedSalary")],
                                          cl = training_set_scaled$Purchased,
                                          k = k_val,
                                          prob = TRUE)
                        y_pred <- factor(as.numeric(as.character(y_pred_raw)), levels = c(0, 1))
                        list(y_pred = y_pred, model_obj = NULL, title_suffix = paste0('K-NN (k=', k_val, ')')) 
                      },
                      "SVM" = {
                        svm_model <- svm(Purchased ~ Age + EstimatedSalary,
                                         data = training_set_scaled,
                                         kernel = input$svm_kernel,
                                         cost = input$svm_cost,
                                         gamma = if(input$svm_kernel %in% c("radial", "polynomial")) input$svm_gamma else 1/ncol(training_set_scaled[, c("Age", "EstimatedSalary")]),
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
                                          method = "class") 
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
                        lr_model_temp <- NULL 
                        y_pred_temp <- factor(rep(0, nrow(test_set_scaled)), levels = c(0, 1)) # Default in case of severe error
                        
                        tryCatch({
                          
                          lr_model_temp <- glm(Purchased ~ Age + EstimatedSalary, data = training_set_scaled, family = binomial)
                          
                          
                          if (!lr_model_temp$converged) {
                            model_status_message <- "Model did not converge."
                          }
                          if (any(is.infinite(coef(lr_model_temp))) || any(is.na(coef(lr_model_temp)))) {
                            if (is.null(model_status_message)) { 
                              model_status_message <- "Perfect separation detected (infinite coefficients)."
                            } else {
                              model_status_message <- paste(model_status_message, " Perfect separation detected (infinite coefficients).")
                            }
                          }
                          
                          # Get predictions on the test set
                          prob_pred <- predict(lr_model_temp, newdata = test_set_scaled[, c("Age", "EstimatedSalary")], type = "response")
                          prob_pred[is.na(prob_pred) | is.infinite(prob_pred)] <- 0.5
                          y_pred_temp <- factor(ifelse(prob_pred > 0.5, 1, 0), levels = c(0, 1))
                          
                        }, error = function(e) {
                          model_status_message <- paste("Error during fitting:", e$message)
                          lr_model_temp <- NULL # Set model_obj to NULL if fitting completely fails
                        })
                        
                        list(y_pred = y_pred_temp,
                             model_obj = lr_model_temp, 
                             title_suffix = 'Logistic Regression',
                             model_status_message = model_status_message)
                      },
                      {
                        list(y_pred = factor(), cm = matrix(), model_obj = NULL, title_suffix = "N/A", model_status_message = NULL)
                      }
    )
    

    cm <- table(test_set_scaled$Purchased, results$y_pred)
    
    list(y_pred = results$y_pred,
         cm = cm,
         model_obj = results$model_obj,
         algorithm = current_algo,
         title_suffix = results$title_suffix,
         model_status_message = results$model_status_message) # Pass message out
  })
  
  # this is the code for the confusion Matrix
  
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
      "FIND ISSUE WHY no message" 
    }
  })
  
  # This is the code to print out the Training Set / Test Cut / and Conf Matrix
  generate_plot_data <- function(plot_set_orig, training_set_scaled, center_params, scale_params,
                                 full_data_orig_x_range, full_data_orig_y_range,
                                 algorithm_type, model_object, k_val_knn) {
    

    X1_range_orig <- seq(full_data_orig_x_range[1], full_data_orig_x_range[2], by = 0.25) 
    X2_range_orig <- seq(full_data_orig_y_range[1], full_data_orig_y_range[2], by = 100) 
    
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
                       factor(as.numeric(as.character(knn_pred)), levels = c(0, 1)) 
                     },
                     "SVM" = {
                       svm_pred <- predict(model_object, newdata = grid_set_scaled)
                       factor(as.numeric(as.character(svm_pred)), levels = c(0, 1)) 
                     },
                     "NaiveBayes" = {
                       nb_pred <- predict(model_object, newdata = grid_set_scaled, type = "class")
                       factor(as.numeric(as.character(nb_pred)), levels = c(0, 1)) 
                     },
                     "DecisionTree" = {
                       dt_pred <- predict(model_object, newdata = grid_set_scaled, type = "class")
                       factor(as.numeric(as.character(dt_pred)), levels = c(0, 1)) 
                     },
                     "RandomForest" = {
                       rf_pred <- predict(model_object, newdata = grid_set_scaled, type = "class")
                       factor(as.numeric(as.character(rf_pred)), levels = c(0, 1)) 
                     },
                     "LogisticRegression" = {
                       if (is.null(model_object)) { 
                         factor(rep(0, nrow(grid_set_scaled)), levels = c(0,1)) 
                       } else {
                         prob_grid <- predict(model_object, newdata = grid_set_scaled, type = "response")
                         

                         prob_grid[prob_grid < 1e-6] <- 1e-6
                         prob_grid[prob_grid > (1 - 1e-6)] <- 1 - 1e-6
                         

                         prob_grid[is.na(prob_grid) | is.infinite(prob_grid)] <- 0.5
                         factor(ifelse(prob_grid > 0.5, 1, 0), levels = c(0, 1))
                       }
                     },
                     {
                       factor(rep(0, nrow(grid_set_orig)), levels = c(0,1))
                     }
    )
    

    grid_set_orig$Prediction <- y_grid
    
    original_points <- plot_set_orig
    colnames(original_points) <- c("Age", "EstimatedSalary", 'ActualClass')
    
    list(grid_data = grid_set_orig,
         original_points = original_points)
  }
  
  output$trainingPlot <- renderPlot({
    results <- model_results()
    data_sets <- split_data()
    
    plot_data <- generate_plot_data(plot_set_orig = data_sets$training_set_split,
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
    
    plot_data <- generate_plot_data(plot_set_orig = data_sets$test_set_split,
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
