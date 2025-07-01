# ui.R

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Classification Algorithm Visualizer"),
  
  # Sidebar with input controls
  sidebarLayout(
    sidebarPanel(
      # Data Split Ratio
      sliderInput("split_ratio",
                  "Training Data Split Ratio:",
                  min = 0.50,
                  max = 0.90,
                  value = 0.75,
                  step = 0.05),
      
      # Seed Value
      numericInput("seed_value",
                   "Seed Value for Data Split:",
                   value = 0,
                   min = 0),
      actionButton("calculate_seed_button", "Calculate Seed * 5"),
      textOutput("calculated_seed_output"),
      tags$hr(),
      
      # Algorithm Choice
      radioButtons("algorithm_choice",
                   "Choose Classification Algorithm:",
                   choices = list("Logistic Regression" = "LogisticRegression",
                                  "K-Nearest Neighbors (KNN)" = "KNN",
                                  "Kernel SVM" = "SVM",
                                  "Naive Bayes" = "NaiveBayes",
                                  "Decision Tree" = "DecisionTree",
                                  "Random Forest" = "RandomForest"),
                   selected = "LogisticRegression"),
      
      # KNN specific parameters
      conditionalPanel(
        condition = "input.algorithm_choice == 'KNN'",
        sliderInput("k_value",
                    "Number of Neighbors (K):",
                    min = 1,
                    max = 15,
                    value = 5,
                    step = 1)
      ),
      
      # SVM specific parameters
      conditionalPanel(
        condition = "input.algorithm_choice == 'SVM'",
        selectInput("svm_kernel",
                    "Kernel Type:",
                    choices = c("linear", "polynomial", "radial", "sigmoid"),
                    selected = "radial"),
        sliderInput("svm_cost",
                    "Cost (C):",
                    min = 0.1,
                    max = 10,
                    value = 1,
                    step = 0.1),
        conditionalPanel(
          condition = "input.svm_kernel == 'polynomial'",
          sliderInput("svm_degree",
                      "Degree (for polynomial kernel):",
                      min = 2,
                      max = 10,
                      value = 3,
                      step = 1)
        ),
        conditionalPanel(
          condition = "input.svm_kernel == 'radial' || input.svm_kernel == 'polynomial' || input.svm_kernel == 'sigmoid'",
          sliderInput("svm_gamma",
                      "Gamma:",
                      min = 0.1,
                      max = 10,
                      value = 0.5,
                      step = 0.1)
        )
      ),
      
      # Decision Tree specific parameters
      conditionalPanel(
        condition = "input.algorithm_choice == 'DecisionTree'",
        sliderInput("dt_cp",
                    "Complexity Parameter (cp):",
                    min = 0.001,
                    max = 0.1,
                    value = 0.01,
                    step = 0.001)
      ),
      
      # Random Forest specific parameters
      conditionalPanel(
        condition = "input.algorithm_choice == 'RandomForest'",
        sliderInput("rf_ntree",
                    "Number of Trees (ntree):",
                    min = 10,
                    max = 500,
                    value = 100,
                    step = 10),
        sliderInput("rf_mtry",
                    "Number of Variables to Sample (mtry):",
                    min = 1,
                    max = 2, # Since we only have 2 features (Age, EstimatedSalary)
                    value = 2,
                    step = 1)
      ),
      tags$hr(),
      
      h4("Predict New Data Point:"),
      numericInput("new_age", "Age:", value = 30, min = 0),
      numericInput("new_salary", "Estimated Salary:", value = 50000, min = 0),
      actionButton("predict_new_button", "Predict Class"),
      textOutput("new_prediction_output")
    ),
    
    # Main panel with tabs for output
    mainPanel(
      tabsetPanel(
        tabPanel("How to Use",
                 h3("Welcome to the Classification Algorithm Visualizer!"),
                 p("This interactive tool allows you to explore how different machine learning classification algorithms draw decision boundaries based on two features (Age and Estimated Salary) and predict a binary outcome (Purchased or Not Purchased)."),
                 tags$hr(),
                 h4("1. Data Loading:"),
                 p("The app automatically loads the ", code("Social_Network_Ads.csv"), " dataset. This dataset is expected to have 'Age' in the 3rd column, 'Estimated Salary' in the 4th column, and 'Purchased' (0 or 1) in the 5th column."),
                 h4("2. Adjust Data Split Ratio:"),
                 p("Use the ", strong("Training Data Split Ratio"), " slider to determine what percentage of your uploaded data should be used for training the model, with the remainder used for testing."),
                 tags$hr(),
                 h4("3. Select a Classification Algorithm:"),
                 p("Choose one of the following algorithms from the radio buttons in the sidebar:"),
                 tags$ul(
                   tags$li(strong("K-Nearest Neighbors (KNN):"), " A simple, instance-based learning algorithm."),
                   tags$li(strong("Kernel Support Vector Machine (SVM):"), " A powerful algorithm for finding optimal hyperplanes."),
                   tags$li(strong("Naive Bayes:"), " A probabilistic classifier based on Bayes' theorem."),
                   tags$li(strong("Decision Tree:"), " A tree-like model of decisions and their possible consequences."),
                   tags$li(strong("Random Forest:"), " An ensemble method that builds multiple decision trees and merges their results."),
                   tags$li(strong("Logistic Regression:"), " A linear model used for binary classification, predicting the probability of an event.")
                 ),
                 p("Each algorithm may have its own adjustable parameters below its selection (e.g., 'K' for KNN, 'Cost' for SVM). Experiment with these values!"),
                 tags$hr(),
                 h4("4. Adjust Seed Value:"),
                 p("The ", strong("Seed Value"), " controls the randomness in data splitting. Changing it will create different training and test sets, which can affect model results."),
                 p("There's also a button to calculate the seed value multiplied by 5, just for demonstration!"),
                 tags$hr(),
                 h4("5. Predict New Data Point:"),
                 p("Enter new values for Age and Estimated Salary in the sidebar, then click 'Predict Class' to see the model's classification for that new point."),
                 tags$hr(),
                 h4("6. View Results:"),
                 p("Navigate through the tabs in the main panel to see the results:"),
                 tags$ul(
                   tags$li(strong("Training Set Visualization:"), " Shows the decision boundary learned by the chosen algorithm on the data it was trained on."),
                   tags$li(strong("Test Set Visualization:"), " Shows how the learned decision boundary classifies unseen (test) data points."),
                   tags$li(strong("Confusion Matrix:"), " Provides a summary of the model's performance on the test set, showing true positives, true negatives, false positives, and false negatives.")
                 ),
                 p("In the visualization plots, the colors consistently represent the following:",
                   tags$ul(
                     tags$li(strong("Red:"), " Indicates 'Did Not Purchase' (Class 0)."),
                     tags$li(strong("Green:"), " Indicates 'Purchased' (Class 1).")
                   ),
                   "The background regions show the model's predicted class for any given Age and Estimated Salary combination. The solid circles are the actual data points. If a green dot appears in a red region, or vice-versa, it means the model has made a misclassification for that data point, which is expected for imperfect models."
                 ),
                 tags$hr(),
                 em("Built with Shiny in R.")
        ),
        tabPanel("Raw Data", tableOutput("rawDataOutput")),
        tabPanel("Training Set",
                 textOutput("model_status_text"), # Display model status message here
                 plotOutput("trainingPlot")),
        tabPanel("Test Set", plotOutput("testPlot")),
        tabPanel("Confusion Matrix", plotOutput("confusionMatrixPlot"))
      )
    )
  )
))
