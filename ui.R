# This is the main ui.R file for the app I am building

library(shiny)

shinyUI(fluidPage(
  
  # This is the title of my App
  titlePanel("Visualization of different classification algorithms"),
  
  # I want my mainpage to be able to input
  # 1. training / test date split between 50 and 90%
  # 2. Seed value for reproducability
  # 3. The Algorithm to choose
  # 4. Inference based on the algorithm used
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("split_ratio",
                  "Training vs Test data ratio:",
                  min = 0.50,
                  max = 0.90,
                  value = 0.75,
                  step = 0.05),
      
      numericInput("seed_value",
                   "Enter a Seed value to get same results each time:",
                   value = 144,
                   min = 0),
   
      
      # Going to try a radio instead of a pulldown since only 6 options
      # Depending on the radio button selected you could have different options
      # will use conditional Panel for this
      
      radioButtons("algorithm_choice",
                   "Pick a  Classification Algorithm to run Social Ad Data on",
                   choices = list("Logistic Regression" = "LogisticRegression",
                                  "K-Nearest Neighbors (KNN)" = "KNN",
                                  "Kernel SVM" = "SVM",
                                  "Naive Bayes" = "NaiveBayes",
                                  "Decision Tree" = "DecisionTree",
                                  "Random Forest" = "RandomForest"),
                   selected = "LogisticRegression"),
      
      # I made the values between 5 and 10
      conditionalPanel(
        condition = "input.algorithm_choice == 'KNN'",
        sliderInput("k_value",
                    "Number of Neighbors (K):",
                    min = 5,
                    max = 10,
                    value = 5,
                    step = 1)
      ),
      
      # SVM - I left out gaussian and sigmond
      conditionalPanel(
        condition = "input.algorithm_choice == 'SVM'",
        selectInput("svm_kernel",
                    "Kernel Type:",
                    choices = c("linear", "polynomial", "radial"),
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
          condition = "input.svm_kernel == 'radial' || input.svm_kernel == 'polynomial'",
          sliderInput("svm_gamma",
                      "Gamma:",
                      min = 0.1,
                      max = 10,
                      value = 0.5,
                      step = 0.1)
        )
      ),
      
      # Random Forest specific parameters
      conditionalPanel(
        condition = "input.algorithm_choice == 'RandomForest'",
        sliderInput("rf_ntree",
                    "Number of Trees (ntree):",
                    min = 10,
                    max = 100,
                    value = 20,
                    step = 10),
        sliderInput("rf_mtry",
                    "Number of Variables to Sample (mtry):",
                    min = 1,
                    max = 2, # Since we only have 2 features age and salary
                    value = 2,
                    step = 1)
      ),

      h5("Input a new Data Point, it will run inference on the classification algorithm selected above "),
      numericInput("inf_age", "Age:", value = 30, min = 0),
      numericInput("inf_salary", "Estimated Salary:", value = 75000, min = 0),
      actionButton("run_inference", "Run Inference"),
      textOutput("inf_prediction_output")
    ),
    
    # Main panel with tabs for output
    mainPanel(
      tabsetPanel(

        tabPanel("Selected Algorithm Results on Training Set",
                 textOutput("model_status_text"),
                 plotOutput("trainingPlot")),
        tabPanel("Selected Algorithm Results on Test Set", plotOutput("testPlot")),
        tabPanel("Confusion Matrix", plotOutput("confusionMatrixPlot")),

        tabPanel("Raw Data", tableOutput("rawDataOutput")),
        tabPanel("Help & Documentation",
                 h2("Help & Documentation"),
                 p(
                   tags$a(href = "https://github.com/rjburgess/DS501/blob/main/README.md", target = "_blank", "Will redirect you to the Readme in Github"),
                 )
        )
      )
    )
  )
))
