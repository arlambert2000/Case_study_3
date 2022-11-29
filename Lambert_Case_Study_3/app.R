#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(shiny)
library(tidyverse)
library(randomForest)
library(caret)


breast.cancer <- read.csv('./breast-cancer.csv', stringsAsFactors=FALSE, sep=",", header=TRUE)
breast.cancer <- select(breast.cancer,-id)
breast.cancer$diagnosis <- as.factor(breast.cancer$diagnosis)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Breast Cancer Type"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("trees",
                        "Number of trees:",
                        min = 1,
                        max = 1000,
                        value = 500),
            
            sliderInput("vars",
                      "Number of variables tried at each split:",
                      min = 1,
                      max = 20,
                      value = 5),
            
            numericInput("sample",
                         "Percent of Data Used in Training Set:",
                         value = 70)
            ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           textOutput("testing_accuracy")
        )
)
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  

  model<- reactive({
  
  prob <- c((input$sample)/100,1-((input$sample)/100))
  
  sample_split <- sample(c(TRUE,FALSE),nrow(breast.cancer),replace=TRUE,prob=prob)
  training <- breast.cancer[sample_split,]
  testing <- breast.cancer[!sample_split,]
    
  rf_model <- randomForest(diagnosis~., data = training, ntree = input$trees, mtry = input$vars, proximity = TRUE)
  
  prediction_train <-predict(rf_model,training)
  training_cm <- confusionMatrix(prediction_train,training$diagnosis)
  training_accuracy <- training_cm$overall[1]*100
  
  prediction_test <-predict(rf_model,testing)
  testing_cm <- confusionMatrix(prediction_test,testing$diagnosis)
  testing_accuracy <- testing_cm$overall[1]*100
  
  bar_plot<- barplot(table(prediction_test), col = c('red','blue'), border = 'white',
                     xlab = 'Type of Tumor',
                     ylim = c(0,150),
                     main = 'Classification of Cancer')
  
  list(prediction_test = prediction_test,training_accuracy = training_accuracy,testing_accuracy = testing_accuracy, Plot = bar_plot)
  
  })
  
  output$distPlot <- renderPlot({
     model()$Plot
     
     label<-table(model()$prediction_test)
     
     text(model()$Plot,label+4,labels=as.character(label))
    })
  
  output$testing_accuracy <- renderText({
    testing_accuracy <- paste0("The Testing Accuracy is: ", model()$testing_accuracy)
    print(testing_accuracy)
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
