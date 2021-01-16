##  The app is deployed on https://henrychang.shinyapps.io/Blood_Donations/
##  Author: Henry Chang   

lapply(c('shinydashboard','shiny','DT'), function(package) {
  if(!require(package, character.only = TRUE)) {
    tryCatch({
      install.packages(package)
    },
    warning = function(w) {
      NULL
    },
    error = function(e) {
      NULL
    },
    finally = {
      require(package, character.only = TRUE)
    }
    )
  }
}
)

  
library(shinydashboard)
library(shiny)  
library(DT)
data = read.csv("Blood_Donations_Data.csv")

ui <- dashboardPage(
  skin = "purple",
  ######### Dashboard Header ###############
  dashboardHeader(title = "Predicting Blood Donations",
                  titleWidth = 500),
  
  ######### Dashboard Sidebar ##############
  dashboardSidebar(
    sidebarMenu( 
      menuItem("Individual prediction", tabName = "prediction", icon = icon("bullseye")),
     
      menuItem("Group prediction", tabName = "Group", icon = icon("eye")) 
      
      
    )
  ),
  ######### Dashboard Body ##############
  dashboardBody( 
     
    tabItems(
      tabItem( tabName= "prediction",
               fluidPage(  
   
                 h2("Hello, Please alter the donor's data to obtain the Probability of donating in the next month"),  
                   
               tags$br(),
               tags$br(),  
               h3("The Donor's data:"),
                 fluidRow(
              
                   column( 4, numericInput("v1", "Months since Last Donation", 2, min = 1, max = 100)
                   ),
                   column( 4, numericInput("v2", "Frequency (times)", 3, min = 1, max = 100)
                   ),
                   column( 4, numericInput("v3", "Months since First Donation", 30, min = 1, max = 100)
                   )) 
               ,
               tags$br(),   
 
               tags$br(),  
               h3("Result:"),
               h4("Probability of Donating blood in the next month:") ,
               verbatimTextOutput("likelihood"),
             
               tags$br(),  
               tags$br(),  
             
               h4("Average time(Month) between each visit"),
               verbatimTextOutput("ave_time")      
  
         
               )) 
      ,
  
      tabItem( tabName= "Group",
               fluidPage(  
                 titlePanel("Please upload the data (CSV file) to see the prediction for a group of donors"),
                 
                 tags$br(),
                 tags$br(),
                 fluidRow(
                   
                   # Input: Select a file ----
                    
                   fileInput("test_data", "Choose CSV File (test_sample.csv)",
                             multiple = FALSE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")) 
           
           
                 ),
                 h4("Expected total blood donation (c.c.)"),
                 verbatimTextOutput("total_blood_donation"), 
          
                 tags$br(),
                 h4("Prediction for the uploaded CSV file"),
                 tabPanel("group prediction",
                          DT::dataTableOutput("datatable", width = "100%")), 
             
                 tags$br(),
                 tags$br() 
               
                 ))   
 
    )))



server <- function(input, output) {
  
     
  
  
  # Tab1   --- Prediction ---------
 
  train = read.csv("Blood_Donations_Data.csv")
  
  
  train <- data[c("Recency..months.", "Frequency..times.","Time..months.","Average.time.between.each.visit","donated_May_2018")]
  
  ### logistic regression   
  
  train$donated_May_2018 <- as.factor(data$donated_May_2018)   # make target variable become catergorical
  
  
  
  model <- glm(donated_May_2018 ~.,family=binomial(link='logit'),data=train)
  
  # make prediction and return the result
  
  output$likelihood <-  renderText({ 
     
    
    v4 = input$v3 / input$v2 
    userInput <- data.frame(input$v1,input$v2,input$v3,v4)
    names(userInput)<-c("Recency..months.", "Frequency..times.","Time..months.","Average.time.between.each.visit")
       
    predict(model, newdata = userInput, type="response")
     
    
    })  
   
  
  output$ave_time <-   renderText({ 
     v4 = input$v3 / input$v2
     v4
    
  })  
  
###  ------Tab2 - predicted Data Table -----------

  
  train2 = readr::read_csv("Blood_Donations_Data.csv", col_names = TRUE)
  
  train2 <- train2[-c(1)]
  
  train2$donated_May_2018 <- as.factor(train2$donated_May_2018)   # make target variable become catergorical
  
  model2 <- glm(donated_May_2018 ~.,family=binomial(link='logit'),data=train2)
  
   
  
  # Page 2 - Data table output
   output$datatable <- DT::renderDataTable({ 
    
         
     if (is.null(input$test_data)){
       return(NULL)
     }else{ 
  
    test =  readr::read_csv(input$test_data$datapath, col_names = TRUE)
   
    
    Prediction_Probability <- predict(model2, newdata = test, type="response")
    test <- cbind(test,Prediction_Probability)
    test$Prediction_binary <- test$Prediction_Probability
    test$Prediction_binary  <-  ifelse(test$Prediction_Probability < 0.5, 'No', 'Yes')
     
    dat <- datatable(test, options = list(scrollX = TRUE, lengthMenu = c(50, 100, 200)))%>% 
        formatStyle(c("Prediction_Probability","Prediction_binary"),
        color = 'white', backgroundColor = 'gray', fontWeight = 'bold')   
              
       }
 
     
    return(dat)
  })
   
    
   # Page 2 - total_blood_donation
   output$total_blood_donation <-   renderText({ 
     
     
     if (is.null(input$test_data)){
       return(NULL)
     }else{ 
       test =  readr::read_csv(input$test_data$datapath, col_names = TRUE)
       Prediction_Probability <- predict(model2, newdata = test, type="response")
       test <- cbind(test,Prediction_Probability)
       sum(test$Prediction_Probability)*250  }
   })  
       
}
shinyApp(ui = ui, server = server)