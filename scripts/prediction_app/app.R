#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
DF_orig <- read_csv("./data/animal/train.csv.gz")

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Making prediction"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       radioButtons(inputId="AnimalType", 
                    label="Choose Cat or Dog",
                    choices=c("Cat",
                              "Dog"),
                    selected = "Cat"),
       
       textInput(inputId = "Name",
                 label = "Specify Name",
                 value = "Bob"),
       
       selectInput(inputId = "SexuponOutcome",
                   label = "Choose sex upon outcome",
                   choices = unique(DF_orig$SexuponOutcome)[1:5],
                   selected = unique(DF_orig$SexuponOutcome)[1]),
       
       textInput(inputId = "Color",
                   label = "Choose color",
                   value = "White"),
       
       textInput(inputId = "Breed",
                   label = "Specify Breed",
                   value = "Shetland Sheepdog Mix"),
       
       selectInput(inputId = "AgeuponOutcome_N",
                   label = "Choose Age upon Outcome",
                   choices = 1:100,
                   selected = 1),
       
       radioButtons(inputId="AgeuponOutcome_unit", 
                    label="",
                    choices=c("day(s)",
                              "week(s)",
                              "month(s)", 
                              "year(s)"),
                    selected = "week(s)")),
      
      # Show a plot of the generated distribution
      mainPanel(
        actionButton("button", "Calculate"),
        textOutput("prediction_obj")
        #dataTableOutput('output$prediction_dt')
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  library(stringr); library(readr)

  calculation_function <- eventReactive(input$button, {
    
    DF_orig_temp <- DF_orig
    subDF <- DF_orig[1, ]
    
    subDF$AnimalType <- input$AnimalType
    subDF$SexuponOutcome <- input$SexuponOutcome
    subDF$AgeuponOutcome <- str_c(input$AgeuponOutcome_N, " ",
                                  input$AgeuponOutcome_unit)
    subDF$Breed <- input$Breed
    subDF$Color <- input$Color
    subDF$Name <- input$Name
    
    DF_orig_temp[nrow(DF_orig) +1 , ] <- subDF[1, ]  
    
    source("../../scripts/making_features_4_func.R")
    DF_binary <- make_features(DF_orig_temp)
    DF_for_prep <- DF_binary
    
    for_pred = TRUE
    use_formula = TRUE
    source("../../scripts/Pre_process_data_func.R")
    x_data_pred <- pre_proc_my_data(DF_for_prep)
    res_GLM <- load("../../Rdata/res_GLM.RData")
    
    pred_adoption_prob <- predict(res_GLM$model, x_data_pred, type = "prob")
    as.character(pred_adoption_prob$yes)
  })
  
  output$prediction_obj <- renderText({
    calculation_function()
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)

