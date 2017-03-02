library(shiny)
library(DT)

MaturityData <- read.csv("MaturityData.csv", stringsAsFactors = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
  h1('Fish length at maturity'),
  
  fluidRow(
    column(12, DT::dataTableOutput('MaturityTable'))
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$table = DT::renderDataTable(MaturityData, server = TRUE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

