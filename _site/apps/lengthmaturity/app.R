library(shiny)
library(shinythemes)
library(DT)

# Change plot fonts from defaults
library(showtext)
font.add.google("Lato","lato")
showtext.auto()

options(scipen=5)

# Import Maturity Data
MaturityData <- read.csv("MaturityData.csv")
MaturityData$X1 <- NULL

# Convert some variable to factors. This makes it easier to sort the table
MaturityData$Sex <- as.factor(MaturityData$Sex)
MaturityData$Country <- as.factor(MaturityData$Country)
MaturityData$Measurement <- as.factor(MaturityData$Measurement)

# Sort data by Country then Sex
MaturityData <- MaturityData[order(MaturityData$Country, MaturityData$Sex),]


# Convert length from cm to mm
MaturityData$Minimum.length.at.first.maturity <- MaturityData$Minimum.length.at.first.maturity * 10
MaturityData$Mean.length.at.first.maturity..Lm. <- MaturityData$Mean.length.at.first.maturity..Lm. * 10

ui <- fluidPage(
  
  # Set theme
  theme = shinytheme("cosmo"),
  
  # Tool title
  h1('Fish Length at Maturity', align = "center"),
  
  # Add break below title
  br(),
  
  # Help text explaining how to use the tool
  helpText("Select a species using the scientific or common name. The available length at maturity data for the selected species will be displayed in the table below. All data is from", a("FishBase.", href='http://www.fishbase.ca/'), "The Measurement column shows the type of fish length measurement used: FL (Fork Length), SL (Standard Length), TL (Total Length), or NG (Not Given). "),
  
  # Add break before selecter
  br(),
  
  # Create radio button to select species by common or scientific name
  column(6, radioButtons("Selecter", label = "Select species using:",
               choices = list("Common name" = 1, "Scientific name" = 2), selected=1)),
  
  # Create drop down menus based on common or scientific selection
  conditionalPanel("input.Selecter == '1'",
                   column(6, selectInput("CommonSpecies", 
                                         label = "Species",
                                         choices =  sort(unique(MaturityData$CommonName)),
                                         selected = "Northern pike")
                   )
  ),
  conditionalPanel("input.Selecter == '2'",
    column(9, selectInput("ScientificSpecies", 
                          label = "Species",
                          choices =  sort(unique(MaturityData$ScientificName)),
                          selected = MaturityData$ScientificName[230])
    )
  ),
  
  # Add break before table
  br(),
  
  # Create table
  column(12, DT::dataTableOutput('MaturityData'))
)

server <- function(input, output) {
  
  # Subset the MaturityData table based on the species selected
  DisplayData <- reactive({
    displaydata <- MaturityData
    if (input$Selecter==1){
      displaydata <- subset(displaydata, CommonName == input$CommonSpecies)
    }
    if (input$Selecter==2){
      displaydata <- subset(displaydata, ScientificName == input$ScientificSpecies)
    }
    displaydata
  })
  
  # Create data table
  output$MaturityData = DT::renderDataTable(DisplayData()[,c("ScientificName","CommonName",
                                                            "Sex","Minimum.length.at.first.maturity",
                                                            "Mean.length.at.first.maturity..Lm.", "Measurement", 
                                                            "Country","Locality","Reference")],
                                            filter = "top",
                                            class = "compact",
                                            server = TRUE, 
                                            escape = FALSE, 
                                            rownames = FALSE,
                                            colnames=c("Minimum length at first maturity (mm)" = 4,
                                                       "Mean length at first maturity (mm)" = 5),
                                            options = list(
                                              columnDefs = list(list(className = 'dt-center', targets = c(3, 4, 5)),
                                                                (list(targets = c(0, 1), visible = FALSE))),
                                              pageLength = 500,
                                              dom = 't',
                                              autoWidth = TRUE
                                              )
                                            )
}

shinyApp( ui = ui, server = server)
