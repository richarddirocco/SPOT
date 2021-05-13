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

# Convert some variable to factors. This makes it easier to sort the table
MaturityData$Sex <- as.factor(MaturityData$Sex)
MaturityData$Country <- as.factor(MaturityData$Country)

# Translate column names to French
names(MaturityData) <- c("ScientificName", "EnglishName", "FrenchName", "Sexe", "Longueur minimale à première maturité (mm)", 
                         "Longueur moyenne à première maturité (mm)", "Type de mesure", "Pays", "Région", "Référence")

# Translate length types
# MaturityData$"Type de mesure"[MaturityData$"Type de mesure"=="FL"] <- "LF"
# MaturityData$"Type de mesure"[MaturityData$"Type de mesure"=="SL"] <- "LS"
# MaturityData$"Type de mesure"[MaturityData$"Type de mesure"=="TL"] <- "LT"
# MaturityData$"Type de mesure"[MaturityData$"Type de mesure"=="NG"] <- "SO"
# MaturityData$"Type de mesure" <- as.factor(MaturityData$"Type de mesure")

# Sort data by Country then Sex
MaturityData <- MaturityData[order(MaturityData$Pays, MaturityData$Sexe),]


ui <- fluidPage(
  
  # Add script to resize iframe automatically
  # Script from here: https://groups.google.com/forum/#!topic/shiny-discuss/cFpn3UcZTvQ
  tags$head(includeScript("iframeResizer.contentWindow.min.js")),
  
  # Set theme
  theme = shinytheme("cosmo"),

  # Add break below title
  br(),
  
  # Help text explaining how to use the tool
  helpText("Sélectionnez une espèce par son nom courant ou scientifique. Les données disponibles sur la longueur à maturité de l’espèce sélectionnée s’afficheront dans le tableau ci-dessous. Les données proviennent de FishBase et d’autres sources (voir la colonne sur les références). La colonne sur le type de mesure indique le type de mesure de la longueur du poisson utilisé : FL (longueur à la fourche), SL (longueur standard), TL (longueur totale) ou NG (sans objet)."),
  
  # Add break before selecter
  br(),
  
  # Create radio button to select species by common or scientific name
  column(6, radioButtons("Selecter", label = "Sélection par :",
               choices = list("nom courant" = 1, "nom scientifique" = 2), selected=1)),
  
  # Create drop down menus based on common or scientific selection
  conditionalPanel("input.Selecter == '1'",
                   column(6, selectInput("CommonSpecies", 
                                         label = "Espèce :",
                                         choices =  sort(unique(MaturityData$FrenchName)),
                                         selected = "Brochet")
                   )
  ),
  conditionalPanel("input.Selecter == '2'",
    column(6, selectInput("ScientificSpecies", 
                          label = "Species",
                          choices =  sort(unique(MaturityData$ScientificName)),
                          selected = "Esox lucius")
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
      displaydata <- subset(displaydata, FrenchName == input$CommonSpecies)
    }
    if (input$Selecter==2){
      displaydata <- subset(displaydata, ScientificName == input$ScientificSpecies)
    }
    displaydata
  })
  
  # Create data table
  output$MaturityData = DT::renderDataTable(DisplayData(),
                                            filter = "top",
                                            class = "compact",
                                            server = TRUE, 
                                            escape = FALSE, 
                                            rownames = FALSE,
                                            colnames=c("Longueur minimale à première maturité (mm)" = 5,
                                                       "Longueur moyenne à première maturité (mm)" = 6,
                                                       "Type de mesure" = 7),
                                            options = list(
                                              columnDefs = list(list(className = 'dt-center', targets = c(3, 4, 5)),
                                                                (list(targets = c(0,1,2), visible = FALSE))),
                                              pageLength = 500,
                                              dom = 't',
                                              autoWidth = TRUE
                                            )
  )
}

shinyApp( ui = ui, server = server)
