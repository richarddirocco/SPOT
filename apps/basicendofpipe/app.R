library(shiny)
library(shinythemes)
library(ggvis)
library(dplyr)

# Import species group
SpeciesGroups <- read.csv("SpeciesGroups.csv")

ui <- function(request){
  (fluidPage(
    
    # Add script to resize iframe automatically
    # Script from here: https://groups.google.com/forum/#!topic/shiny-discuss/cFpn3UcZTvQ
    tags$head(includeScript("iframeResizer.contentWindow.min.js")),
    
    # Hide control button (contains SVG, Canvas and download options)
    tags$head(
      tags$style(HTML('a[class="ggvis-dropdown-toggle"]{display:none;}'))
    ),
    
    theme = shinytheme("cosmo"),
    
    sidebarLayout(
      sidebarPanel(
        
#        radioButtons("CommonScientific", "Select species by:", choices = c("Common name", "Scientific name"), selected = "Common name"),
#        conditionalPanel("input.CommonScientific == 'Common name'",
                         selectInput("SelectSpecies", 
                                     label = "Select species:", 
                                     choices = sort(SpeciesGroups$English.Common.Name), 
                                     multiple=TRUE, selectize = TRUE),
#        ),
#        conditionalPanel("input.CommonScientific == 'Scientific name'",
#                         selectInput("SelectSpecies:", 
#                                     label = "Select species:", 
#                                     choices = sort(SpeciesGroups$Scientific.Name), 
#                                     multiple=TRUE)
#        ),
        br(),
        numericInput("EoP_flowrate", label = "Maximum intake flow rate (L/s):", min = 0, value = 125, step = 5),
        
        helpText(a(href="mailto:richard.dirocco@dfo-mpo.gc.ca", "Submit feedback"), align = "center")
      ),    # close sidebarPanel
      
      mainPanel(
        ggvisOutput("ggvis"),
        br(),
        htmlOutput("EoP_Text"),
        align = "center"
      )    # close mainpanel
    )      # close sidebarLayout
  ))}       # close fluidpage


server <- function(input, output, session){ 

  EoP_PlotData <- reactive({
    # A blank value will cause the application to crash. Set flowrate to 1 if it's blank
    if(!is.numeric(input$EoP_flowrate)){temp_flowrate <- 1} else {temp_flowrate <- input$EoP_flowrate}
    # Create a dataframe based on the flowrate selected by the user
    EoP_dataSet <- data.frame(seq(from=0, to=(temp_flowrate), by=temp_flowrate/10))
    colnames(EoP_dataSet) <- "Flow"
    # Create a temporary dataframe with only the selected Fishes
    Temp <- SpeciesGroups %>% 
      filter(English.Common.Name %in% input$SelectSpecies)
    # Fill the flow dataframe with the required screen area based on the slowest swimming group
    EoP_dataSet$Screen.Area <- max(Temp$Screen.Area.Coefficient) * EoP_dataSet$Flow
    EoP_dataSet
  })
  
  EoP_PlotData %>%
    ggvis(x= ~Flow, y=~Screen.Area) %>%
    layer_lines(strokeWidth := 2.5, stroke := "#337ab7") %>%
    add_axis("y", offset = 1, title = "Effective Screen Area (m²)", title_offset = 50, ticks = 6,
             properties = axis_props(
               title = list(fontSize = 15, fontWeight = "normal"),
               labels = list(fontSize = 12)
             )
    )%>%
    add_axis("x" ,offset = 1, title = "Intake flow rate (L/s)", ticks = 5,
             properties = axis_props(
               title = list(fontSize = 15, fontWeight = "normal"),
               labels = list(fontSize = 12)
             )
    )%>%
    scale_numeric("x", domain = c(0,NA), nice = FALSE, override = TRUE, expand = 0) %>%
    scale_numeric("y", domain = c(0,NA), nice = FALSE, override = TRUE, expand = 0) %>%
    set_options(width="auto", renderer = "canvas") %>%
    bind_shiny("ggvis")
  
  output$EoP_Text <- renderUI({
    if(is.numeric(input$EoP_flowrate) & !is.na(max(EoP_PlotData()$Screen.Area))){
      HTML("The selected fish require an Effective Screen Area of ", max(EoP_PlotData()$Screen.Area), "m<sup>2</sup>.")
    }

  })
}

enableBookmarking(store = "url")

shinyApp( ui = ui, server = server)