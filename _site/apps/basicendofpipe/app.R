library(shiny)
library(shinythemes)
library(ggvis)
library(dplyr)

# Import species group
SpeciesGroups <- read.csv("SpeciesGroups.csv")

ui <- function(request){
  (fluidPage(
    
    tags$head(includeScript("google-analytics.js")),
    # Set maximum width of app
    tags$head(tags$style(type="text/css", ".container-fluid {  max-width: 1200px}")),
    
    # Add script to resize iframe automatically
    # Script from here: https://groups.google.com/forum/#!topic/shiny-discuss/cFpn3UcZTvQ
    tags$head(includeScript("iframeResizer.contentWindow.min.js")),
    
    # Hide control button (contains SVG, Canvas and download options)
    tags$head(
      tags$style(HTML('a[class="ggvis-dropdown-toggle"]{display:none;}'))
    ),
    
    theme = shinytheme("cosmo"),
    br(),
    br(),
    sidebarLayout(
      sidebarPanel(
        selectInput("SelectSpecies", 
                    label = "Select species:", 
                    choices = SpeciesGroups$English.Common.Name, 
                    multiple=TRUE, selectize = TRUE),
      helpText("Tip: Select All/Unknown if you are unsure of the species near the intake"),
      
      radioButtons("radio", "Calculate:",
                   choices = list("Screen size using intake rate" = 1, "Intake rate using screen size" = 2),selected = 1),
      
      br(),
      conditionalPanel(
        condition = "input.radio == 1",
        numericInput("flowrate", label = "Maximum Intake Flow Rate (L/s):", min = 0, value = 150, step = 5),),  
      conditionalPanel(
        condition = "input.radio == 2",
        numericInput("screensize", label = "Effective Screen Area (m²):", min = 0, value = 5, step = 0.5)),
      helpText(a(href="mailto:richard.dirocco@dfo-mpo.gc.ca", "Submit feedback"), align = "center"),
    ),   
      
      mainPanel(
        ggvisOutput("ggvis"),
        br(),
        conditionalPanel(
          condition = "input.radio == 1",
          htmlOutput("ScreenAreaText")),
        conditionalPanel(
          condition = "input.radio == 2",
          htmlOutput("FlowText")),
        htmlOutput("ApproachText"),
        align = "center"
      )    # close mainpanel
    )      # close sidebarLayout
  ))}       # close fluidpage


server <- function(input, output, session){ 

  PlotData <- reactive({
    if(input$radio==1){
      # A blank value will cause the application to crash. Set flowrate to 1 if it's blank
      if(!is.numeric(input$flowrate)){temp_flowrate <- 1} else {temp_flowrate <- input$flowrate}
      # Create a dataframe based on the flowrate selected by the user
      dataSet <- data.frame(seq(from=0, to=(temp_flowrate), by=temp_flowrate/10))
      colnames(dataSet) <- "Flow"
      # Create a temporary dataframe with only the selected Fishes
      Temp <- SpeciesGroups %>% 
        filter(English.Common.Name %in% input$SelectSpecies)
      # Fill the flow dataframe with the required screen area based on the slowest swimming group
      dataSet$Screen.Area <- max(Temp$Screen.Area.Coefficient) * dataSet$Flow
    }
    if(input$radio==2){
      # A blank value will cause the application to crash. Set screensize to 1 if it's blank
      if(!is.numeric(input$screensize)){temp_screensize <- 1} else {temp_screensize <- input$screensize}
      # Create a dataframe based on the screensize selected by the user
      dataSet <- data.frame(seq(from=0, to=(temp_screensize), by=temp_screensize/10))
      colnames(dataSet) <- "Screen.Area"
      # Create a temporary dataframe with only the selected Fishes
      Temp <- SpeciesGroups %>%
        filter(English.Common.Name %in% input$SelectSpecies)
      # Screen Area / Screen Area Coefficient = Flow
      dataSet$Flow <- dataSet$Screen.Area / max(Temp$Screen.Area.Coefficient)
      # The last line will generate an infinite value (something / 0) so we will replace it
      dataSet$Flow[!is.finite(dataSet$Flow)] <- 0
    }
    dataSet$Approach.Velocity <- min(Temp$Approach.Velocity)
    dataSet
  })

  
  PlotData %>%
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
  
  output$ScreenAreaText <- renderUI({
    if(is.numeric(input$flowrate) & !is.na(max(PlotData()$Screen.Area))){
      HTML("An Effective Screen Area of ", max(round(PlotData()$Screen.Area, 2)), "m<sup>2</sup> is recommended for an intake flow rate of ", input$flowrate, "L/s.")
    }
  })
  output$FlowText <- renderUI({
    if(is.numeric(input$screensize) & !is.na(max(PlotData()$Screen.Area))){
      HTML("The intake flow rate should not exceed", max(round(PlotData()$Flow, 1)), "L/s if the Effective Screen Area is " ,input$screensize, "m<sup>2</sup>.")
    }
  })
  output$ApproachText <- renderUI({
    if(is.numeric(input$flowrate) & !is.na(max(PlotData()$Screen.Area))){
      HTML("The approach velocity should not exceed ", min(PlotData()$Approach.Velocity), "m/s.")
    }
  })
  
}

enableBookmarking(store = "url")

shinyApp( ui = ui, server = server)