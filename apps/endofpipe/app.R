library(shiny)
library(shinythemes)
library(ggvis)
library(tidyr)

# import K and b values for each group of fish (used in equations)
data <- read.csv("GroupVariables.csv")
rownames(data)<-data$Group
data$Group <- NULL

# import list of common and scientific names and their respective lengths
FishList <- read.csv("FishList.csv", stringsAsFactors=FALSE)
FishList$GroupName <- as.character(FishList$GroupName)

Velocity = data.frame(seq(from=0.1, to=10, by=0.01))
colnames(Velocity)[1] <- "Velocity"



########################
##   User Interface   ##
########################

ui <- function(request){
  (fluidPage(
    
    tags$head(includeScript("google-analytics.js")),
    
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
        helpText("Need help? Visit the ",
                 a(href="http://www.fishprotectiontools.ca/endofpipe-manual.html",target="_blank", "Manual"), align = "center"
        ),

        selectInput("EoP_Selecter", label = "Select fish by:", 
                     choices = list("All" = 4, "Group" = 0, "Common name" = 1, "Scientific name" = 2), selected=4),
        
        conditionalPanel("input.EoP_Selecter == '0'",
                         h5(strong("Select groups:")),
                         checkboxInput("CatfishSunfish",label = "Catfish & Sunfish", value = TRUE),
                         checkboxInput("Eel", label = "Eel", value = TRUE),
                         checkboxInput("Herring", label="Herring", value=TRUE),
                         checkboxInput("Pike", label="Pike", value =TRUE),
                         checkboxInput("SalmonWalleye", label = "Salmon & Walleye", value = TRUE),
                         checkboxInput("Sturgeon", label = "Sturgeon", value = TRUE)
        ),
        
        conditionalPanel("input.EoP_Selecter == '1'",
                         selectInput("EoP_CName", label = "Select species", choices = sort(FishList$CommonName), selected = "Brook trout")
        ),
        
        conditionalPanel("input.EoP_Selecter == '2'",
                         selectInput("EoP_SName", label = "Select species", choices = sort(FishList$ScientificName), selected = "Salvelinus fontinalis")
        ),
        
        numericInput("EoP_flowrate", label = "Maximum intake flow rate (L/s):", min = 0, value = 125, step = 5),
        
        checkboxInput("Mode",label = "Show advanced options", value = FALSE),
        
        # Advanced mode inputs:
        conditionalPanel("input.Mode == true",
                         selectInput("EoP_Proportion",
                                     label = "Proportion of fish protected",
                                     choices = c("97.5%", "87.5%", "50.0%", "12.5%", "2.5%"),
                                     selected = "97.5%"),
                         
                         sliderInput("EoP_l", label = "Fish length (mm):", min = 25, max = 1000, 
                                     value = 2.5, step = 5, ticks = FALSE),
                         sliderInput("EoP_time", label = "Time to escape screen face (min):", min = 1, max = 30, 
                                     value = 10, step = 0.5, ticks = FALSE)
        ),
        
        helpText(a(href="mailto:richard.dirocco@dfo-mpo.gc.ca", "Submit feedback"), align = "center")
        
      ),    # close sidebarPanel
      
      mainPanel(
        ggvisOutput("ggvis"),
        br(),
        span(textOutput("Warning_Text"), style="color:red"),
        br(),
        htmlOutput("EoP_Text"),
        textOutput("Approach_velocity"),
        align = "center"
      )    # close mainpanel
    )      # close sidebarLayout
  ))}       # close fluidpage



#######################
##       Server      ##
#######################


server <- function(input, output, session){ 
  
  # Set gravitational acceleration constant
  g=9.81

  ##########################################
  ##  End-of-Pipe Screen Size Calculator  ##
  ##########################################
  
  EoP_k <- reactive({
    if(input$EoP_Proportion=="97.5%")
      temp_k <- "X95L_k"
    if(input$EoP_Proportion=="87.5%")
      temp_k <- "X75L_k"
    if(input$EoP_Proportion=="50.0%")
      temp_k <- "k"
    if(input$EoP_Proportion=="12.5%")
      temp_k <- "X75U_k"
    if(input$EoP_Proportion=="2.5%")
      temp_k <- "X95U_k"
    temp_k
  })
  
  EoP_b <- reactive({
    if(input$EoP_Proportion=="97.5%")
      temp_b <- "X95L_b"
    if(input$EoP_Proportion=="87.5%")
      temp_b <- "X75L_b"
    if(input$EoP_Proportion=="50.0%")
      temp_b <- "b"
    if(input$EoP_Proportion=="12.5%")
      temp_b <- "X75U_b"
    if(input$EoP_Proportion=="2.5%")
      temp_b <- "X95U_b"
    temp_b
  })  
  
  # Update group based on species selected
  EoP_Group <- reactive({
    if(input$EoP_Selecter==0){
      tempGroup <- NULL
      if(input$CatfishSunfish == TRUE){
        tempGroup <- append (tempGroup,"Catfish & Sunfish")
      }
      if(input$Eel == TRUE){
        tempGroup <- append (tempGroup,"Eel")
      }
      if(input$Herring == TRUE){
        tempGroup <- append (tempGroup,"Herring")
      }
      if(input$Pike == TRUE){
        tempGroup <- append (tempGroup,"Pike (derived)")
      }
      if(input$SalmonWalleye == TRUE){
        tempGroup <- append (tempGroup,"Salmon & Walleye")
      }
      if(input$Sturgeon == TRUE){
        tempGroup <- append (tempGroup,"Sturgeon")
      }
    }
    if(input$EoP_Selecter==1){
      tempGroup <- (FishList[which(FishList$CommonName==input$EoP_CName),"GroupName"])}
    if(input$EoP_Selecter==2){(
      tempGroup <-FishList[which(FishList$ScientificName==input$EoP_SName),"GroupName"])}
    if(input$EoP_Selecter==4){
      tempGroup <- c(unique(FishList$GroupName))}
    tempGroup
  })
  
  # Update length slider scale based on group selected
  observe({
    if(input$EoP_Selecter==0){
      scale <- 1500}
    if(input$EoP_Selecter==1){
      scale <- FishList[which(FishList$CommonName==input$EoP_CName),"MaxSize"]}
    if(input$EoP_Selecter==2){
      scale <- FishList[which(FishList$ScientificName==input$EoP_SName),"MaxSize"]}
    if(input$EoP_Selecter==4){
      scale <- 1500}
    updateSliderInput(session, "EoP_l", min = 25, max = scale)
  })
  
  # Create functions to determine swimming speed (m/s) based on time (seconds)
  EoP_func = function (t,EoP_Group){data[EoP_Group,EoP_k()]*sqrt(g*input$EoP_l/1000)*((sqrt(input$EoP_l/1000/g))^-data[EoP_Group,"X95L_b"])*t^data[EoP_Group,"X95L_b"]}
  
  # Decrease number of calculations when dealing with larger intakes
  Scaler <- reactive({
    req(input$EoP_flowrate)
      input$EoP_flowrate/80000
  })
  

  # create a dataframe containing the velocity vs distance data based on limits of swim time
  EoP_PlotData <- reactive({
    if(is.numeric(input$EoP_flowrate) & !is.null(EoP_Group())){
      EoP_dataSet <- data.frame(seq(from=0, to=(input$EoP_flowrate/1000), by=Scaler()))
      colnames(EoP_dataSet) <- "Flow"

      TempVelocity <- EoP_func(input$EoP_time*60, "Catfish & Sunfish")
      EoP_dataSet$"Catfish & Sunfish" <- EoP_dataSet$Flow / TempVelocity

      TempVelocity <- EoP_func(input$EoP_time*60, "Salmon & Walleye")
      EoP_dataSet$"Salmon & Walleye" <- EoP_dataSet$Flow / TempVelocity

      TempVelocity <- EoP_func(input$EoP_time*60, "Sturgeon")
      EoP_dataSet$"Sturgeon" <- EoP_dataSet$Flow / TempVelocity

      TempVelocity <- EoP_func(input$EoP_time*60, "Pike (derived)")
      EoP_dataSet$"Pike (derived)" <- EoP_dataSet$Flow / TempVelocity

      TempVelocity <- EoP_func(input$EoP_time*60, "Eel")
      EoP_dataSet$"Eel" <- EoP_dataSet$Flow / TempVelocity

      TempVelocity <- EoP_func(input$EoP_time*60, "Herring")
      EoP_dataSet$"Herring" <- EoP_dataSet$Flow / TempVelocity
      
      EoP_dataSet$Flow <-  EoP_dataSet$Flow*1000

      EoP_dataSet <- EoP_dataSet %>%
        gather(EoP_Group(), key="Group", value=EffectiveScreenArea)

      EoP_dataSet$EffectiveScreenArea <- signif(EoP_dataSet$EffectiveScreenArea, digits = 4)
      
      EoP_dataSet
    } else {
      EoP_dataSet <- data.frame(Flow=c(1),Group=c("Catfish & Sunfish"),EffectiveScreenArea=c(1))
      EoP_dataSet
    }
    
  })

  
  EoP_Worst <- reactive({
    max(EoP_PlotData()$EffectiveScreenArea)
  })
  
  EoP_Worst_Group <- reactive({
    with(EoP_PlotData(),Group[which.max(EffectiveScreenArea)])
  })

  
  output$EoP_Text <- renderUI({
    if(is.numeric(input$EoP_flowrate) & !is.null(EoP_Group())){
        HTML(input$EoP_Proportion, " of", input$EoP_l, "mm fish in ", input$EoP_flowrate, "L/s flow with", input$EoP_time, "minutes to escape the screen need an Effective Screen Area of", round(EoP_Worst(),3), "m<sup>2</sup>.")
    }
  })
  
  output$Warning_Text <- renderText({
    Warning_Text <- ""
    if(input$EoP_time < 10){
      Warning_Text <- paste("Warning: 10 minutes is the default time given for fish to escape the screen face and should not be lowered in most situations.")}
    Warning_Text
  })
  
  Approach_velocity <- reactive({
    EoP_func(input$EoP_time*60, EoP_Worst_Group())
  })
  
  output$Approach_velocity <- renderText({
    if(is.numeric(input$EoP_flowrate) & !is.null(EoP_Group())){
      paste("The design approach velocity is", signif(Approach_velocity(), digits = 2), "m/s.")
    }
  })
  
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    paste0(names(x), ": ", format(x), collapse = "<br />")
  }
  
  
  EoP_PlotData %>%
    ggvis(x= ~Flow, y=~EffectiveScreenArea, stroke = ~Group) %>%
    layer_lines(strokeWidth := 2.5) %>%
    layer_points(size := 40, opacity := 0) %>%
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
    add_tooltip(all_values, "hover") %>%
    bind_shiny("ggvis")
}


enableBookmarking(store = "url")

shinyApp( ui = ui, server = server)


# runApp("app.r")