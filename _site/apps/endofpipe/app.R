library(shiny)
library(shinythemes)
library(ggplot2)
library(scales)

# change plot fonts from defaults
library(showtext)
font.add.google("Lato","lato")
showtext.auto()

options(scipen=5)

# import K and b values for each group of fish (used in equations)
data <- read.csv("GroupVariables.csv")
rownames(data)<-data$Group
data$Group <- NULL

# import list of common and scientific names and their respective lengths
FishList <- read.csv("FishList.csv", stringsAsFactors=FALSE)
FishList$GroupName <- as.character(FishList$GroupName)

Velocity = data.frame(seq(from=0.1, to=10, by=0.01))
colnames(Velocity)[1] <- "Velocity"
Flow <- seq(from=0, to=1, by=0.0005)


########################
##   User Interface   ##
########################

ui <- function(request){
  (fluidPage(
    
    # Add script to resize iframe automatically
    # Script from here: https://groups.google.com/forum/#!topic/shiny-discuss/cFpn3UcZTvQ
    tags$head(includeScript("iframeResizer.contentWindow.min.js")),
    
    theme = shinytheme("cosmo"),
    
    sidebarLayout(
      sidebarPanel(
        helpText("Need help? Visit the ",
                 a(href="http://www.fishprotectiontools.ca/endofpipe-manual.html",target="_blank", "Manual"), align = "center"
        ),

        radioButtons("EoP_Selecter", label = "Select fish by:", 
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
        
        numericInput("EoP_flowrate", label = "Intake flow rate (L/s):", min = 0, max = 1000, value = 0, step = 0.5),
        
        checkboxInput("Mode",label = "Show advanced options", value = FALSE),
        
        # Advanced mode inputs:
        conditionalPanel("input.Mode == true",
                         selectInput("EoP_Proportion",
                                     label = "Proportion of fish protected",
                                     choices = c("97.5%", "87.5%", "50.0%", "12.5%", "2.5%"),
                                     selected = "97.5%")
        ),
        
        conditionalPanel("input.Mode == true",
                         sliderInput("EoP_l", label = "Fish length (mm):", min = 25, max = 1000, value = 2.5, step = 5)
        ),
        
        conditionalPanel("input.Mode == true",
                         sliderInput("EoP_time", label = "Time to escape screen face (min):", min = 1, max = 30, value = 10, step = 0.5)
        )
        
      ),    # close sidebarPanel
      
      mainPanel(
        plotOutput("EoP_Plot", height = "auto"),
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
      tempGroup <- 0}
    if(input$EoP_Selecter==1){
      tempGroup <- (FishList[which(FishList$CommonName==input$EoP_CName),"GroupName"])}
    if(input$EoP_Selecter==2){(
      tempGroup <-FishList[which(FishList$ScientificName==input$EoP_SName),"GroupName"])}
    if(input$EoP_Selecter==4){
      tempGroup <- 0}
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
  
  # Sloppy code that creates a dataframe containing the velocity vs distance data based on limits of swim time
  EoP_PlotData <- reactive({
    EoP_dataSet <- data.frame(Flow)
    EoP_dataSet$Flow_l_per_s <- signif(EoP_dataSet$Flow * 1000, digits = 5)
    
    TempVelocity <- EoP_func(input$EoP_time*60, "Salmon & Walleye")
    EoP_dataSet$Salmon_Walleye_Area <- EoP_dataSet$Flow / TempVelocity
    
    TempVelocity <- EoP_func(input$EoP_time*60, "Catfish & Sunfish")
    EoP_dataSet$Catfish_Sunfish_Area <- EoP_dataSet$Flow / TempVelocity
    
    TempVelocity <- EoP_func(input$EoP_time*60, "Sturgeon")
    EoP_dataSet$Sturgeon_Area <- EoP_dataSet$Flow / TempVelocity
    
    TempVelocity <- EoP_func(input$EoP_time*60, "Pike (derived)")    
    EoP_dataSet$Pike_Area <- EoP_dataSet$Flow / TempVelocity
    
    TempVelocity <- EoP_func(input$EoP_time*60, "Eel") 
    EoP_dataSet$Eel_Area <- EoP_dataSet$Flow / TempVelocity
    
    TempVelocity <- EoP_func(input$EoP_time*60, "Herring") 
    EoP_dataSet$Herring_Area <- EoP_dataSet$Flow / TempVelocity
    
    EoP_dataSet
  })
  

  # find the worst performing group value for flow caculation
  EoP_Worst <- reactive({
    EoP_Weakest = 0
    if(input$EoP_Selecter == 0 & input$EoP_flowrate > 0 & input$CatfishSunfish==TRUE &
       EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == input$EoP_flowrate), "Catfish_Sunfish_Area"] > EoP_Weakest){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == input$EoP_flowrate), "Catfish_Sunfish_Area"]}
    if(input$EoP_Selecter == 0 & input$EoP_flowrate > 0 & input$Eel==TRUE &
       EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == input$EoP_flowrate), "Eel_Area"] > EoP_Weakest){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == input$EoP_flowrate), "Eel_Area"]}
    if(input$EoP_Selecter == 0 & input$EoP_flowrate > 0 & input$Herring==TRUE &
       EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == input$EoP_flowrate), "Herring_Area"] > EoP_Weakest){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == input$EoP_flowrate), "Herring_Area"]}
    if(input$EoP_Selecter == 0 & input$EoP_flowrate > 0 & input$Pike==TRUE &
       EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == input$EoP_flowrate), "Pike_Area"] > EoP_Weakest){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == input$EoP_flowrate), "Pike_Area"]}
    if(input$EoP_Selecter == 0 & input$EoP_flowrate > 0 & input$SalmonWalleye==TRUE &
       EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == input$EoP_flowrate), "Salmon_Walleye_Area"] > EoP_Weakest){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == input$EoP_flowrate), "Salmon_Walleye_Area"]}
    if(input$EoP_Selecter == 0 & input$EoP_flowrate > 0 & input$Sturgeon==TRUE &
       EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == input$EoP_flowrate), "Sturgeon_Area"] > EoP_Weakest){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == input$EoP_flowrate), "Sturgeon_Area"]}
    if(input$EoP_Selecter>0 & input$EoP_flowrate>0 & EoP_Group() == "Catfish & Sunfish"){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == input$EoP_flowrate), "Catfish_Sunfish_Area"]}
    if(input$EoP_Selecter>0 & input$EoP_flowrate>0 & EoP_Group() == "Eel"){ 
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == input$EoP_flowrate), "Eel_Area"]}
    if(input$EoP_Selecter>0 & input$EoP_flowrate>0 & EoP_Group() == "Herring"){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == input$EoP_flowrate), "Herring_Area"]}
    if(input$EoP_Selecter>0 & input$EoP_flowrate>0 & EoP_Group() == "Pike (derived)"){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == input$EoP_flowrate), "Pike_Area"]}
    if(input$EoP_Selecter>0 & input$EoP_flowrate>0 & EoP_Group() == "Salmon & Walleye"){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == input$EoP_flowrate), "Salmon_Walleye_Area"]}
    if(input$EoP_Selecter>0 & input$EoP_flowrate>0 & EoP_Group() == "Sturgeon"){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == input$EoP_flowrate), "Sturgeon_Area"]}
    if(input$EoP_Selecter == 4 & input$EoP_flowrate > 0){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == input$EoP_flowrate), "Eel_Area"]}
    if(input$EoP_Selecter == 4 & input$EoP_flowrate > 0 & 
       EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == input$EoP_flowrate), "Sturgeon_Area"] >EoP_Weakest){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == input$EoP_flowrate), "Sturgeon_Area"]}
    EoP_Weakest
  })
 
  EoP_Worst_Group <- reactive({
    EoP_Weakest = -1
    Worst_Group <- "Eel"
    
    if(input$EoP_Selecter == 0 & input$CatfishSunfish==TRUE & EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == 10), "Catfish_Sunfish_Area"] > EoP_Weakest){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == 10), "Catfish_Sunfish_Area"]
      Worst_Group <- "Catfish & Sunfish"}
    
    if(input$EoP_Selecter == 0 & input$Eel==TRUE & EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == 10), "Eel_Area"] > EoP_Weakest){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == 10), "Eel_Area"]
      Worst_Group <- "Eel"}
    
    if(input$EoP_Selecter == 0 & input$Herring==TRUE & EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == 10), "Herring_Area"] > EoP_Weakest){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == 10), "Herring_Area"]
      Worst_Group <- "Herring"}
    
    if(input$EoP_Selecter == 0 & input$Pike==TRUE & EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == 10), "Pike_Area"] > EoP_Weakest){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == 10), "Pike_Area"]
      Worst_Group <- "Pike"}
    
    if(input$EoP_Selecter == 0 & input$SalmonWalleye==TRUE & EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == 10), "Salmon_Walleye_Area"] > EoP_Weakest){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == 10), "Salmon_Walleye_Area"]
      Worst_Group <- "Salmon & Walleye"}
  
    if(input$EoP_Selecter == 0 & input$Sturgeon==TRUE & EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == 10), "Sturgeon_Area"] > EoP_Weakest){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == 10), "Sturgeon_Area"]
      Worst_Group <- "Sturgeon"}
    
    if(input$EoP_Selecter>0 & EoP_Group() == "Catfish & Sunfish"){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == 10), "Catfish_Sunfish_Area"]
      Worst_Group <- "Catfish & Sunfish"}
    
    if(input$EoP_Selecter>0 & EoP_Group() == "Eel"){ 
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == 10), "Eel_Area"]
      Worst_Group <- "Eel"}
    
    if(input$EoP_Selecter>0 & EoP_Group() == "Herring"){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == 10), "Herring_Area"]
      Worst_Group <- "Herring"}
    
    if(input$EoP_Selecter>0 & EoP_Group() == "Pike (derived)"){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == 10), "Pike_Area"]
      Worst_Group <- "Pike"}
    
    if(input$EoP_Selecter>0 & EoP_Group() == "Salmon & Walleye"){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == 10), "Salmon_Walleye_Area"]
      Worst_Group <- "Salmon & Walleye"}
    
    if(input$EoP_Selecter>0 & EoP_Group() == "Sturgeon"){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == 10), "Sturgeon_Area"]
      Worst_Group <- "Sturgeon"}
    
    if(input$EoP_Selecter == 4){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == 10), "Eel_Area"]
      Worst_Group <- "Eel"}
    
    if(input$EoP_Selecter == 4 & EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == 10), "Sturgeon_Area"] >EoP_Weakest){
      EoP_Weakest <- EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == 10), "Sturgeon_Area"]
      Worst_Group <- "Sturgeon"}
    Worst_Group
  })
  

  
  output$EoP_Text <- renderUI({
    if(input$EoP_flowrate >0 & input$EoP_flowrate <= 1000 & input$EoP_flowrate %% 0.5 == 0 & is.na(as.numeric(input$EoP_flowrate))==FALSE){HTML(input$EoP_Proportion, " of", input$EoP_l, "mm fish in ", input$EoP_flowrate, "L/s flow with", input$EoP_time, "minutes to escape the screen need an Open Screen Area of", round(EoP_Worst(),3), "m<sup>2</sup>.")}})
  
  output$Warning_Text <- renderText({
    Warning_Text <- ""
    if(input$EoP_time < 10){
      Warning_Text <- paste("Warning: 10 minutes is the default time given for fish to escape the screen face and should not be lowered in most situations.")}
    Warning_Text
  })
  
  Approach_velocity <- reactive({EoP_func(input$EoP_time*60, EoP_Worst_Group())})
  output$Approach_velocity <- renderText({
    paste("The design approach velocity is", signif(Approach_velocity(), digits = 2), "m/s.")
  })
  
  
  EoP_Plot_X_Max <- reactive({
    temp_X_Max <- 1000
    if(input$EoP_flowrate > 0 & input$EoP_flowrate < 10){
      temp_X_Max <- 10}
    if(input$EoP_flowrate >= 10 & input$EoP_flowrate < 100){
      temp_X_Max <- 100}
    if(input$EoP_flowrate >= 100 & input$EoP_flowrate <= 1000){
      temp_X_Max <- 1000}
    temp_X_Max
  })
  
  EoP_Plot_Y_Max <- reactive({
    temp_Y_Max <- 30
    if(input$EoP_flowrate == 0){
      temp_Y_Max <- (EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == 1000), "Eel_Area"])}
    if(input$EoP_flowrate > 0 & input$EoP_flowrate < 10){
      temp_Y_Max <- (EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == 10), "Eel_Area"])}
    if(input$EoP_flowrate >= 10 & input$EoP_flowrate < 100){
      temp_Y_Max <- (EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == 100), "Eel_Area"])}
    if(input$EoP_flowrate >= 100 & input$EoP_flowrate <= 1000){
      temp_Y_Max <- (EoP_PlotData()[which(EoP_PlotData()$Flow_l_per_s == 1000), "Eel_Area"])}
    temp_Y_Max
  })
  
  output$EoP_Plot <- renderPlot(
    {validate(
      need(try(input$EoP_flowrate >= 0), "Please enter a positive number"),
      need(try(input$EoP_flowrate <= 1000), "Please enter a number below 1000"),
      need(try(input$EoP_flowrate %% 0.5 == 0), "Flow rate must be in 0.5 L increments")
    )
      ggplot(data=EoP_PlotData(), aes(Flow_l_per_s)) +
        theme_classic() +
        theme(axis.line.x = element_line(color="black", size = .5),
              axis.line.y = element_line(color="black", size = .5))+
        theme(legend.key = element_blank(), legend.position="top", legend.text=element_text(size=14, family="lato"))+
        theme(axis.text = element_text(size=14, family="lato"), axis.title = element_text(size=16, family="lato"))+
        theme(plot.margin=unit(c(1,1,0,0),"cm"))+
        scale_x_continuous(name = expression('Intake flow rate (L/s)'), expand = c(0,0) , limits = c(0,EoP_Plot_X_Max())) +
        scale_y_continuous(name = expression('Open Screen Area (m'^2*')'), expand = c(0,0), limits=c(0,EoP_Plot_Y_Max()), 
                           breaks = scales::pretty_breaks(n = 6))+
        
        #draw Group lines if fish are selected by All
        {if(input$EoP_Selecter==4) 
          geom_line(aes(y= Catfish_Sunfish_Area, colour = "Catfish & Sunfish  "), size = 1)}+
        {if(input$EoP_Selecter==4) 
          geom_line(aes(y= Eel_Area, colour = "Eel  "), size = 1)}+
        {if(input$EoP_Selecter==4)
          geom_line(aes(y= Herring_Area, colour = "Herring  "), size = 1)}+
        {if(input$EoP_Selecter==4) 
          geom_line(aes(y= Pike_Area, colour = "Pike  "), size = 1)}+
        {if(input$EoP_Selecter==4) 
          geom_line(aes(y= Salmon_Walleye_Area, colour = "Salmon & Walleye  "), size = 1)}+
        {if(input$EoP_Selecter==4) 
          geom_line(aes(y= Sturgeon_Area, colour = "Sturgeon  "), size = 1)}+
        
        # draw Group lines if fish are selected by Groups
        {if(input$EoP_Selecter==0 & input$CatfishSunfish==TRUE) geom_line(aes(y= Catfish_Sunfish_Area, colour = "Catfish & Sunfish  "), size = 1)}+
        {if(input$EoP_Selecter==0 & input$Eel==TRUE)            geom_line(aes(y= Eel_Area, colour = "Eel  "), size = 1)}+
        {if(input$EoP_Selecter==0 & input$Herring==TRUE)        geom_line(aes(y= Herring_Area, colour = "Herring  "), size = 1)}+
        {if(input$EoP_Selecter==0 & input$Pike==TRUE)           geom_line(aes(y= Pike_Area, colour = "Pike  "), size = 1)}+
        {if(input$EoP_Selecter==0 & input$SalmonWalleye==TRUE)  geom_line(aes(y= Salmon_Walleye_Area, colour = "Salmon & Walleye  "), size = 1)}+
        {if(input$EoP_Selecter==0 & input$Sturgeon==TRUE)       geom_line(aes(y= Sturgeon_Area, colour = "Sturgeon  "), size = 1)}+
        
        # draw group lines if fish are selected by Species
        {if(input$EoP_Selecter>0 & EoP_Group() == "Catfish & Sunfish")
          geom_line(aes(y= Catfish_Sunfish_Area, colour = "Catfish & Sunfish  "), size = 1)}+
        {if(input$EoP_Selecter>0 & EoP_Group() == "Eel")
          geom_line(aes(y= Eel_Area, colour = "Eel  "), size = 1)}+
        {if(input$EoP_Selecter>0 & EoP_Group() == "Herring")
          geom_line(aes(y= Herring_Area, colour = "Herring  "), size = 1)}+
        {if(input$EoP_Selecter>0 & EoP_Group() == "Pike (derived)")
          geom_line(aes(y= Pike_Area, colour = "Pike  "), size = 1)}+
        {if(input$EoP_Selecter>0 & EoP_Group() == "Salmon & Walleye")
          geom_line(aes(y= Salmon_Walleye_Area, colour = "Salmon & Walleye  "), size = 1)}+
        {if(input$EoP_Selecter>0 & EoP_Group() == "Sturgeon")
          geom_line(aes(y= Sturgeon_Area, colour = "Sturgeon  "), size = 1)}+
        
        # draw estimate lines for slowest group
        {if(input$EoP_flowrate>0) 
          geom_segment(aes(x = input$EoP_flowrate, y = 0, xend = input$EoP_flowrate, yend = EoP_Worst()), linetype = "dashed", size = 0.8)}+
        {if(input$EoP_flowrate>0) 
          geom_segment(aes(x = 0, y = EoP_Worst(), xend = input$EoP_flowrate, yend = EoP_Worst()), linetype = "dashed", size = 0.8)}+
        
        # Add legend
        scale_colour_manual("",
                            breaks=c("Catfish & Sunfish  ", "Eel  ", "Herring  ", "Pike  ", "Salmon & Walleye  ", "Sturgeon  "),
                            values = c("Catfish & Sunfish  "= "#E41A1C", 
                                       "Eel  "= "#377EB8",
                                       "Herring  "= "#4DAF4A",
                                       "Pike  "= "#984EA3",
                                       "Salmon & Walleye  "= "#FF7F00",
                                       "Sturgeon  "= "#A65628"))
    },
    height = function() {
      session$clientData$output_EoP_Plot_width*0.6
    }
  )
  
  
}


enableBookmarking(store = "url")

shinyApp( ui = ui, server = server)


# runApp("app.r")