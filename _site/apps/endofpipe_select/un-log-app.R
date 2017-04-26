library(shiny)
library(shinythemes)
library(ggplot2)
library(scales)

options(scipen=5)

#import K and b values for each group of fish (used in equations)
data <- read.csv("GroupVariables.csv")
rownames(data)<-data$Group
data$Group <- NULL

#import list of common and scientific names and their respective lengths
FishList <- read.csv("FishList.csv", stringsAsFactors=FALSE)
FishList$GroupName <- as.character(FishList$GroupName)

Velocity = seq(from=0.1, to=10, by=0.01)
Flow <- seq(from=.001, to=1, by=0.001)


########################
##   User Interface   ##
########################

ui <- (fluidPage(
  
  #Add Google anaylytics script to track application usage
  tags$head(includeScript("google-analytics.js")),
  
  theme = shinytheme("cosmo"),
  
  #Logo title
  titlePanel(title=div(img(src='DFOwordmark.png', align = "right", width = 400), ""), windowTitle = "Fish Swimming Performance Calculator"),
  br(),
  br(),
  br(),
  
  #add favicon
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),

  navbarPage("Fish Swimming Performance Tools:",
             
             ###############################
             ##  Swim Speed vs Swim Time  ##
             ###############################
             
             tabPanel("Swim Speed & Swim Time",
                      sidebarLayout(
                        sidebarPanel(
                          
                          radioButtons("Selecter", label = "Select fish by:",
                                       choices = list("Group" = 0, "Common name" = 1, "Scientific name" = 2), selected=0),
                          
                          conditionalPanel("input.Selecter == '0'",selectInput("Group", 
                                                                               label = "Select group",
                                                                               choices = list("Catfish & Sunfish", "Eel", "Herring", "Salmon & Walleye", "Sturgeon",
                                                                                              "Pike (derived)"),
                                                                               selected = "Salmon & Walleye")),
                          
                          conditionalPanel("input.Selecter == '1'",selectInput("CName", 
                                                                               label = "Select species",
                                                                               choices = sort(FishList$CommonName),
                                                                               selected = "Brook trout")),
                          
                          conditionalPanel("input.Selecter == '2'",selectInput("SName", 
                                                                               label = "Select species",
                                                                               choices = sort(FishList$ScientificName),
                                                                               selected = "Salvelinus fontinalis")),
                          sliderInput("l", 
                                      label = "Fish length (mm):",
                                      min = 25, max = 1000, value = c(2.5), step = 5),
                          
                          #Add checkboxes for to disable log scale on x and y axes                            
#                          checkboxInput("Logx",label = "Swim time on log scale", value = TRUE),
#                          checkboxInput("Logy",label = "Swim speed on log scale", value = TRUE),
                          
                          radioButtons("Calculate", label = "Calculations:",
                                       choices = list("None" = 0, "Swim speed" = 1, "Swim time" = 2)),
                          
                          conditionalPanel("input.Calculate == '1'",
                                           textInput("ST", 
                                                     label = "Swim time in seconds (3-1800):",
                                                     value = "60")),
                          
                          conditionalPanel("input.Calculate == '1'",
                                           checkboxInput("STEst", label = "Plot estimates", value = TRUE)),
                          
                          conditionalPanel("input.Calculate == '2'",
                                           textInput("SS", 
                                                     label = "Swim speed in m/s",
                                                     value = "0.3")),
                          
                          conditionalPanel("input.Calculate == '2'",
                                           checkboxInput("SSEst", label = "Plot estimates", value = TRUE)),
                          
                          #Conditional panels if Swim distance & water velocity is selected
                          
                          div(style = "margin-top: 30px;", HTML("Based on: <a href='http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2016/2016_002-eng.pdf'>Katopodis & Gervais 2016</a>")),
                          div(style = "margin-top: 10px;", HTML("Coded by: <a href='mailto:richard.dirocco@dfo-mpo.gc.ca'>R. Di Rocco</a>"))
                        ),
                        mainPanel(
                          h1(textOutput("GroupTitle")),
                          plotOutput("Plot0", height = "auto"),
                          br(),
                          conditionalPanel("input.Calculate == '1'", 
                                           h2("Estimates"),
                                           textOutput("SpeedText5"),
                                           textOutput("SpeedText25"),
                                           textOutput("SpeedText50"),
                                           textOutput("SpeedText75"),
                                           textOutput("SpeedText95")
                          ),
                          conditionalPanel("input.Calculate == '2'", 
                                           h2("Estimates"),
                                           textOutput("TimeText5"),
                                           textOutput("TimeText25"),
                                           textOutput("TimeText50"),
                                           textOutput("TimeText75"),
                                           textOutput("TimeText95")
                          ),
                          align = "center"
                        )    #close mainPanel
                      )     #close sidebarLayout
             ),     #close tabPanel 
             
             #######################################
             ##  Water Velocity vs Swim Distance  ##
             #######################################
             
             tabPanel("Swim Distance & Water Velocity",
                      sidebarLayout(
                        sidebarPanel(
                          
                          radioButtons("VvD_Selecter", label = "Select fish by:",
                                       choices = list("Group" = 0, "Common name" = 1, "Scientific name" = 2), selected=0),
                          
                          conditionalPanel("input.VvD_Selecter == '0'",selectInput("VvD_Group", 
                                                                                   label = "Select group",
                                                                                   choices = list("Catfish & Sunfish", "Eel", "Herring", "Salmon & Walleye", "Sturgeon",
                                                                                                  "Pike (derived)"),
                                                                                   selected = "Salmon & Walleye")),
                          
                          conditionalPanel("input.VvD_Selecter == '1'",selectInput("VvD_CName", 
                                                                                   label = "Select species",
                                                                                   choices = sort(FishList$CommonName),
                                                                                   selected = "Brook trout")),
                          
                          conditionalPanel("input.VvD_Selecter == '2'",selectInput("VvD_SName", 
                                                                                   label = "Select species",
                                                                                   choices = sort(FishList$ScientificName),
                                                                                   selected = "Salvelinus fontinalis")),
                          sliderInput("VvD_l", 
                                      label = "Fish length (mm):",
                                      min = 25, max = 1000, value = c(2.5), step = 5),
                          
                          #Add checkboxes for to disable log scale on x and y axes 
#                          checkboxInput("Logx2",label = "Swim distance on log scale", value = TRUE),
#                          checkboxInput("Logy2",label = "Water velocity on log scale", value = TRUE),
                          
                          radioButtons("Calculate2", label = "Calculations:",
                                       choices = list("None" = 0, "Swim distance" = 1, "Water velocity" = 2)),
                          
                          conditionalPanel("input.Calculate2 == '1'",
                                           textInput("WV", 
                                                     label = "Water velocity in m/s:",
                                                     value = "0.15")),
                          
                          conditionalPanel("input.Calculate2 == '1'",
                                           checkboxInput("WVEst", label = "Plot estimates", value = TRUE)),
                          
                          conditionalPanel("input.Calculate2 == '2'",
                                           textInput("SD", 
                                                     label = "Swim distance in meters:",
                                                     value = "10")),
                          
                          conditionalPanel("input.Calculate2 == '2'",
                                           checkboxInput("SDEst", label = "Plot estimates", value = TRUE)),
                          div(style = "margin-top: 30px;", HTML("Based on: <a href='http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2016/2016_002-eng.pdf'>Katopodis & Gervais 2016</a>")),
                          div(style = "margin-top: 10px;", HTML("Coded by: <a href='mailto:richard.dirocco@dfo-mpo.gc.ca'>R. Di Rocco</a>"))
                        ),
                        
                        mainPanel(
                          h1(textOutput("GroupTitle2")),        
                          plotOutput("Plot1", height = "auto"),
                          br(),                
                          conditionalPanel("input.Calculate2 == '1'", 
                                           h2("Estimates"),
                                           textOutput("DistanceText5"),
                                           textOutput("DistanceText25"),
                                           textOutput("DistanceText50"),
                                           textOutput("DistanceText75"),
                                           textOutput("DistanceText95")),
                          
                          conditionalPanel("input.Calculate2 == '2'",
                                           h2("Estimates"),
                                           textOutput("VelocityText5"),
                                           textOutput("VelocityText25"),
                                           textOutput("VelocityText50"),
                                           textOutput("VelocityText75"),
                                           textOutput("VelocityText95")),
                          align = "center"
                        )    #close mainpanel
                      )     #close sidebarLayout
             ),    #close tabpanel
             
             ##########################################
             ##  End-of-Pipe Screen Size Calculator  ##
             ##########################################
             
             tabPanel("End-of-Pipe Screen Size",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("This tool calculates the screen area required to prevent impingement of 97.5% of fish based on the factors below."),
                          radioButtons("EoP_Selecter", label = "Select fish by:",
                                       choices = list("Group" = 0, "Common name" = 1, "Scientific name" = 2), selected=0),
                          
                          conditionalPanel("input.EoP_Selecter == '0'",
                                           h5(strong("Select groups:")),
                                           checkboxInput("CatfishSunfish",label = "Catfish & Sunfish", value = TRUE),
                                           checkboxInput("Eel", label = "Eel", value = TRUE),
                                           checkboxInput("Herring", label="Herring", value=TRUE),
                                           checkboxInput("Pike", label="Pike", value =TRUE),
                                           checkboxInput("SalmonWalleye", label = "Salmon & Walleye", value = TRUE),
                                           checkboxInput("Sturgeon", label = "Sturgeon", value = TRUE)
                          ),
                          
                          conditionalPanel("input.EoP_Selecter == '1'",selectInput("EoP_CName", 
                                                                                   label = "Select species",
                                                                                   choices = sort(FishList$CommonName),
                                                                                   selected = "Brook trout")),
                          
                          conditionalPanel("input.EoP_Selecter == '2'",selectInput("EoP_SName", 
                                                                                   label = "Select species",
                                                                                   choices = sort(FishList$ScientificName),
                                                                                   selected = "Salvelinus fontinalis")),
                          sliderInput("EoP_l", 
                                      label = "Fish length (mm):",
                                      min = 25, max = 1000, value = 2.5, step = 5),
                          
                          
                          sliderInput("EoP_time", 
                                      label = "Time (min):",
                                      min = 1, max = 30, value = 10, step = 0.5),
                          helpText("Description: The amount of time required for a fish to escape the face of the screen (default: 10 min)."),
                          
                          sliderInput("EoP_flowrate", 
                                      label = "Flow rate (L/s):",
                                      min = 0, max = 1000, value = 0, step = 5)
                          
                        ),    #close sidebarPanel
                        mainPanel(
                          h1("End-of-Pipe Screen Size"),
                          plotOutput("EoP_Plot", height = "auto"),
                          br(),
                          htmlOutput("EoP_Text"),
                          align = "center"
                        )    #close mainpanel
                      )      #close sidebarLayout
             ),    #close tabpanel
             
             # edit html so tab bar is left justified
             tags$head(tags$style('
                                  nav .container:first-child {
                                  margin-left:10px; width: 100%;
                                  }'))
  )       #close navbarPage
             ))       #close fluidpage



#######################
##       Server      ##
#######################

server <- function(input, output, session){ 
  
  #Set gravitational acceleration constant
  g=9.81
  
  ##########################################
  ##  End-of-Pipe Screen Size Calculator  ##
  ##########################################
  
  
  #Update group based on species selected
  EoP_Group <- reactive({
    if(input$EoP_Selecter==0){
      tempGroup <- 0}
    if(input$EoP_Selecter==1){
      tempGroup <- (FishList[which(FishList$CommonName==input$EoP_CName),"GroupName"])}
    if(input$EoP_Selecter==2){(
      tempGroup <-FishList[which(FishList$ScientificName==input$EoP_SName),"GroupName"])}
    tempGroup
  })
  
  #Update length slider scale based on group selected
  observe({
    if(input$EoP_Selecter==0){
      scale <- data[input$Group,"Max_l"]}
    if(input$EoP_Selecter==1){
      scale <- FishList[which(FishList$CommonName==input$EoP_CName),"MaxSize"]}
    if(input$EoP_Selecter==2){
      scale <- FishList[which(FishList$ScientificName==input$EoP_SName),"MaxSize"]}
    updateSliderInput(session, "EoP_l", min = 25, max = scale)
  })
  
  #Create functions to determine swimming speed (m/s) based on time (seconds)
  EoP_U    = function (t,EoP_Group){data[EoP_Group,"k"]*sqrt(g*input$EoP_l/1000)*((sqrt(input$EoP_l/1000/g))^-data[EoP_Group,"b"])*t^data[EoP_Group,"b"]}
  EoP_U75U = function (t,EoP_Group){data[EoP_Group,"X75U_k"]*sqrt(g*input$EoP_l/1000)*((sqrt(input$EoP_l/1000/g))^-data[EoP_Group,"X75U_b"])*t^data[EoP_Group,"X75U_b"]}
  EoP_U75L = function (t,EoP_Group){data[EoP_Group,"X75L_k"]*sqrt(g*input$EoP_l/1000)*((sqrt(input$EoP_l/1000/g))^-data[EoP_Group,"X75L_b"])*t^data[EoP_Group,"X75L_b"]}
  EoP_U95U = function (t,EoP_Group){data[EoP_Group,"X95U_k"]*sqrt(g*input$EoP_l/1000)*((sqrt(input$EoP_l/1000/g))^-data[EoP_Group,"X95U_b"])*t^data[EoP_Group,"X95U_b"]}
  EoP_U95L = function (t,EoP_Group){data[EoP_Group,"X95L_k"]*sqrt(g*input$EoP_l/1000)*((sqrt(input$EoP_l/1000/g))^-data[EoP_Group,"X95L_b"])*t^data[EoP_Group,"X95L_b"]}
  
  #Sloppy code that creates a dataframe containing the velocity vs distance data based on limits of swim time
  EoP_PlotData <- reactive({
    EoP_dataSet <- data.frame(Flow)
    EoP_dataSet$Flow_l_per_s <- as.integer(EoP_dataSet$Flow * 1000)
    
    TempVelocity <- EoP_U95L(input$EoP_time*60, "Salmon & Walleye")
    EoP_dataSet$Salmon_Walleye_Area <- EoP_dataSet$Flow / TempVelocity
    
    TempVelocity <- EoP_U95L(input$EoP_time*60, "Catfish & Sunfish")
    EoP_dataSet$Catfish_Sunfish_Area <- EoP_dataSet$Flow / TempVelocity
    
    TempVelocity <- EoP_U95L(input$EoP_time*60, "Sturgeon")
    EoP_dataSet$Sturgeon_Area <- EoP_dataSet$Flow / TempVelocity
    
    TempVelocity <- EoP_U95L(input$EoP_time*60, "Pike (derived)")    
    EoP_dataSet$Pike_Area <- EoP_dataSet$Flow / TempVelocity
    
    TempVelocity <- EoP_U95L(input$EoP_time*60, "Eel") 
    EoP_dataSet$Eel_Area <- EoP_dataSet$Flow / TempVelocity
    
    TempVelocity <- EoP_U95L(input$EoP_time*60, "Herring") 
    EoP_dataSet$Herring_Area <- EoP_dataSet$Flow / TempVelocity
    
    EoP_dataSet
  })
  
  output$EoP_Text <- renderUI({
    if(input$EoP_flowrate >0){HTML(input$EoP_l, "mm fish in ", input$EoP_flowrate, "L/s flow require an Open Screen Area of", round(EoP_Worst(),2), "m<sup>2</sup>")}})
  
  #find the worst performing group for flow caculation
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
    EoP_Weakest
  })
  
  
  output$EoP_Plot <- renderPlot(
    {ggplot(data=EoP_PlotData(), aes(Flow_l_per_s)) +
        theme_classic() +
        theme(axis.line.x = element_line(color="black", size = .5),
              axis.line.y = element_line(color="black", size = .5))+
        theme(legend.key = element_blank(), legend.position="top", legend.text=element_text(size=16))+
        theme(axis.text = element_text(size=14), axis.title = element_text(size=16))+
        theme(plot.margin=unit(c(1,1,0,0),"cm"))+
        scale_x_continuous(name = expression('Flow (L/s)'), expand = c(0,0) , limits = c(0,1000)) +
        scale_y_continuous(name = expression('Open Screen Area (m'^2*')'), expand = c(0,0), breaks = scales::pretty_breaks(n = 6))+
        
        #draw Group lines if fish are selected by Groups
        {if(input$EoP_Selecter==0 & input$CatfishSunfish==TRUE) geom_line(aes(y= Catfish_Sunfish_Area, colour = "Catfish & Sunfish"), size = 1)}+
        {if(input$EoP_Selecter==0 & input$Eel==TRUE)            geom_line(aes(y= Eel_Area, colour = "Eel"), size = 1)}+
        {if(input$EoP_Selecter==0 & input$Herring==TRUE)        geom_line(aes(y= Herring_Area, colour = "Herring"), size = 1)}+
        {if(input$EoP_Selecter==0 & input$Pike==TRUE)           geom_line(aes(y= Pike_Area, colour = "Pike"), size = 1)}+
        {if(input$EoP_Selecter==0 & input$SalmonWalleye==TRUE)  geom_line(aes(y= Salmon_Walleye_Area, colour = "Salmon & Walleye"), size = 1)}+
        {if(input$EoP_Selecter==0 & input$Sturgeon==TRUE)       geom_line(aes(y= Sturgeon_Area, colour = "Sturgeon"), size = 1)}+
        
        #draw group lines if fish are selected by Species
        {if(input$EoP_Selecter>0 & EoP_Group() == "Catfish & Sunfish")
          geom_line(aes(y= Catfish_Sunfish_Area, colour = "Catfish & Sunfish"), size = 1)}+
        {if(input$EoP_Selecter>0 & EoP_Group() == "Eel")
          geom_line(aes(y= Eel_Area, colour = "Eel"), size = 1)}+
        {if(input$EoP_Selecter>0 & EoP_Group() == "Herring")
          geom_line(aes(y= Herring_Area, colour = "Herring"), size = 1)}+
        {if(input$EoP_Selecter>0 & EoP_Group() == "Pike (derived)")
          geom_line(aes(y= Pike_Area, colour = "Pike"), size = 1)}+
        {if(input$EoP_Selecter>0 & EoP_Group() == "Salmon & Walleye")
          geom_line(aes(y= Salmon_Walleye_Area, colour = "Salmon & Walleye"), size = 1)}+
        {if(input$EoP_Selecter>0 & EoP_Group() == "Sturgeon")
          geom_line(aes(y= Sturgeon_Area, colour = "Sturgeon"), size = 1)}+
        
        #draw estimate lines for slowest group
        {if(input$EoP_flowrate>0) 
          geom_segment(aes(x = input$EoP_flowrate, y = 0, xend = input$EoP_flowrate, yend = EoP_Worst()), linetype = "dashed", size = 0.8)}+
        {if(input$EoP_flowrate>0) 
          geom_segment(aes(x = 0, y = EoP_Worst(), xend = input$EoP_flowrate, yend = EoP_Worst()), linetype = "dashed", size = 0.8)}+
        
        #Add legend
        scale_colour_manual("",
                            breaks=c("Catfish & Sunfish", "Eel", "Herring", "Pike", "Salmon & Walleye", "Sturgeon"),
                            values = c("Catfish & Sunfish"= "#E41A1C", 
                                       "Eel"= "#377EB8",
                                       "Herring"= "#4DAF4A",
                                       "Pike"= "#984EA3",
                                       "Salmon & Walleye"= "#FF7F00",
                                       "Sturgeon"= "#A65628"))
    },
    height = function() {
      session$clientData$output_EoP_Plot_width*0.5
    }
  )
  
  ###############################
  ##  Swim Speed vs Swim Time  ##
  ###############################
  
  #Update group based on species selected
  Group <- reactive({
    if(input$Selecter==0){
      tempGroup <- input$Group}
    if(input$Selecter==1){
      tempGroup <- (FishList[which(FishList$CommonName==input$CName),"GroupName"])}
    if(input$Selecter==2){(
      tempGroup <-FishList[which(FishList$ScientificName==input$SName),"GroupName"])}
    tempGroup
  })
  
  #Update length slider scale based on group selected
  observe({
    if(input$Selecter==0){
      scale <- data[input$Group,"Max_l"]}
    if(input$Selecter==1){
      scale <- FishList[which(FishList$CommonName==input$CName),"MaxSize"]}
    if(input$Selecter==2){
      scale <- FishList[which(FishList$ScientificName==input$SName),"MaxSize"]}
    updateSliderInput(session, "l", min = 25, max = scale)
  })
  
  #convert text input to number for calculations
  STnum <- reactive({
    temp <- as.numeric(input$ST)
    validate(
      need(!is.na(temp) & temp == c(3:1800),"Please enter number between 3 and 1800")
    )
    as.numeric(input$ST)
  })
  SSnum <- reactive({
    temp2 <- as.numeric(input$SS)
    validate(
      need(!is.na(temp2) & temp2 > 0,"Please enter positive number")
    )
    as.numeric(input$SS)
  })
  
  #Create functions to determine swimming speed (m/s) based on time (seconds)
  U    = function (t){data[Group(),"k"]*sqrt(g*input$l/1000)*((sqrt(input$l/1000/g))^-data[Group(),"b"])*t^data[Group(),"b"]}
  U75U = function (t){data[Group(),"X75U_k"]*sqrt(g*input$l/1000)*((sqrt(input$l/1000/g))^-data[Group(),"X75U_b"])*t^data[Group(),"X75U_b"]}
  U75L = function (t){data[Group(),"X75L_k"]*sqrt(g*input$l/1000)*((sqrt(input$l/1000/g))^-data[Group(),"X75L_b"])*t^data[Group(),"X75L_b"]}
  U95U = function (t){data[Group(),"X95U_k"]*sqrt(g*input$l/1000)*((sqrt(input$l/1000/g))^-data[Group(),"X95U_b"])*t^data[Group(),"X95U_b"]}
  U95L = function (t){data[Group(),"X95L_k"]*sqrt(g*input$l/1000)*((sqrt(input$l/1000/g))^-data[Group(),"X95L_b"])*t^data[Group(),"X95L_b"]}
  
  #create reactive to run function only once per user input
  U5Num <- reactive({U95U(STnum())})
  U25Num <- reactive({U75U(STnum())})
  U50Num <- reactive({U(STnum())})
  U75Num <- reactive({U75L(STnum())})
  U95Num <- reactive({U95L(STnum())})
  
  #Create functions to determine time (seconds) based on swimming speed (m/s)
  T50  = function (U){(((U/sqrt(g*input$l/1000))/data[Group(),"k"])^(-1/(abs(data[Group(),"b"]))))/(1/(sqrt(input$l/1000/g)))}
  T75U = function (U){(((U/sqrt(g*input$l/1000))/data[Group(),"X75U_k"])^(-1/(abs(data[Group(),"X75U_b"]))))/(1/(sqrt(input$l/1000/g)))}
  T75L = function (U){(((U/sqrt(g*input$l/1000))/data[Group(),"X75L_k"])^(-1/(abs(data[Group(),"X75L_b"]))))/(1/(sqrt(input$l/1000/g)))}
  T95U = function (U){(((U/sqrt(g*input$l/1000))/data[Group(),"X95U_k"])^(-1/(abs(data[Group(),"X95U_b"]))))/(1/(sqrt(input$l/1000/g)))}
  T95L = function (U){(((U/sqrt(g*input$l/1000))/data[Group(),"X95L_k"])^(-1/(abs(data[Group(),"X95L_b"]))))/(1/(sqrt(input$l/1000/g)))}
  
  #Create reactive to run function only once per user input
  T5Num <- reactive({T95U(SSnum())})
  T25Num <- reactive({T75U(SSnum())})
  T50Num <- reactive({T50(SSnum())})
  T75Num <- reactive({T75L(SSnum())})
  T95Num <- reactive({T95L(SSnum())})
  
  #Change alpha based on whether calculations are being made. This will fade the function lines
  Alpha <- reactive({
    if(input$Calculate == 1 | input$Calculate == 2){tempAlpha = 0.5}
    else tempAlpha = 1
    tempAlpha
  })
  
  #Create plot for speed vs time
  output$Plot0 <- renderPlot(
    {ggplot(data.frame(x = c(0, 2)), aes(x)) +
        
        #Customize appearance of plot
        theme_classic() +
        theme(axis.line.x = element_line(color="black", size = .5),
              axis.line.y = element_line(color="black", size = .5))+
        theme(legend.key = element_blank(), legend.position="top", legend.text=element_text(size=16))+
        theme(axis.text = element_text(size=14), axis.title = element_text(size=16))+
        
        #add function to plot
        stat_function(fun = U, aes(colour = "Mean performance"), size = 0.8, alpha=Alpha()) +
        stat_function(fun= U75U, aes(colour="75% prediction interval"), size = 0.8, alpha=Alpha()) +
        stat_function(fun= U75L, aes(colour="75% prediction interval"), size = 0.8, alpha=Alpha()) +
        stat_function(fun= U95U, aes(colour="95% prediction interval"), size = 0.8, alpha=Alpha()) +
        stat_function(fun= U95L, aes(colour="95% prediction interval"), size = 0.8, alpha=Alpha()) +
        
        #logic to switch scale depending on checkbox selection  
        {if(input$Logx==TRUE) scale_x_log10(name = "Swim time (s)", limits=c(3, 1800))
          else scale_x_continuous(name = "Swim time (s)", limits=c(3, 1800))}+
        {if(input$Logx==TRUE) annotation_logticks(base = 10, sides = "b")}+
        {if(input$Logy==TRUE) scale_y_log10(name = "Swim speed (m/s)", limits=c(.02, 14))
          else scale_y_continuous(name = "Swim speed (m/s)", limits=c(0,U95U(3)))}+
        {if(input$Logy==TRUE) annotation_logticks(base = 10, sides = "l")}+
        
        #add vertical line to plot for swim speed calculations
        {if(input$Calculate == '1') geom_vline(xintercept=STnum(), linetype = "dashed", size = 0.8)}+
        
        #add horizontal estimate lines for calculations
        {if(input$STEst == TRUE & input$Calculate == '1' & U5Num() >= 0.02 & U5Num() <= 14)
          geom_segment(aes(x = 3, y = U5Num(), xend = STnum(), yend = U5Num()), linetype = "dashed", colour="orangered", size = 0.8)}+
        {if(input$STEst == TRUE & input$Calculate == '1' & U25Num() >= 0.02 & U25Num() <= 14)
          geom_segment(aes(x = 3, y = U25Num(), xend = STnum(), yend = U25Num()), linetype = "dashed", colour="skyblue3", size = 0.8)}+
        {if(input$STEst == TRUE & input$Calculate == '1' & U50Num() >= 0.02 & U50Num() <= 14)
          geom_segment(aes(x = 3, y = U50Num(), xend = STnum(), yend = U50Num()), linetype = "dashed", colour="gray65", size = 0.8)}+
        {if(input$STEst == TRUE & input$Calculate == '1' & U75Num() >= 0.02 & U75Num() <= 14)
          geom_segment(aes(x = 3, y = U75Num(), xend = STnum(), yend = U75Num()), linetype = "dashed", colour="skyblue3", size = 0.8)}+
        {if(input$STEst == TRUE & input$Calculate == '1' & U95Num() >= 0.02 & U95Num() <= 14)
          geom_segment(aes(x = 3, y = U95Num(), xend = STnum(), yend = U95Num()), linetype = "dashed", colour="orangered", size = 0.8)}+
        
        #add horizontal line to plot for swim time calculations
        {if(input$Calculate == '2') geom_hline(yintercept=SSnum(), linetype = "dashed", size = 0.8)}+
        
        #add vertical estimate lines for calculations
        {if(input$SSEst == TRUE & input$Calculate == '2' & T5Num() >= 3 & T5Num() <= 1800)
          geom_segment(aes(x = T5Num(), y = 0.02, xend = T5Num(), yend = SSnum()), linetype = "dashed", colour="orangered", size = 0.8)}+
        {if(input$SSEst == TRUE & input$Calculate == '2' & T25Num() >= 3 & T25Num() <= 1800)
          geom_segment(aes(x = T25Num(), y = 0.02, xend = T25Num(), yend = SSnum()), linetype = "dashed", colour="skyblue3", size = 0.8)}+
        {if(input$SSEst == TRUE & input$Calculate == '2' & T50Num() >= 3 & T50Num() <= 1800)
          geom_segment(aes(x = T50Num(), y = 0.02, xend = T50Num(), yend = SSnum()), linetype = "dashed", colour="gray65", size = 0.8)}+
        {if(input$SSEst == TRUE & input$Calculate == '2' & T75Num() >= 3 & T75Num() <= 1800)
          geom_segment(aes(x = T75Num(), y = 0.02, xend = T75Num(), yend = SSnum()), linetype = "dashed", colour="skyblue3", size = 0.8)}+
        {if(input$SSEst == TRUE & input$Calculate == '2' & T95Num() >= 3 & T95Num() <= 1800)
          geom_segment(aes(x = T95Num(), y = 0.02, xend = T95Num(), yend = SSnum()), linetype = "dashed", colour="orangered", size = 0.8)}+
        
        #add legend to plot
        scale_colour_manual("",values = c("skyblue2","orangered", "gray65"), 
                            breaks=c("Mean performance","75% prediction interval","95% prediction interval"))
    },
    height = function() {
      session$clientData$output_Plot0_width*0.4
    }
    
  )
  
  #Create plot title text
  output$GroupTitle <- renderText({
    paste(Group(), "Group")
  })
  
  #Create output text for swim speed based on swim time
  output$SpeedText5 <- renderText({
    paste("2.5% of ", input$l, "mm", Group(), "can achieve ", signif(U5Num(), digits = 2), "m/s or better when swimming for ", STnum(), "s" )
  })
  output$SpeedText25 <- renderText({
    paste("12.5% of ", input$l, "mm", Group(), "can achieve ", signif(U25Num(), digits = 2), "m/s or better when swimming for ", STnum(), "s" )
  })
  output$SpeedText50 <- renderText({
    paste("50% of ", input$l, "mm", Group(), "can achieve ", signif(U50Num(), digits = 2), "m/s or better when swimming for ", STnum(), "s" )
  })
  output$SpeedText75 <- renderText({
    paste("87.5% of ", input$l, "mm", Group(), "can achieve ", signif(U75Num(), digits = 2), "m/s or better when swimming for ", STnum(), "s" )
  })
  output$SpeedText95 <- renderText({
    paste("97.5% of ", input$l, "mm", Group(), "can achieve ", signif(U95Num(), digits = 2), "m/s or better when swimming for ", STnum(), "s" )
  })
  
  #Create output text for Swim time based on swim speed
  output$TimeText5 <- renderText({
    if(T5Num()<3){
      TT5 <- paste("2.5% of ", input$l, "mm", Group(), "cannot swim", SSnum(), " m/s.")}
    if(T5Num()>=3 & T5Num()<=1800){
      TT5 <- paste("2.5% of ", input$l, "mm", Group(), " can swim", SSnum(), " m/s for at least ", signif(T5Num(), digits = 2), "s")}
    if(T5Num()>1800){
      TT5 <- paste("2.5% of ", input$l, "mm", Group(), " can swim", SSnum(), " m/s for longer than 1800 s")}
    TT5
  })
  
  output$TimeText25 <- renderText({
    if(T25Num()<3){
      TT25 <- paste("12.5% of ", input$l, "mm", Group(), "cannot swim", SSnum(), " m/s.")}
    if(T25Num()>=3 & T25Num()<=1800){
      TT25 <- paste("12.5% of ", input$l, "mm", Group(), " can swim", SSnum(), " m/s for at least ", signif(T25Num(), digits = 2), "s")}
    if(T25Num()>1800){
      TT25 <- paste("12.5% of ", input$l, "mm", Group(), " can swim", SSnum(), " m/s for longer than 1800 s")}
    TT25
  })
  
  output$TimeText50 <- renderText({
    if(T50Num()<3){
      TT50 <- paste("50% of ", input$l, "mm", Group(), "cannot swim", SSnum(), " m/s.")}
    if(T50Num()>=3 & T50Num()<=1800){
      TT50 <- paste("50% of ", input$l, "mm", Group(), " can swim", SSnum(), " m/s for at least ", signif(T50Num(), digits = 2), "s")}
    if(T50Num()>1800){
      TT50 <- paste("50% of ", input$l, "mm", Group(), " can swim", SSnum(), " m/s for longer than 1800 s")}
    TT50
  })
  
  output$TimeText75 <- renderText({
    if(T75Num()<3){
      TT75 <- paste("87.5% of ", input$l, "mm", Group(), "cannot swim", SSnum(), " m/s.")}
    if(T75Num()>=3 & T75Num()<=1800){
      TT75 <- paste("87.5% of ", input$l, "mm", Group(), " can swim", SSnum(), " m/s for at least ", signif(T75Num(), digits = 2), "s")}
    if(T75Num()>1800){
      TT75 <- paste("87.5% of ", input$l, "mm", Group(), " can swim", SSnum(), " m/s for longer than 1800 s")}
    TT75
  })
  
  output$TimeText95 <- renderText({
    if(T95Num()<3){
      TT95 <- paste("97.5% of ", input$l, "mm", Group(), "cannot swim", SSnum(), " m/s.")}
    if(T95Num()>=3 & T95Num()<=1800){
      TT95 <- paste("97.5% of ", input$l, "mm", Group(), " can swim", SSnum(), " m/s for at least ", signif(T95Num(), digits = 2), "s")}
    if(T95Num()>1800){
      TT95 <- paste("97.5% of ", input$l, "mm", Group(), " can swim", SSnum(), " m/s for longer than 1800 s")}
    TT95
  })
  
  #######################################
  ##  Water Velocity vs Swim Distance  ##
  #######################################
  
  #Update group based on species selected
  VvD_Group <- reactive({
    if(input$VvD_Selecter==0){
      tempVvD_Group <- input$VvD_Group}
    if(input$VvD_Selecter==1){
      tempVvD_Group <- (FishList[which(FishList$CommonName==input$VvD_CName),"GroupName"])}
    if(input$VvD_Selecter==2){(
      tempVvD_Group <-FishList[which(FishList$ScientificName==input$VvD_SName),"GroupName"])}
    tempVvD_Group
  })
  
  #Update length slider scale based on group selected
  observe({
    if(input$VvD_Selecter==0){
      scale <- data[input$VvD_Group,"Max_l"]}
    if(input$VvD_Selecter==1){
      scale <- FishList[which(FishList$CommonName==input$VvD_CName),"MaxSize"]}
    if(input$VvD_Selecter==2){
      scale <- FishList[which(FishList$ScientificName==input$VvD_SName),"MaxSize"]}
    updateSliderInput(session, "VvD_l", min = 25, max = scale)
  })
  
  #convert text input to number for calculations
  WVnum <- reactive({
    temp3 <- as.numeric(input$WV)
    validate(
      need(!is.na(temp3) & temp3 > 0,"Please enter a positive number")
    )
    as.numeric(input$WV)
  })
  SDnum <- reactive({
    temp4 <- as.numeric(input$SD)
    validate(
      need(!is.na(temp4) & temp4 > 0,"Please enter a positive number")
    )
    as.numeric(input$SD)
  })
  
  
  #Create functions to determine time (seconds) based on swimming speed (m/s)
  VvD_T50  = function (U){(((U/sqrt(g*input$VvD_l/1000))/data[VvD_Group(),"k"])^(-1/(abs(data[VvD_Group(),"b"]))))/(1/(sqrt(input$VvD_l/1000/g)))}
  VvD_T75U = function (U){(((U/sqrt(g*input$VvD_l/1000))/data[VvD_Group(),"X75U_k"])^(-1/(abs(data[VvD_Group(),"X75U_b"]))))/(1/(sqrt(input$VvD_l/1000/g)))}
  VvD_T75L = function (U){(((U/sqrt(g*input$VvD_l/1000))/data[VvD_Group(),"X75L_k"])^(-1/(abs(data[VvD_Group(),"X75L_b"]))))/(1/(sqrt(input$VvD_l/1000/g)))}
  VvD_T95U = function (U){(((U/sqrt(g*input$VvD_l/1000))/data[VvD_Group(),"X95U_k"])^(-1/(abs(data[VvD_Group(),"X95U_b"]))))/(1/(sqrt(input$VvD_l/1000/g)))}
  VvD_T95L = function (U){(((U/sqrt(g*input$VvD_l/1000))/data[VvD_Group(),"X95L_k"])^(-1/(abs(data[VvD_Group(),"X95L_b"]))))/(1/(sqrt(input$VvD_l/1000/g)))}
  
  #Create reactive to run function only once per user input
  VvD_T5Num <- reactive({VvD_T95U(SSnum())})
  VvD_T25Num <- reactive({VvD_T75U(SSnum())})
  VvD_T50Num <- reactive({VvD_T50(SSnum())})
  VvD_T75Num <- reactive({VvD_T75L(SSnum())})
  VvD_T95Num <- reactive({VvD_T95L(SSnum())})
  
  #calculate U (fish swim speed in m/s) from V (water velocity)
  VtoU50 = function(V){V/(1+data[VvD_Group(),"b"])}
  VtoU75U = function(V){V/(1+data[VvD_Group(),"X75U_b"])}
  VtoU75L = function(V){V/(1+data[VvD_Group(),"X75L_b"])}
  VtoU95U = function(V){V/(1+data[VvD_Group(),"X95U_b"])}
  VtoU95L = function(V){V/(1+data[VvD_Group(),"X75L_b"])}
  
  #Create functions to determine swim distance (m) based on water velocity (m/s)
  X50  = function (V){(data[VvD_Group(),"M"]*(V/(sqrt(g*input$VvD_l/1000)))^data[VvD_Group(),"a"])*(input$VvD_l/1000)}
  X75U  = function (V){(data[VvD_Group(),"X75U_M"]*(V/(sqrt(g*input$VvD_l/1000)))^data[VvD_Group(),"X75U_a"])*(input$VvD_l/1000)}
  X75L  = function (V){(data[VvD_Group(),"X75L_M"]*(V/(sqrt(g*input$VvD_l/1000)))^data[VvD_Group(),"X75L_a"])*(input$VvD_l/1000)} 
  X95U  = function (V){(data[VvD_Group(),"X95U_M"]*(V/(sqrt(g*input$VvD_l/1000)))^data[VvD_Group(),"X95U_a"])*(input$VvD_l/1000)}
  X95L  = function (V){(data[VvD_Group(),"X95L_M"]*(V/(sqrt(g*input$VvD_l/1000)))^data[VvD_Group(),"X95L_a"])*(input$VvD_l/1000)}
  
  #Create reactive to run function only once per user input
  X5Num <- reactive({X95U(WVnum())})
  X25Num <- reactive({X75U(WVnum())})
  X50Num <- reactive({X50(WVnum())})
  X75Num <- reactive({X75L(WVnum())})
  X95Num <- reactive({X95L(WVnum())})
  
  #Creat functions to determine water velocity (m/s) based on swim distance (m)
  V50 = function (X){((X/(input$VvD_l/1000)/data[VvD_Group(),"M"])^(-1/abs(data[VvD_Group(),"a"]))/(1/sqrt(g*(input$VvD_l/1000))))}
  V75U = function (X){((X/(input$VvD_l/1000)/data[VvD_Group(),"X75U_M"])^(-1/abs(data[VvD_Group(),"X75U_a"]))/(1/sqrt(g*(input$VvD_l/1000))))}
  V75L = function (X){((X/(input$VvD_l/1000)/data[VvD_Group(),"X75L_M"])^(-1/abs(data[VvD_Group(),"X75L_a"]))/(1/sqrt(g*(input$VvD_l/1000))))}
  V95U = function (X){((X/(input$VvD_l/1000)/data[VvD_Group(),"X95U_M"])^(-1/abs(data[VvD_Group(),"X95U_a"]))/(1/sqrt(g*(input$VvD_l/1000))))}
  V95L = function (X){((X/(input$VvD_l/1000)/data[VvD_Group(),"X95L_M"])^(-1/abs(data[VvD_Group(),"X95L_a"]))/(1/sqrt(g*(input$VvD_l/1000))))}
  
  #Create reactive to run function only once per user input
  V5Num <- reactive({V95U(SDnum())})
  V25Num <- reactive({V75U(SDnum())})
  V50Num <- reactive({V50(SDnum())})
  V75Num <- reactive({V75L(SDnum())})
  V95Num <- reactive({V95L(SDnum())})
  
  Alpha2 <- reactive({
    if(input$Calculate2 == 1 | input$Calculate2 == 2){tempAlpha2 = 0.5}
    else tempAlpha2 = 1
    tempAlpha2
  })
  
  #Sloppy code that creates a dataframe containing the velocity vs distance data based on limits of swim time
  Plot2Data <- reactive({
    dataSet <- data.frame(Velocity)
    dataSet5 <- data.frame(Velocity)
    dataSet25 <- data.frame(Velocity)
    dataSet50 <- data.frame(Velocity)
    dataSet75 <- data.frame(Velocity)
    dataSet95 <- data.frame(Velocity)
    dataSet5$T5 <- VvD_T95U(VtoU95U(dataSet$Velocity))
    dataSet25$T25 <- VvD_T75U(VtoU75U(dataSet$Velocity))
    dataSet50$T50 <- VvD_T50(VtoU50(dataSet$Velocity))
    dataSet75$T75 <- VvD_T75L(VtoU75L(dataSet$Velocity))
    dataSet95$T95 <- VvD_T95L(VtoU95L(dataSet$Velocity))
    dataSet5 <- subset(dataSet5, T5 >=3 & T5 <=1800)
    dataSet25 <- subset(dataSet25, T25 >=3 & T25 <=1800)
    dataSet50 <- subset(dataSet50, T50 >=3 & T50 <=1800)
    dataSet75 <- subset(dataSet75, T75 >=3 & T75 <=1800)
    dataSet95 <- subset(dataSet95, T95 >=3 & T95 <=1800)
    dataSet5$X5 <- X95U(dataSet5$Velocity)
    dataSet25$X25 <- X75U(dataSet25$Velocity)
    dataSet50$X50 <- X50(dataSet50$Velocity)
    dataSet75$X75 <- X75L(dataSet75$Velocity)
    dataSet95$X95 <- X95L(dataSet95$Velocity)
    dataSet <- merge(dataSet, dataSet5, by="Velocity", all=TRUE)
    dataSet <- merge(dataSet, dataSet25, by="Velocity", all=TRUE)
    dataSet <- merge(dataSet, dataSet50, by="Velocity", all=TRUE)
    dataSet <- merge(dataSet, dataSet75, by="Velocity", all=TRUE)
    dataSet <- merge(dataSet, dataSet95, by="Velocity", all=TRUE)
    dataSet
  })
  
  #Create plot for distance vs velocity
  output$Plot1 <- renderPlot(
    {ggplot(data=Plot2Data(), aes(Velocity)) +
        
        #Customize appearance of plot
        theme_classic()+ 
        theme(axis.line.x = element_line(color="black", size = .5),
              axis.line.y = element_line(color="black", size = .5))+
        theme(legend.key = element_blank(), legend.position="top", legend.text=element_text(size=16))+
        theme(axis.text = element_text(size=14), axis.title = element_text(size=16))+
        
        #draw lines based on Plot2Data    
        geom_line(aes(y=X5,  colour = "95% prediction interval"), size = 0.8, alpha=Alpha2(),  na.rm = TRUE)+
        geom_line(aes(y=X25, colour = "75% prediction interval"), size = 0.8, alpha=Alpha2(),  na.rm = TRUE)+
        geom_line(aes(y=X50, colour = "Mean performance"), size = 0.8, alpha=Alpha2(),  na.rm = TRUE)+
        geom_line(aes(y=X75, colour = "75% prediction interval"), size = 0.8, alpha=Alpha2(),  na.rm = TRUE)+
        geom_line(aes(y=X95, colour = "95% prediction interval"), size = 0.8, alpha=Alpha2(),  na.rm = TRUE)+
        
        #change scale based on user input    
        {if(input$Logx2==TRUE) scale_x_log10(name = "Water velocity (m/s)", limits=c(0.1, 10), breaks = c(0.1,1,10))
          else scale_x_continuous(name = "Water velocity (m/s)", limits=c(0.1, 10))} +
        {if(input$Logx2==TRUE) annotation_logticks(base = 10, sides = "b")}+
        {if(input$Logy2==TRUE) scale_y_log10(name = "Swim distance (m)", limits=c(0.1, 1000), breaks = c(0.1,1,10,100,1000))
          else scale_y_continuous(name = "Swim distance (m)", limits=c(0.1, max(Plot2Data()$X5,na.rm=TRUE)))} +
        {if(input$Logy2==TRUE) annotation_logticks(base = 10, sides = "VvD_l")}+
        
        #add vertical dashed line to plot for swim distance calculations  
        {if(input$Calculate2 == '1') geom_vline(xintercept=WVnum(), linetype = "dashed", size = 0.8)}+
        
        #add horizontal estimate lines for swim distance calculations
        {if(input$WVEst == TRUE & input$Calculate2 == '1' & X5Num() >= min(Plot2Data()$X5,na.rm=TRUE) & X5Num() <= max(Plot2Data()$X5,na.rm=TRUE))
          geom_segment(aes(x = 0.1, y = X5Num(), xend = WVnum(), yend = X5Num()), linetype = "dashed", colour="orangered", size = 0.8)}+
        {if(input$WVEst == TRUE & input$Calculate2 == '1' & X5Num() >= max(Plot2Data()$X5,na.rm=TRUE))
          geom_segment(aes(x = 0.1, y = max(Plot2Data()$X5,na.rm=TRUE), xend = Plot2Data()$Velocity[which.max(Plot2Data()$X5)], yend = max(Plot2Data()$X5,na.rm=TRUE)), linetype = "dashed", colour="orangered", size = 0.8)}+    
        
        {if(input$WVEst == TRUE & input$Calculate2 == '1' & X25Num() >= min(Plot2Data()$X25,na.rm=TRUE) & X25Num() <= max(Plot2Data()$X25,na.rm=TRUE))
          geom_segment(aes(x = 0.1, y = X25Num(), xend = WVnum(), yend = X25Num()), linetype = "dashed", colour="skyblue3", size = 0.8)}+ 
        {if(input$WVEst == TRUE & input$Calculate2 == '1' & X25Num() >= max(Plot2Data()$X25,na.rm=TRUE))
          geom_segment(aes(x = 0.1, y = max(Plot2Data()$X25,na.rm=TRUE), xend = Plot2Data()$Velocity[which.max(Plot2Data()$X25)], yend = max(Plot2Data()$X25,na.rm=TRUE)), linetype = "dashed", colour="skyblue3", size = 0.8)}+    
        
        {if(input$WVEst == TRUE & input$Calculate2 == '1' & X50Num() >= min(Plot2Data()$X50,na.rm=TRUE) & X50Num() <= max(Plot2Data()$X50,na.rm=TRUE))
          geom_segment(aes(x = 0.1, y = X50Num(), xend = WVnum(), yend = X50Num()), linetype = "dashed", colour="grey65", size = 0.8)}+ 
        {if(input$WVEst == TRUE & input$Calculate2 == '1' & X50Num() >= max(Plot2Data()$X50,na.rm=TRUE))
          geom_segment(aes(x = 0.1, y = max(Plot2Data()$X50,na.rm=TRUE), xend = Plot2Data()$Velocity[which.max(Plot2Data()$X50)], yend = max(Plot2Data()$X50,na.rm=TRUE)), linetype = "dashed", colour="grey65", size = 0.8)}+    
        
        {if(input$WVEst == TRUE & input$Calculate2 == '1' & X75Num() >= min(Plot2Data()$X75,na.rm=TRUE) & X75Num() <= max(Plot2Data()$X75,na.rm=TRUE))
          geom_segment(aes(x = 0.1, y = X75Num(), xend = WVnum(), yend = X75Num()), linetype = "dashed", colour="skyblue3", size = 0.8)}+
        {if(input$WVEst == TRUE & input$Calculate2 == '1' & X75Num() >= max(Plot2Data()$X75,na.rm=TRUE))
          geom_segment(aes(x = 0.1, y = max(Plot2Data()$X75,na.rm=TRUE), xend = Plot2Data()$Velocity[which.max(Plot2Data()$X75)], yend = max(Plot2Data()$X75,na.rm=TRUE)), linetype = "dashed", colour="skyblue3", size = 0.8)}+
        
        {if(input$WVEst == TRUE & input$Calculate2 == '1' & X95Num() >= min(Plot2Data()$X95,na.rm=TRUE) & X95Num() <= max(Plot2Data()$X95,na.rm=TRUE))
          geom_segment(aes(x = 0.1, y = X95Num(), xend = WVnum(), yend = X95Num()), linetype = "dashed", colour="orangered", size = 0.8)}+ 
        {if(input$WVEst == TRUE & input$Calculate2 == '1' & X95Num() >= max(Plot2Data()$X95,na.rm=TRUE))
          geom_segment(aes(x = 0.1, y = max(Plot2Data()$X95,na.rm=TRUE), xend = Plot2Data()$Velocity[which.max(Plot2Data()$X95)], yend = max(Plot2Data()$X95,na.rm=TRUE)), linetype = "dashed", colour="orangered", size = 0.8)}+    
        
        #add horizontal dashed line to plot for velocity calculations        
        {if(input$Calculate2 == '2') geom_hline(yintercept=SDnum(), linetype = "dashed", size = 0.8)}+
        
        #add vertical estimate lines for velocity calculations  
        {if(input$SDEst == TRUE & input$Calculate2 == '2' & SDnum()  >= min(Plot2Data()$X5,na.rm=TRUE) & SDnum()  <= max(Plot2Data()$X5,na.rm=TRUE))
          geom_segment(aes(x = V5Num(), y = 0.1, xend = V5Num(), yend = SDnum()), linetype = "dashed", colour="orangered", size = 0.8)}+ 
        {if(input$SDEst == TRUE & input$Calculate2 == '2' & SDnum()  <= min(Plot2Data()$X5,na.rm=TRUE))
          geom_segment(aes(x = Plot2Data()$Velocity[which.min(Plot2Data()$X5)], y = 0.1, xend = Plot2Data()$Velocity[which.min(Plot2Data()$X5)], yend = min(Plot2Data()$X5,na.rm=TRUE)),linetype = "dashed", colour="orangered", size = 0.8)}+
        
        {if(input$SDEst == TRUE & input$Calculate2 == '2' & SDnum()  >= min(Plot2Data()$X25,na.rm=TRUE) & SDnum()  <= max(Plot2Data()$X25,na.rm=TRUE))
          geom_segment(aes(x = V25Num(), y = 0.1, xend = V25Num(), yend = SDnum()), linetype = "dashed", colour="skyblue3", size = 0.8)}+
        {if(input$SDEst == TRUE & input$Calculate2 == '2' & SDnum()  <= min(Plot2Data()$X25,na.rm=TRUE))
          geom_segment(aes(x = Plot2Data()$Velocity[which.min(Plot2Data()$X25)], y = 0.1, xend = Plot2Data()$Velocity[which.min(Plot2Data()$X25)], yend = min(Plot2Data()$X25,na.rm=TRUE)),linetype = "dashed", colour="skyblue3", size = 0.8)}+
        
        {if(input$SDEst == TRUE & input$Calculate2 == '2' & SDnum()  >= min(Plot2Data()$X50,na.rm=TRUE) & SDnum()  <= max(Plot2Data()$X50,na.rm=TRUE))
          geom_segment(aes(x = V50Num(), y = 0.1, xend = V50Num(), yend = SDnum()), linetype = "dashed", colour="grey65", size = 0.8)}+
        {if(input$SDEst == TRUE & input$Calculate2 == '2' & SDnum()  <= min(Plot2Data()$X50,na.rm=TRUE))
          geom_segment(aes(x = Plot2Data()$Velocity[which.min(Plot2Data()$X50)], y = 0.1, xend = Plot2Data()$Velocity[which.min(Plot2Data()$X50)], yend = min(Plot2Data()$X50,na.rm=TRUE)),linetype = "dashed", colour="grey65", size = 0.8)}+
        
        {if(input$SDEst == TRUE & input$Calculate2 == '2' & SDnum()  >= min(Plot2Data()$X75,na.rm=TRUE) & SDnum()  <= max(Plot2Data()$X75,na.rm=TRUE))
          geom_segment(aes(x = V75Num(), y = 0.1, xend = V75Num(), yend = SDnum()), linetype = "dashed", colour="skyblue3", size = 0.8)}+
        {if(input$SDEst == TRUE & input$Calculate2 == '2' & SDnum()  <= min(Plot2Data()$X75,na.rm=TRUE))
          geom_segment(aes(x = Plot2Data()$Velocity[which.min(Plot2Data()$X75)], y = 0.1, xend = Plot2Data()$Velocity[which.min(Plot2Data()$X75)], yend = min(Plot2Data()$X75,na.rm=TRUE)),linetype = "dashed", colour="skyblue3", size = 0.8)}+
        
        {if(input$SDEst == TRUE & input$Calculate2 == '2' & SDnum()  >= min(Plot2Data()$X95,na.rm=TRUE) & SDnum()  <= max(Plot2Data()$X95,na.rm=TRUE))
          geom_segment(aes(x = V95Num(), y = 0.1, xend = V95Num(), yend = SDnum()), linetype = "dashed", colour="orangered", size = 0.8)}+
        {if(input$SDEst == TRUE & input$Calculate2 == '2' & SDnum()  <= min(Plot2Data()$X95,na.rm=TRUE))
          geom_segment(aes(x = Plot2Data()$Velocity[which.min(Plot2Data()$X95)], y = 0.1, xend = Plot2Data()$Velocity[which.min(Plot2Data()$X95)], yend = min(Plot2Data()$X95,na.rm=TRUE)),linetype = "dashed", colour="orangered", size = 0.8)}+
        
        #add legend      
        scale_colour_manual("",values = c("skyblue2","orangered", "grey65"), 
                            breaks=c("Mean performance","75% prediction interval","95% prediction interval"))
    },
    height = function() {
      session$clientData$output_Plot1_width*0.4
    })
  
  
  #Create plot title text  
  output$GroupTitle2 <- renderText({
    paste(VvD_Group(), "Group")
  })
  
  EstimatedSwimSpeed <- reactive({
    ESS <- data.frame(Estimates, "Swim Speed" = c(signif(U5Num(), digits = 2), 
                                                  signif(U25Num(), digits = 2), 
                                                  signif(U50Num(), digits = 2), 
                                                  signif(U75Num(), digits = 2),
                                                  signif(U95Num(), digits = 2)))
    ESS
  })
  
  
  #Create output text for swim distance based on water velocity
  output$DistanceText5 <- renderText({
    if(X5Num() < min(Plot2Data()$X5,na.rm=TRUE)){
      DT5 <- paste("2.5% of ", input$VvD_l, "mm", Group(), "cannot swim in", WVnum(), "m/s current")}
    if(X5Num() >= min(Plot2Data()$X5,na.rm=TRUE) & X5Num() < max(Plot2Data()$X5,na.rm=TRUE)){
      DT5 <- paste("2.5% of ", input$VvD_l, "mm", Group(), "can swim at least", signif(X5Num(), digits = 2), "m when swimming in ", WVnum(), "m/s current")}
    if(X5Num() >= max(Plot2Data()$X5,na.rm=TRUE)){
      DT5 <- paste("2.5% of", input$VvD_l, "mm", Group(), "can swim at least", signif(max(Plot2Data()$X5,na.rm=TRUE), digits =2), "m when swimming in ", WVnum(), "m/s current")}
    DT5
  })
  
  output$DistanceText25 <- renderText({
    if(X25Num() < min(Plot2Data()$X25,na.rm=TRUE)){
      DT25 <- paste("12.5% of ", input$VvD_l, "mm", Group(), "cannot swim in", WVnum(), "m/s current")}
    if(X25Num() >= min(Plot2Data()$X25,na.rm=TRUE) & X25Num() < max(Plot2Data()$X25,na.rm=TRUE)){
      DT25 <- paste("12.5% of ", input$VvD_l, "mm", Group(), "can swim at least", signif(X25Num(), digits = 2), "m when swimming in ", WVnum(), "m/s current")}
    if(X25Num() >= max(Plot2Data()$X25,na.rm=TRUE)){
      DT25 <- paste("12.5% of", input$VvD_l, "mm", Group(), "can swim at least", signif(max(Plot2Data()$X25,na.rm=TRUE), digits =2), "m when swimming in ", WVnum(), "m/s current")}
    DT25
  })
  
  output$DistanceText50 <- renderText({
    if(X50Num() < min(Plot2Data()$X50,na.rm=TRUE)){
      DT50 <- paste("50% of ", input$VvD_l, "mm", Group(), "cannot swim in", WVnum(), "m/s current")}
    if(X50Num() >= min(Plot2Data()$X50,na.rm=TRUE) & X50Num() < max(Plot2Data()$X50,na.rm=TRUE)){
      DT50 <- paste("50% of ", input$VvD_l, "mm", Group(), "can swim at least", signif(X50Num(), digits = 2), "m when swimming in ", WVnum(), "m/s current")}
    if(X50Num() >= max(Plot2Data()$X50,na.rm=TRUE)){
      DT50 <- paste("50% of", input$VvD_l, "mm", Group(), "can swim at least", signif(max(Plot2Data()$X50,na.rm=TRUE), digits =2), "m when swimming in ", WVnum(), "m/s current")}
    DT50
  })
  
  output$DistanceText75 <- renderText({
    if(X75Num() < min(Plot2Data()$X75,na.rm=TRUE)){
      DT75 <- paste("87.5% of ", input$VvD_l, "mm", Group(), "cannot swim in", WVnum(), "m/s current")}
    if(X75Num() >= min(Plot2Data()$X75,na.rm=TRUE) & X75Num() < max(Plot2Data()$X75,na.rm=TRUE)){
      DT75 <- paste("87.5% of ", input$VvD_l, "mm", Group(), "can swim at least", signif(X75Num(), digits = 2), "m when swimming in ", WVnum(), "m/s current")}
    if(X75Num() >= max(Plot2Data()$X75,na.rm=TRUE)){
      DT75 <- paste("87.5% of", input$VvD_l, "mm", Group(), "can swim at least", signif(max(Plot2Data()$X75,na.rm=TRUE), digits =2), "m when swimming in ", WVnum(), "m/s current")}
    DT75
  })
  
  output$DistanceText95 <- renderText({
    if(X95Num() < min(Plot2Data()$X95,na.rm=TRUE)){
      DT95 <- paste("97.5% of ", input$VvD_l, "mm", Group(), "cannot swim in", WVnum(), "m/s current")}
    if(X95Num() >= min(Plot2Data()$X95,na.rm=TRUE) & X95Num() < max(Plot2Data()$X95,na.rm=TRUE)){
      DT95 <- paste("97.5% of ", input$VvD_l, "mm", Group(), "can swim at least", signif(X95Num(), digits = 2), "m when swimming in ", WVnum(), "m/s current")}
    if(X95Num() >= max(Plot2Data()$X95,na.rm=TRUE)){
      DT95 <- paste("97.5% of", input$VvD_l, "mm", Group(), "can swim at least", signif(max(Plot2Data()$X95,na.rm=TRUE), digits =2), "m when swimming in ", WVnum(), "m/s current")}
    DT95
  })
  
  
  #Create output text for water velocity based on swim distance
  output$VelocityText5 <- renderText({
    if(V5Num()  < Plot2Data()$Velocity[which.max(Plot2Data()$X5)]){
      VT5 <- paste("2.5% of ", input$VvD_l, "mm", Group(), "cannot swim ", SDnum(), "m in any current")}
    if((V5Num() <= Plot2Data()$Velocity[which.min(Plot2Data()$X5)]) & V5Num()  >= Plot2Data()$Velocity[which.max(Plot2Data()$X5)]){
      VT5 <- paste("2.5% of ", input$VvD_l, "mm", Group(), "can swim in ", signif(V5Num(), digits = 2), "m/s current for at least ", SDnum(), " m")}
    if(V5Num()  > Plot2Data()$Velocity[which.min(Plot2Data()$X5)]){
      VT5 <- paste("2.5% of ", input$VvD_l, "mm", Group(), "can swim in ", signif(Plot2Data()$Velocity[which.min(Plot2Data()$X5)], digits = 2),  "m/s current for at least ", SDnum(), " m")}
    VT5
  })
  
  output$VelocityText25 <- renderText({
    if(V25Num()  < Plot2Data()$Velocity[which.max(Plot2Data()$X25)]){
      VT25 <- paste("12.5% of ", input$VvD_l, "mm", Group(), "cannot swim ", SDnum(), "m in any current")}
    if((V25Num() <= Plot2Data()$Velocity[which.min(Plot2Data()$X25)]) & V25Num()  >= Plot2Data()$Velocity[which.max(Plot2Data()$X25)]){
      VT25 <- paste("12.5% of ", input$VvD_l, "mm", Group(), "can swim in ", signif(V25Num(), digits = 2), "m/s current for at least ", SDnum(), " m")}
    if(V25Num()  > Plot2Data()$Velocity[which.min(Plot2Data()$X25)]){
      VT25 <- paste("12.5% of ", input$VvD_l, "mm", Group(), "can swim in ", signif(Plot2Data()$Velocity[which.min(Plot2Data()$X25)], digits = 2),  "m/s current for at least ", SDnum(), " m")}
    VT25
  })
  
  output$VelocityText50 <- renderText({
    if(V50Num()  < Plot2Data()$Velocity[which.max(Plot2Data()$X50)]){
      VT50 <- paste("50% of ", input$VvD_l, "mm", Group(), "cannot swim ", SDnum(), "m in any current")}
    if((V50Num() <= Plot2Data()$Velocity[which.min(Plot2Data()$X50)]) & V50Num()  >= Plot2Data()$Velocity[which.max(Plot2Data()$X50)]){
      VT50 <- paste("50% of ", input$VvD_l, "mm", Group(), "can swim in ", signif(V50Num(), digits = 2), "m/s current for at least ", SDnum(), " m")}
    if(V50Num()  > Plot2Data()$Velocity[which.min(Plot2Data()$X50)]){
      VT50 <- paste("50% of ", input$VvD_l, "mm", Group(), "can swim in ", signif(Plot2Data()$Velocity[which.min(Plot2Data()$X50)], digits = 2),  "m/s current for at least ", SDnum(), " m")}
    VT50
  })
  
  output$VelocityText75 <- renderText({
    if(V75Num()  < Plot2Data()$Velocity[which.max(Plot2Data()$X75)]){
      VT75 <- paste("87.5% of ", input$VvD_l, "mm", Group(), "cannot swim ", SDnum(), "m in any current")}
    if((V75Num() <= Plot2Data()$Velocity[which.min(Plot2Data()$X75)]) & V75Num()  >= Plot2Data()$Velocity[which.max(Plot2Data()$X75)]){
      VT75 <- paste("87.5% of ", input$VvD_l, "mm", Group(), "can swim in ", signif(V75Num(), digits = 2), "m/s current for at least ", SDnum(), " m")}
    if(V75Num()  > Plot2Data()$Velocity[which.min(Plot2Data()$X75)]){
      VT75 <- paste("87.5% of ", input$VvD_l, "mm", Group(), "can swim in ", signif(Plot2Data()$Velocity[which.min(Plot2Data()$X75)], digits = 2),  "m/s current for at least ", SDnum(), " m")}
    VT75
  })
  
  output$VelocityText95 <- renderText({
    if(V95Num()  < Plot2Data()$Velocity[which.max(Plot2Data()$X95)]){
      VT95 <- paste("97.5% of ", input$VvD_l, "mm", Group(), "cannot swim ", SDnum(), "m in any current")}
    if((V95Num() <= Plot2Data()$Velocity[which.min(Plot2Data()$X95)]) & V95Num()  >= Plot2Data()$Velocity[which.max(Plot2Data()$X95)]){
      VT95 <- paste("97.5% of ", input$VvD_l, "mm", Group(), "can swim in ", signif(V95Num(), digits = 2), "m/s current for at least ", SDnum(), " m")}
    if(V95Num()  > Plot2Data()$Velocity[which.min(Plot2Data()$X95)]){
      VT95 <- paste("97.5% of ", input$VvD_l, "mm", Group(), "can swim in ", signif(Plot2Data()$Velocity[which.min(Plot2Data()$X95)], digits = 2),  "m/s current for at least ", SDnum(), " m")}
    VT95
  })
  
  
}

shinyApp( ui = ui, server = server)


#runApp("app.r")