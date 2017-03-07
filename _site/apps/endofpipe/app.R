library(shiny)
library(shinythemes)
library(ggplot2)
library(scales)

#change plot fonts from defaults
library(showtext)
font.add.google("Lato","lato")
showtext.auto()

options(scipen=5)

#import K and b values for each group of fish (used in equations)
data <- read.csv("GroupVariables.csv")
rownames(data)<-data$Group
data$Group <- NULL

#import list of common and scientific names and their respective lengths
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
  
  #Add Google anaylytics script to track application usage
  tags$head(includeScript("google-analytics.js")),
  
  theme = shinytheme("cosmo"),
  
  #Logo title
  # titlePanel(title=div(img(src='DFOwordmark.png', align = "right", width = 400), ""), windowTitle = "Fish Swimming Performance Tools"),
  # br(),
  # br(),
  # br(),
  
  #add favicon
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
                      sidebarLayout(
                        sidebarPanel(
                          helpText(
                            "Need help? Visit the ",
                            a(href="http://www.fishprotectiontools.ca/endofpipe-manual.html",target="_blank", "Manual"), align = "center"
                          ),
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
                          
                          numericInput("EoP_flowrate", 
                                      label = "Intake flow rate (L/s):",
                                      min = 0, max = 1000, value = 0, step = 0.5)
                          
                        ),    #close sidebarPanel
                        mainPanel(
                          #h1("End-of-Pipe Screen Size"),
                          plotOutput("EoP_Plot", height = "auto"),
                          br(),
                          htmlOutput("EoP_Text"),
                          align = "center"
                        )    #close mainpanel
                      )      #close sidebarLayout
))}       #close fluidpage



#######################
##       Server      ##
#######################


server <- function(input, output, session){ 
  
  #Set gravitational acceleration constant
  g=9.81

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
      scale <- 1500}
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
    EoP_dataSet$Flow_l_per_s <- signif(EoP_dataSet$Flow * 1000, digits = 5)
    
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
  
  output$EoP_Text <- renderUI({
    if(input$EoP_flowrate >0 & input$EoP_flowrate <= 1000 & input$EoP_flowrate %% 0.5 == 0 & is.na(as.numeric(input$EoP_flowrate))==FALSE){HTML("97.5% of", input$EoP_l, "mm fish in ", input$EoP_flowrate, "L/s flow with", input$EoP_time, "minutes to escape the screen need an Open Screen Area of", round(EoP_Worst(),3), "m<sup>2</sup>")}})
  
  
  
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
        scale_y_continuous(name = expression('Open Screen Area (m'^2*')'), expand = c(0,0), limits=c(0,EoP_Plot_Y_Max()), breaks = scales::pretty_breaks(n = 6))+
        
        #draw Group lines if fish are selected by Groups
        {if(input$EoP_Selecter==0 & input$CatfishSunfish==TRUE) geom_line(aes(y= Catfish_Sunfish_Area, colour = "Catfish & Sunfish  "), size = 1)}+
        {if(input$EoP_Selecter==0 & input$Eel==TRUE)            geom_line(aes(y= Eel_Area, colour = "Eel  "), size = 1)}+
        {if(input$EoP_Selecter==0 & input$Herring==TRUE)        geom_line(aes(y= Herring_Area, colour = "Herring  "), size = 1)}+
        {if(input$EoP_Selecter==0 & input$Pike==TRUE)           geom_line(aes(y= Pike_Area, colour = "Pike  "), size = 1)}+
        {if(input$EoP_Selecter==0 & input$SalmonWalleye==TRUE)  geom_line(aes(y= Salmon_Walleye_Area, colour = "Salmon & Walleye  "), size = 1)}+
        {if(input$EoP_Selecter==0 & input$Sturgeon==TRUE)       geom_line(aes(y= Sturgeon_Area, colour = "Sturgeon  "), size = 1)}+
        
        #draw group lines if fish are selected by Species
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
        
        #draw estimate lines for slowest group
        {if(input$EoP_flowrate>0) 
          geom_segment(aes(x = input$EoP_flowrate, y = 0, xend = input$EoP_flowrate, yend = EoP_Worst()), linetype = "dashed", size = 0.8)}+
        {if(input$EoP_flowrate>0) 
          geom_segment(aes(x = 0, y = EoP_Worst(), xend = input$EoP_flowrate, yend = EoP_Worst()), linetype = "dashed", size = 0.8)}+
        
        #Add legend
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


#runApp("app.r")