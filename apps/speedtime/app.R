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
                            a(href="http://www.fishprotectiontools.ca/speedtime-manual.html",target="_blank", "Manual"), align = "center"
                          ),
                          
                          radioButtons("Selecter", label = "Select fish by:",
                                       choices = list("Group" = 0, "Common name" = 1, "Scientific name" = 2), selected=0),
                          
                          conditionalPanel("input.Selecter == '0'",selectInput("Group", 
                                                                               label = "Select group",
                                                                               choices = list("Catfish & Sunfish", "Eel", "Herring", "Salmon & Walleye", "Sturgeon", "Pike (derived)"),
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
                          
                          radioButtons("Calculate", label = "Calculations:",
                                       choices = list("None" = 0, "Swim speed" = 1, "Swim time" = 2)),
                          
                          conditionalPanel("input.Calculate == '1'",
                                           textInput("ST", 
                                                     label = "Swim time in seconds (3-1800):",
                                                     value = "600")),
                          
                          conditionalPanel("input.Calculate == '1'",
                                           checkboxInput("STEst", label = "Plot estimates", value = TRUE)),
                          
                          conditionalPanel("input.Calculate == '2'",
                                           textInput("SS", 
                                                     label = "Swim speed in m/s",
                                                     value = "0.3")),
                          
                          conditionalPanel("input.Calculate == '2'",
                                           checkboxInput("SSEst", label = "Plot estimates", value = TRUE))
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
                                           textOutput("SpeedText95"),br(),br()
                          ),
                          conditionalPanel("input.Calculate == '2'", 
                                           h2("Estimates"),
                                           textOutput("TimeText5"),
                                           textOutput("TimeText25"),
                                           textOutput("TimeText50"),
                                           textOutput("TimeText75"),
                                           textOutput("TimeText95"),br(),br()
                          ),
                          align = "center"
                        )    #close mainPanel
                      )     #close sidebarLayout
))}       #close fluidpage



#######################
##       Server      ##
#######################


server <- function(input, output, session){ 
  
  #Set gravitational acceleration constant
  g=9.81

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
        theme(legend.key = element_blank(), legend.position="top", legend.text=element_text(size=14, family="lato"))+
        theme(axis.text = element_text(size=14, family="lato"), axis.title = element_text(size=16, family="lato"))+
        scale_x_log10(name = "Swim time (s)", limits=c(3, 1800))+
        scale_y_log10(name = "Swim speed (m/s)", limits=c(.02, 14))+
        annotation_logticks(base = 10, sides = "b")+
        annotation_logticks(base = 10, sides = "l")+
        
        #add function to plot
        stat_function(fun = U, aes(colour = "Mean  "), size = 0.8, alpha=Alpha()) +
        stat_function(fun= U75U, aes(colour="75% prediction interval  "), size = 0.8, alpha=Alpha()) +
        stat_function(fun= U75L, aes(colour="75% prediction interval  "), size = 0.8, alpha=Alpha()) +
        stat_function(fun= U95U, aes(colour="95% prediction interval  "), size = 0.8, alpha=Alpha()) +
        stat_function(fun= U95L, aes(colour="95% prediction interval  "), size = 0.8, alpha=Alpha()) +
        
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
                            breaks=c("Mean  ","75% prediction interval  ","95% prediction interval  "))
    },
    height = function() {
      session$clientData$output_Plot0_width*0.5
    }
    
  )
  
  #Create plot title text
  output$GroupTitle <- renderText({
    paste(Group(), "Group")
  })
  
  #Create output text for swim speed based on swim time
  output$SpeedText5 <- renderText({
    paste("2.5% of ", input$l, "mm", Group(), "can achieve ", signif(U5Num(), digits = 2), "m/s when swimming for ", STnum(), "s" )
  })
  output$SpeedText25 <- renderText({
    paste("12.5% of ", input$l, "mm", Group(), "can achieve ", signif(U25Num(), digits = 2), "m/s when swimming for ", STnum(), "s" )
  })
  output$SpeedText50 <- renderText({
    paste("50% of ", input$l, "mm", Group(), "can achieve ", signif(U50Num(), digits = 2), "m/s when swimming for ", STnum(), "s" )
  })
  output$SpeedText75 <- renderText({
    paste("87.5% of ", input$l, "mm", Group(), "can achieve ", signif(U75Num(), digits = 2), "m/s when swimming for ", STnum(), "s" )
  })
  output$SpeedText95 <- renderText({
    paste("97.5% of ", input$l, "mm", Group(), "can achieve ", signif(U95Num(), digits = 2), "m/s when swimming for ", STnum(), "s" )
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
}


enableBookmarking(store = "url")

shinyApp( ui = ui, server = server)


#runApp("app.r")