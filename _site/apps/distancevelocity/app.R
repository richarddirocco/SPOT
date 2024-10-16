library(shiny)
library(shinythemes)
library(ggplot2)
library(scales)
#library(Cairo)
#options(shiny.usecairo=T)


# Change plot fonts from defaults
library(showtext)
font_add_google("Lato","lato")
showtext_auto()

options(scipen=5)

# Import K and b values for each group of fish (used in equations)
data <- read.csv("GroupVariables.csv")
rownames(data)<-data$Group
data$Group <- NULL

# Import list of common and scientific names and their respective lengths
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
               a(href="https://fishprotectiontools.ca/userguide.html",target="_blank", "Manual"), align = "center"
      ),
                          
      radioButtons("VvD_Selecter", label = "Select fish by:", choices = list("Group" = 0, "Common name" = 1, "Scientific name" = 2), selected=0),
                          
      conditionalPanel("input.VvD_Selecter == '0'", 
          selectInput("VvD_Group", label = "Select group", 
                      choices = list("Catfish & Sunfish", "Eel", "Herring", "Salmon & Walleye", "Sturgeon", "Pike (derived)"), 
                      selected = "Salmon & Walleye")
          ),
                          
      conditionalPanel("input.VvD_Selecter == '1'", 
          selectInput("VvD_CName", label = "Select species", choices = sort(FishList$CommonName), selected = "Brook trout")),
                          
      conditionalPanel("input.VvD_Selecter == '2'", 
          selectInput("VvD_SName", label = "Select species", choices = sort(FishList$ScientificName), selected = "Salvelinus fontinalis")),
      
      sliderInput("VvD_l", label = "Fish length (mm):", min = 25, max = 1000, value = c(2.5), step = 5, ticks = FALSE),
                          
      radioButtons("Calculate2", label = "Calculations:", choices = list("None" = 0, "Swim distance" = 1, "Water velocity" = 2)),
                          
      conditionalPanel("input.Calculate2 == '1'", 
          textInput("WV", label = "Water velocity in m/s:", value = "0.15")),
                          
      conditionalPanel("input.Calculate2 == '1'",
          checkboxInput("WVEst", label = "Plot estimates", value = TRUE)),
                          
      conditionalPanel("input.Calculate2 == '2'",
           textInput("SD", label = "Swim distance in meters:", value = "10")),
                          
       conditionalPanel("input.Calculate2 == '2'",
            checkboxInput("SDEst", label = "Plot estimates", value = TRUE)),
      
      helpText(a(href="mailto:richard.dirocco@dfo-mpo.gc.ca", "Submit feedback"), align = "center")
      
    ), #close sidebar panel
                        
    mainPanel(
      h2(textOutput("GroupTitle2")),        
      plotOutput("Plot1", height = "auto"),
      br(),                
      conditionalPanel("input.Calculate2 == '1'", 
                       h2("Estimates"),
                       textOutput("DistanceText5"),
                       textOutput("DistanceText25"),
                       textOutput("DistanceText50"),
                       textOutput("DistanceText75"),
                       textOutput("DistanceText95"),br(),br()),
                          
      conditionalPanel("input.Calculate2 == '2'",
                       h2("Estimates"),
                       textOutput("VelocityText5"),
                       textOutput("VelocityText25"),
                       textOutput("VelocityText50"),
                       textOutput("VelocityText75"),
                       textOutput("VelocityText95"),br(),br()),
      align = "center"
    ) #close mainpanel
  )   #close sidebarLayout
  ))  #close fluidpage
}     #close UI  



#######################
##       Server      ##
#######################


server <- function(input, output, session){ 
  
  # Set gravitational acceleration constant
  g=9.81

  # Create functions to determine swimming speed (m/s) based on time (seconds)
  U    = function (t){data[Group(),"k"]*sqrt(g*input$l/1000)*((sqrt(input$l/1000/g))^-data[Group(),"b"])*t^data[Group(),"b"]}
  U75U = function (t){data[Group(),"X75U_k"]*sqrt(g*input$l/1000)*((sqrt(input$l/1000/g))^-data[Group(),"X75U_b"])*t^data[Group(),"X75U_b"]}
  U75L = function (t){data[Group(),"X75L_k"]*sqrt(g*input$l/1000)*((sqrt(input$l/1000/g))^-data[Group(),"X75L_b"])*t^data[Group(),"X75L_b"]}
  U95U = function (t){data[Group(),"X95U_k"]*sqrt(g*input$l/1000)*((sqrt(input$l/1000/g))^-data[Group(),"X95U_b"])*t^data[Group(),"X95U_b"]}
  U95L = function (t){data[Group(),"X95L_k"]*sqrt(g*input$l/1000)*((sqrt(input$l/1000/g))^-data[Group(),"X95L_b"])*t^data[Group(),"X95L_b"]}
  
  # Create reactive to run function only once per user input
  U5Num <- reactive({U95U(STnum())})
  U25Num <- reactive({U75U(STnum())})
  U50Num <- reactive({U(STnum())})
  U75Num <- reactive({U75L(STnum())})
  U95Num <- reactive({U95L(STnum())})
  
  # Create functions to determine time (seconds) based on swimming speed (m/s)
  T50  = function (U){(((U/sqrt(g*input$l/1000))/data[Group(),"k"])^(-1/(abs(data[Group(),"b"]))))/(1/(sqrt(input$l/1000/g)))}
  T75U = function (U){(((U/sqrt(g*input$l/1000))/data[Group(),"X75U_k"])^(-1/(abs(data[Group(),"X75U_b"]))))/(1/(sqrt(input$l/1000/g)))}
  T75L = function (U){(((U/sqrt(g*input$l/1000))/data[Group(),"X75L_k"])^(-1/(abs(data[Group(),"X75L_b"]))))/(1/(sqrt(input$l/1000/g)))}
  T95U = function (U){(((U/sqrt(g*input$l/1000))/data[Group(),"X95U_k"])^(-1/(abs(data[Group(),"X95U_b"]))))/(1/(sqrt(input$l/1000/g)))}
  T95L = function (U){(((U/sqrt(g*input$l/1000))/data[Group(),"X95L_k"])^(-1/(abs(data[Group(),"X95L_b"]))))/(1/(sqrt(input$l/1000/g)))}
  
  # Create reactive to run function only once per user input
  T5Num <- reactive({T95U(SSnum())})
  T25Num <- reactive({T75U(SSnum())})
  T50Num <- reactive({T50(SSnum())})
  T75Num <- reactive({T75L(SSnum())})
  T95Num <- reactive({T95L(SSnum())})
  
  #######################################
  ##  Water Velocity vs Swim Distance  ##
  #######################################
  
  # Update group based on species selected
  VvD_Group <- reactive({
    if(input$VvD_Selecter==0){
      tempVvD_Group <- input$VvD_Group}
    if(input$VvD_Selecter==1){
      tempVvD_Group <- (FishList[which(FishList$CommonName==input$VvD_CName),"GroupName"])}
    if(input$VvD_Selecter==2){(
      tempVvD_Group <-FishList[which(FishList$ScientificName==input$VvD_SName),"GroupName"])}
    tempVvD_Group
  })
  
  # Update length slider scale based on group selected
  observe({
    if(input$VvD_Selecter==0){
      scale <- data[input$VvD_Group,"Max_l"]}
    if(input$VvD_Selecter==1){
      scale <- FishList[which(FishList$CommonName==input$VvD_CName),"MaxSize"]}
    if(input$VvD_Selecter==2){
      scale <- FishList[which(FishList$ScientificName==input$VvD_SName),"MaxSize"]}
    updateSliderInput(session, "VvD_l", min = 25, max = scale)
  })
  
  # Convert text input to number for calculations
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
  
  
  # Create functions to determine time (seconds) based on swimming speed (m/s)
  VvD_T50  = function (U){(((U/sqrt(g*input$VvD_l/1000))/data[VvD_Group(),"k"])^(-1/(abs(data[VvD_Group(),"b"]))))/(1/(sqrt(input$VvD_l/1000/g)))}
  VvD_T75U = function (U){(((U/sqrt(g*input$VvD_l/1000))/data[VvD_Group(),"X75U_k"])^(-1/(abs(data[VvD_Group(),"X75U_b"]))))/(1/(sqrt(input$VvD_l/1000/g)))}
  VvD_T75L = function (U){(((U/sqrt(g*input$VvD_l/1000))/data[VvD_Group(),"X75L_k"])^(-1/(abs(data[VvD_Group(),"X75L_b"]))))/(1/(sqrt(input$VvD_l/1000/g)))}
  VvD_T95U = function (U){(((U/sqrt(g*input$VvD_l/1000))/data[VvD_Group(),"X95U_k"])^(-1/(abs(data[VvD_Group(),"X95U_b"]))))/(1/(sqrt(input$VvD_l/1000/g)))}
  VvD_T95L = function (U){(((U/sqrt(g*input$VvD_l/1000))/data[VvD_Group(),"X95L_k"])^(-1/(abs(data[VvD_Group(),"X95L_b"]))))/(1/(sqrt(input$VvD_l/1000/g)))}
  
  # Create reactive to run function only once per user input
  VvD_T5Num <- reactive({VvD_T95U(SSnum())})
  VvD_T25Num <- reactive({VvD_T75U(SSnum())})
  VvD_T50Num <- reactive({VvD_T50(SSnum())})
  VvD_T75Num <- reactive({VvD_T75L(SSnum())})
  VvD_T95Num <- reactive({VvD_T95L(SSnum())})
  
  # Calculate U (fish swim speed in m/s) from V (water velocity)
  VtoU50 = function(V){V/(1+data[VvD_Group(),"b"])}
  VtoU75U = function(V){V/(1+data[VvD_Group(),"X75U_b"])}
  VtoU75L = function(V){V/(1+data[VvD_Group(),"X75L_b"])}
  VtoU95U = function(V){V/(1+data[VvD_Group(),"X95U_b"])}
  VtoU95L = function(V){V/(1+data[VvD_Group(),"X75L_b"])}
  
  # Create functions to determine swim distance (m) based on water velocity (m/s)
  X50  = function (V){(data[VvD_Group(),"M"]*(V/(sqrt(g*input$VvD_l/1000)))^data[VvD_Group(),"a"])*(input$VvD_l/1000)}
  X75U  = function (V){(data[VvD_Group(),"X75U_M"]*(V/(sqrt(g*input$VvD_l/1000)))^data[VvD_Group(),"X75U_a"])*(input$VvD_l/1000)}
  X75L  = function (V){(data[VvD_Group(),"X75L_M"]*(V/(sqrt(g*input$VvD_l/1000)))^data[VvD_Group(),"X75L_a"])*(input$VvD_l/1000)} 
  X95U  = function (V){(data[VvD_Group(),"X95U_M"]*(V/(sqrt(g*input$VvD_l/1000)))^data[VvD_Group(),"X95U_a"])*(input$VvD_l/1000)}
  X95L  = function (V){(data[VvD_Group(),"X95L_M"]*(V/(sqrt(g*input$VvD_l/1000)))^data[VvD_Group(),"X95L_a"])*(input$VvD_l/1000)}
  
  # Create reactive to run function only once per user input
  X5Num <- reactive({X95U(WVnum())})
  X25Num <- reactive({X75U(WVnum())})
  X50Num <- reactive({X50(WVnum())})
  X75Num <- reactive({X75L(WVnum())})
  X95Num <- reactive({X95L(WVnum())})
  
  # Create functions to determine water velocity (m/s) based on swim distance (m)
  V50 = function (X){((X/(input$VvD_l/1000)/data[VvD_Group(),"M"])^(-1/abs(data[VvD_Group(),"a"]))/(1/sqrt(g*(input$VvD_l/1000))))}
  V75U = function (X){((X/(input$VvD_l/1000)/data[VvD_Group(),"X75U_M"])^(-1/abs(data[VvD_Group(),"X75U_a"]))/(1/sqrt(g*(input$VvD_l/1000))))}
  V75L = function (X){((X/(input$VvD_l/1000)/data[VvD_Group(),"X75L_M"])^(-1/abs(data[VvD_Group(),"X75L_a"]))/(1/sqrt(g*(input$VvD_l/1000))))}
  V95U = function (X){((X/(input$VvD_l/1000)/data[VvD_Group(),"X95U_M"])^(-1/abs(data[VvD_Group(),"X95U_a"]))/(1/sqrt(g*(input$VvD_l/1000))))}
  V95L = function (X){((X/(input$VvD_l/1000)/data[VvD_Group(),"X95L_M"])^(-1/abs(data[VvD_Group(),"X95L_a"]))/(1/sqrt(g*(input$VvD_l/1000))))}
  
  # Create reactive to run function only once per user input
  V5Num <- reactive({V95U(SDnum())})
  V25Num <- reactive({V75U(SDnum())})
  V50Num <- reactive({V50(SDnum())})
  V75Num <- reactive({V75L(SDnum())})
  V95Num <- reactive({V95L(SDnum())})
  
  # Change alpha based on whether calculations are being made. This will fade the function lines
  Alpha2 <- reactive({
    if(input$Calculate2 == 1 | input$Calculate2 == 2){tempAlpha2 = 0.5}
    else tempAlpha2 = 1
    tempAlpha2
  })
  
  # Sloppy code that creates a dataframe containing the velocity vs distance data based on limits of swim time
  Plot2Data <- reactive({
    dataSet <- Velocity
    dataSet5 <- Velocity
    dataSet25 <- Velocity
    dataSet50 <- Velocity
    dataSet75 <- Velocity
    dataSet95 <- Velocity
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
  
  # Create plot for distance vs velocity
  output$Plot1 <- renderPlot(
    {ggplot(data=Plot2Data(), aes(Velocity)) +
        
        # Customize appearance of plot
        theme_classic()+ 
        theme(axis.line.x = element_line(color="black", size = .5),
              axis.line.y = element_line(color="black", size = .5))+
        theme(legend.key = element_blank(), legend.position="top", legend.text=element_text(size=12, family="lato"))+
        theme(axis.text = element_text(size=16, family="lato"), axis.title = element_text(size=16, family="lato"))+
        scale_x_log10(name = "Water velocity (m/s)", limits=c(0.1, 10), breaks = c(0.1,1,10))+
        scale_y_log10(name = "Swim distance (m)", limits=c(0.1, 1000), breaks = c(0.1,1,10,100,1000))+
        annotation_logticks(base = 10, sides = "b")+
        annotation_logticks(base = 10, sides = "VvD_l")+
        
        # Draw lines based on Plot2Data    
        geom_line(aes(y=X5,  colour = "95% prediction interval  "), size = 0.8, alpha=Alpha2(),  na.rm = TRUE)+
        geom_line(aes(y=X25, colour = "75% prediction interval  "), size = 0.8, alpha=Alpha2(),  na.rm = TRUE)+
        geom_line(aes(y=X50, colour = "Mean  "), size = 0.8, alpha=Alpha2(),  na.rm = TRUE)+
        geom_line(aes(y=X75, colour = "75% prediction interval  "), size = 0.8, alpha=Alpha2(),  na.rm = TRUE)+
        geom_line(aes(y=X95, colour = "95% prediction interval  "), size = 0.8, alpha=Alpha2(),  na.rm = TRUE)+
         

        # Add vertical dashed line to plot for swim distance calculations  
        {if(input$Calculate2 == '1') geom_vline(xintercept=WVnum(), linetype = "dashed", size = 0.8)}+
        
        # Add horizontal estimate lines for swim distance calculations
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
          geom_segment(aes(x = 0.1, y = max(Plot2Data()$X50,na.rm=TRUE), xend = Plot2Data()$Velocity[which.max(Plot2Data()$X50)], yend = max(Plot2Data()$X50,na.rm=TRUE)),linetype = "dashed", colour="grey65", size = 0.8)}+    
        
        {if(input$WVEst == TRUE & input$Calculate2 == '1' & X75Num() >= min(Plot2Data()$X75,na.rm=TRUE) & X75Num() <= max(Plot2Data()$X75,na.rm=TRUE))
          geom_segment(aes(x = 0.1, y = X75Num(), xend = WVnum(), yend = X75Num()), linetype = "dashed", colour="skyblue3", size = 0.8)}+
        {if(input$WVEst == TRUE & input$Calculate2 == '1' & X75Num() >= max(Plot2Data()$X75,na.rm=TRUE))
          geom_segment(aes(x = 0.1, y = max(Plot2Data()$X75,na.rm=TRUE), xend = Plot2Data()$Velocity[which.max(Plot2Data()$X75)], yend = max(Plot2Data()$X75,na.rm=TRUE)), linetype = "dashed", colour="skyblue3", size = 0.8)}+
        
        {if(input$WVEst == TRUE & input$Calculate2 == '1' & X95Num() >= min(Plot2Data()$X95,na.rm=TRUE) & X95Num() <= max(Plot2Data()$X95,na.rm=TRUE))
          geom_segment(aes(x = 0.1, y = X95Num(), xend = WVnum(), yend = X95Num()), linetype = "dashed", colour="orangered", size = 0.8)}+ 
        {if(input$WVEst == TRUE & input$Calculate2 == '1' & X95Num() >= max(Plot2Data()$X95,na.rm=TRUE))
          geom_segment(aes(x = 0.1, y = max(Plot2Data()$X95,na.rm=TRUE), xend = Plot2Data()$Velocity[which.max(Plot2Data()$X95)], yend = max(Plot2Data()$X95,na.rm=TRUE)), linetype = "dashed", colour="orangered", size = 0.8)}+    
        
        # Add horizontal dashed line to plot for velocity calculations        
        {if(input$Calculate2 == '2') geom_hline(yintercept=SDnum(), linetype = "dashed", size = 0.8)}+
        
        # Add vertical estimate lines for velocity calculations  
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
        
        # Add legend      
        scale_colour_manual("",values = c("gray65", "skyblue2","orangered"), 
                            breaks=c("Mean  ","75% prediction interval  ","95% prediction interval  "))
    },
    height = function() {
      session$clientData$output_Plot1_width*0.5
    })
  
  
  # Create plot title text  
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
  
  
  # Create output text for swim distance based on water velocity
  output$DistanceText5 <- renderText({
    if(X5Num() < min(Plot2Data()$X5,na.rm=TRUE)){
      DT5 <- paste("2.5% of ", input$VvD_l, "mm", VvD_Group(), "cannot swim against", WVnum(), "m/s current")}
    if(X5Num() >= min(Plot2Data()$X5,na.rm=TRUE) & X5Num() < max(Plot2Data()$X5,na.rm=TRUE)){
      DT5 <- paste("2.5% of ", input$VvD_l, "mm", VvD_Group(), "in ", WVnum(), "m/s current can swim ", signif(X5Num(), digits = 2), "m")}
    if(X5Num() >= max(Plot2Data()$X5,na.rm=TRUE)){
      DT5 <- paste("2.5% of", input$VvD_l, "mm", VvD_Group(), "in ", WVnum(), "m/s current can swim ", signif(max(Plot2Data()$X5,na.rm=TRUE), digits =2), "m")}
    DT5
  })
  
  output$DistanceText25 <- renderText({
    if(X25Num() < min(Plot2Data()$X25,na.rm=TRUE)){
      DT25 <- paste("12.5% of ", input$VvD_l, "mm", VvD_Group(), "cannot swim against", WVnum(), "m/s current")}
    if(X25Num() >= min(Plot2Data()$X25,na.rm=TRUE) & X25Num() < max(Plot2Data()$X25,na.rm=TRUE)){
      DT25 <- paste("12.5% of ", input$VvD_l, "mm", VvD_Group(), "in ", WVnum(), "m/s current can swim ", signif(X25Num(), digits = 2), "m")}
    if(X25Num() >= max(Plot2Data()$X25,na.rm=TRUE)){
      DT25 <- paste("12.5% of", input$VvD_l, "mm", VvD_Group(), "in ", WVnum(), "m/s current can swim ", signif(max(Plot2Data()$X25,na.rm=TRUE), digits =2), "m")}
    DT25
  })
  
  output$DistanceText50 <- renderText({
    if(X50Num() < min(Plot2Data()$X50,na.rm=TRUE)){
      DT50 <- paste("50% of ", input$VvD_l, "mm", VvD_Group(), "cannot swim against", WVnum(), "m/s current")}
    if(X50Num() >= min(Plot2Data()$X50,na.rm=TRUE) & X50Num() < max(Plot2Data()$X50,na.rm=TRUE)){
      DT50 <- paste("50% of ", input$VvD_l, "mm", VvD_Group(), "in ", WVnum(), "m/s current can swim ", signif(X50Num(), digits = 2), "m")}
    if(X50Num() >= max(Plot2Data()$X50,na.rm=TRUE)){
      DT50 <- paste("50% of", input$VvD_l, "mm", VvD_Group(), "in ", WVnum(), "m/s current can swim ", signif(max(Plot2Data()$X50,na.rm=TRUE), digits =2), "m")}
    DT50
  })
  
  output$DistanceText75 <- renderText({
    if(X75Num() < min(Plot2Data()$X75,na.rm=TRUE)){
      DT75 <- paste("87.5% of ", input$VvD_l, "mm", VvD_Group(), "cannot swim against", WVnum(), "m/s current")}
    if(X75Num() >= min(Plot2Data()$X75,na.rm=TRUE) & X75Num() < max(Plot2Data()$X75,na.rm=TRUE)){
      DT75 <- paste("87.5% of ", input$VvD_l, "mm", VvD_Group(), "in ", WVnum(), "m/s current can swim ", signif(X75Num(), digits = 2), "m")}
    if(X75Num() >= max(Plot2Data()$X75,na.rm=TRUE)){
      DT75 <- paste("87.5% of", input$VvD_l, "mm", VvD_Group(), "in ", WVnum(), "m/s current can swim ", signif(max(Plot2Data()$X75,na.rm=TRUE), digits =2), "m")}
    DT75
  })
  
  output$DistanceText95 <- renderText({
    if(X95Num() < min(Plot2Data()$X95,na.rm=TRUE)){
      DT95 <- paste("97.5% of ", input$VvD_l, "mm", VvD_Group(), "cannot swim against", WVnum(), "m/s current")}
    if(X95Num() >= min(Plot2Data()$X95,na.rm=TRUE) & X95Num() < max(Plot2Data()$X95,na.rm=TRUE)){
      DT95 <- paste("97.5% of ", input$VvD_l, "mm", VvD_Group(), "in ", WVnum(), "m/s current can swim ", signif(X95Num(), digits = 2), "m")}
    if(X95Num() >= max(Plot2Data()$X95,na.rm=TRUE)){
      DT95 <- paste("97.5% of", input$VvD_l, "mm", VvD_Group(), "in ", WVnum(), "m/s current can swim ", signif(max(Plot2Data()$X95,na.rm=TRUE), digits =2), "m")}
    DT95
  })
  
  
  # Create output text for water velocity based on swim distance
  output$VelocityText5 <- renderText({
    if(V5Num()  < Plot2Data()$Velocity[which.max(Plot2Data()$X5)]){
      VT5 <- paste("2.5% of ", input$VvD_l, "mm", VvD_Group(), "cannot swim ", SDnum(), "m in any current")}
    if((V5Num() <= Plot2Data()$Velocity[which.min(Plot2Data()$X5)]) & V5Num()  >= Plot2Data()$Velocity[which.max(Plot2Data()$X5)]){
      VT5 <- paste("2.5% of ", input$VvD_l, "mm", VvD_Group(), "can swim in ", signif(V5Num(), digits = 2), "m/s current for at least ", SDnum(), " m")}
    if(V5Num()  > Plot2Data()$Velocity[which.min(Plot2Data()$X5)]){
      VT5 <- paste("2.5% of ", input$VvD_l, "mm", VvD_Group(), "can swim in ", signif(Plot2Data()$Velocity[which.min(Plot2Data()$X5)], digits = 2),  "m/s current for at least ", SDnum(), " m")}
    VT5
  })
  
  output$VelocityText25 <- renderText({
    if(V25Num()  < Plot2Data()$Velocity[which.max(Plot2Data()$X25)]){
      VT25 <- paste("12.5% of ", input$VvD_l, "mm", VvD_Group(), "cannot swim ", SDnum(), "m in any current")}
    if((V25Num() <= Plot2Data()$Velocity[which.min(Plot2Data()$X25)]) & V25Num()  >= Plot2Data()$Velocity[which.max(Plot2Data()$X25)]){
      VT25 <- paste("12.5% of ", input$VvD_l, "mm", VvD_Group(), "can swim in ", signif(V25Num(), digits = 2), "m/s current for at least ", SDnum(), " m")}
    if(V25Num()  > Plot2Data()$Velocity[which.min(Plot2Data()$X25)]){
      VT25 <- paste("12.5% of ", input$VvD_l, "mm", VvD_Group(), "can swim in ", signif(Plot2Data()$Velocity[which.min(Plot2Data()$X25)], digits = 2),  "m/s current for at least ", SDnum(), " m")}
    VT25
  })
  
  output$VelocityText50 <- renderText({
    if(V50Num()  < Plot2Data()$Velocity[which.max(Plot2Data()$X50)]){
      VT50 <- paste("50% of ", input$VvD_l, "mm", VvD_Group(), "cannot swim ", SDnum(), "m in any current")}
    if((V50Num() <= Plot2Data()$Velocity[which.min(Plot2Data()$X50)]) & V50Num()  >= Plot2Data()$Velocity[which.max(Plot2Data()$X50)]){
      VT50 <- paste("50% of ", input$VvD_l, "mm", VvD_Group(), "can swim in ", signif(V50Num(), digits = 2), "m/s current for at least ", SDnum(), " m")}
    if(V50Num()  > Plot2Data()$Velocity[which.min(Plot2Data()$X50)]){
      VT50 <- paste("50% of ", input$VvD_l, "mm", VvD_Group(), "can swim in ", signif(Plot2Data()$Velocity[which.min(Plot2Data()$X50)], digits = 2),  "m/s current for at least ", SDnum(), " m")}
    VT50
  })
  
  output$VelocityText75 <- renderText({
    if(V75Num()  < Plot2Data()$Velocity[which.max(Plot2Data()$X75)]){
      VT75 <- paste("87.5% of ", input$VvD_l, "mm", VvD_Group(), "cannot swim ", SDnum(), "m in any current")}
    if((V75Num() <= Plot2Data()$Velocity[which.min(Plot2Data()$X75)]) & V75Num()  >= Plot2Data()$Velocity[which.max(Plot2Data()$X75)]){
      VT75 <- paste("87.5% of ", input$VvD_l, "mm", VvD_Group(), "can swim in ", signif(V75Num(), digits = 2), "m/s current for at least ", SDnum(), " m")}
    if(V75Num()  > Plot2Data()$Velocity[which.min(Plot2Data()$X75)]){
      VT75 <- paste("87.5% of ", input$VvD_l, "mm", VvD_Group(), "can swim in ", signif(Plot2Data()$Velocity[which.min(Plot2Data()$X75)], digits = 2),  "m/s current for at least ", SDnum(), " m")}
    VT75
  })
  
  output$VelocityText95 <- renderText({
    if(V95Num()  < Plot2Data()$Velocity[which.max(Plot2Data()$X95)]){
      VT95 <- paste("97.5% of ", input$VvD_l, "mm", VvD_Group(), "cannot swim ", SDnum(), "m in any current")}
    if((V95Num() <= Plot2Data()$Velocity[which.min(Plot2Data()$X95)]) & V95Num()  >= Plot2Data()$Velocity[which.max(Plot2Data()$X95)]){
      VT95 <- paste("97.5% of ", input$VvD_l, "mm", VvD_Group(), "can swim in ", signif(V95Num(), digits = 2), "m/s current for at least ", SDnum(), " m")}
    if(V95Num()  > Plot2Data()$Velocity[which.min(Plot2Data()$X95)]){
      VT95 <- paste("97.5% of ", input$VvD_l, "mm", VvD_Group(), "can swim in ", signif(Plot2Data()$Velocity[which.min(Plot2Data()$X95)], digits = 2),  "m/s current for at least ", SDnum(), " m")}
    VT95
  })
  
}


enableBookmarking(store = "url")

shinyApp( ui = ui, server = server)


#runApp("app.r")