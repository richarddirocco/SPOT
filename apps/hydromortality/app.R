## app.R ##
library(shiny)
library(shinydashboard)

# Import K and b values for each group of fish (used in equations)
data <- read.csv("GroupVariables.csv")
rownames(data)<-data$Group
data$Group <- NULL

# Import fish width and depth data
# From "The Inland Fishes of New York State" by Lavett Smith. Published in 1985.

Fish_Width_Depth_Data <- read.csv("Fish_Width_Depth_Data.csv")
rownames(Fish_Width_Depth_Data)<-Fish_Width_Depth_Data$Name
Fish_Width_Depth_Data$Name <- NULL

# Create an object with different lengths between 0-1000 mm
Length <- seq(from=0, to=1000, by=1)

ui <- dashboardPage(
  dashboardHeader(title = "Hydro Mortality Calculator"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Blade Strike", tabName = "BladeStrike", icon = icon("asterisk")),
      menuItem("Approach Velocity", tabName = "ApproachVelocity", icon = icon("dashboard"))
    )
  ),
  ## Body content
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      tabItem(tabName = "BladeStrike",
        fluidRow(
          box(
            title = "Controls",
            sliderInput("F_Lambda", "Strike mortality correlation factor (λ)", min = 0, max = 1, value = 0),
            numericInput("F_Blades", "Number of blades or buckets (N)", 4, min = NA, max = NA, step = NA),
            numericInput("F_FishLength", "Fish length in mm (L)", 25, min = 0, max = 1500, step = 5),
            numericInput("F_Diameter", "Diameter of runner in mm (D)", 1000, min = 0, max = NA, step = 10),
            withMathJax(), 
            numericInput("F_Angle", label = "Angle to axial of absolute flow upstream of runner (\\( \\alpha_{\\alpha} \\))", 0),
            numericInput("F_RPM", label = "Rotational speed in RPM (ω)", 0),
            numericInput("F_Discharge", label = "Turbine discharge in length\\( ^3\\)/time (Q)", 0)
          )    
          #box(plotOutput("plot1", height = 250))
        )
      ),
      tabItem(tabName = "ApproachVelocity",
              h2("Widgets tab content"),
              box(
                title = "Controls",
                numericInput("AV_Approach_Velocity", "Approach velocity in m/s", min = 0, value = 0.15),
                sliderInput("AV_Time", label = "Time to escape entrainment/impingement (min):", min = 1, max = 30, value = 10, step = 0.5),
                numericInput("AV_Spacing", "Maximum spacing of bar rack or screen in mm", min = 0, value = 2.56),
                helpText("Enter a value of 0 if there is no screen or trash rack")
              ),
              box(plotOutput("AV_Plot", height = 250)),
              dataTableOutput('testtable')
      )
    )
  )
)

server <- function(input, output) {
  # Set gravitational acceleration constant
  g = 9.81
  
  AV_func = function (t,Group,Length){data[Group,"k"]*sqrt(g*Length/1000)*((sqrt(Length/1000/g))^-data[Group,"X95L_b"])*t^data[Group,"X95L_b"]}
  
  # Set some variable for testing purposes
  AV_CompareData <- reactive({
    AV_dataSet <- data.frame(Length)
    AV_dataSet$CatfishSunfish <- AV_func(input$AV_Time*60, "Catfish & Sunfish", Length)
    AV_dataSet$Eel <- AV_func(input$AV_Time*60, "Eel", Length)
    AV_dataSet$Herring <- AV_func(input$AV_Time*60, "Herring", Length)
    AV_dataSet$Pike <- AV_func(input$AV_Time*60, "Pike", Length)
    AV_dataSet$SalmonWalleye <- AV_func(input$AV_Time*60, "Salmon & Walleye", Length)
    AV_dataSet$Sturgeon <- AV_func(input$AV_Time*60, "Sturgeon", Length)
    AV_dataSet
  })
  
  AV_PlotData <- reactive({
    
  })
  
  output$testtable = renderDataTable({
    AV_CompareData()
  })
}

shinyApp(ui, server)
