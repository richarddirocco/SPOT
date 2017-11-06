library(shiny)
library(shinyAce)
library(sendmailR)
sendmail_options(verboseShow=TRUE)

ui <- (pageWithSidebar(
  headerPanel("Feedback"),
  
  sidebarPanel(
    textInput("From", "Name:", value=""),
    textInput("FromEmail", "E-mail:", ""),
    textInput("Subject", "Subject:", value=""),
    tags$head(tags$script(src = "message-handler.js")),
    actionButton("Send", "Send feedback")
  ),
  
  mainPanel(    
    aceEditor("Message", value="Write message here",
              showLineNumbers = FALSE,
              highlightActiveLine = FALSE,
              height = "295px")
  )
))

server <- function(input, output, session) {
  
  observe({
    if(is.null(input$send) || input$send==0) return(NULL)
    from <- "richard.dirocco@dfo-mpo.gc.ca"
    to <- "richard.dirocco@dfo-mpo.gc.ca"
    subject <- "Feedback FishProtectionTools.ca"
    msg <- isolate(input$Message)
    sendmail(from, to, subject, msg)
  })
  
  observeEvent(input$Send, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Thank you for your feedback')
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

