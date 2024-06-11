testButton <- function(id, label="Test Boyo"){
  ns<-NS(id)
  tagList(
    actionButton(ns("testButton1"), label=label),
    verbatimTextOutput(ns("out"), placeholder=T)
  )
}

#test_ui <- function(id, input, output){
# testButton("testbutton1", "Test Button #1")
#}

#test_server <- function(id) {
#  moduleServer(id, function(input, output, session) {
#
#observeEvent(input$testButton1, {
#  output$out <- renderText("It works!")
#  showNotification("It works")
#})
#  })
#}


testServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      count <- reactiveVal(0)
      observeEvent(input$testButton1, {
        count(count() + 1)
      })
      output$out <- renderText({
        count()
      })
      count
    }
  )
}