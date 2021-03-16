library(shiny)
library(inputknob)

ui <- fluidPage(
  selectInput("attr", "Set attribute", c("value", "scale", "max", "min")),
  numericInput("num", "value", 70),
  actionButton("go", "go"),
  actionButton("rotateleft", "rotateleft"),
  actionButton("get_value", "get value"),
  inputknob(
    id = "testknob",
    value = 50, scale = 10, min = 0, max = 100,
    "^",
    `slot-back-side` = tags$span('..'),
    `css-knob-size` = "100px"
  ),
  div("Value:", textOutput("value"))
)

server <- function(input, output, session) {
  output$value <- renderText({
    input$testknob_end
  })

  observeEvent(input$rotateleft, {
    inputknob_call("testknob", "rotateLeft", 1)
  })

  observeEvent(input$go, {
    inputknob_set_attr(id = "testknob", attr = input$attr, value = input$num)
  })

  observeEvent(input$get_value, {
    inputknob_get_attr(id = "testknob", attr = "value", cb = function(x) { shinyalert::shinyalert(x)})
  })
}

shinyApp(ui, server)
