library(shiny)
library(inputknob)

ui <- fluidPage(
  h1("<input-knob> web component Shiny demo"),

  inputknob(
    id = "testknob",
    value = 50, scale = 10, min = 0, max = 100,
    slot = "^",
    `slot-back-side` = tags$span('..'),
    `css-knob-size` = "150px"
  ),

  "'knob-move-end' event is triggered:",
  textOutput("value", inline = TRUE),

  fluidRow(
    column(
      3,
      h3("Set attribute"),
      selectInput("attr", "Attribute", c("value", "scale", "max", "min")),
      numericInput("num", "value", 70),
      actionButton("go", "Set")
    ),
    column(
      3,
      h3("Rotate right"),
      numericInput("rotatenum", "How many turns?", 0.1),
      actionButton("dorotate", "Rotate")
    ),
    column(
      3,
      h3("Get attribute"),
      actionButton("get_value", "Show value attribute"),
    )
  )
)

server <- function(input, output, session) {
  output$value <- renderText({
    input$`testknob_knob-move-end`
  })

  observeEvent(input$dorotate, {
    inputknob_call("testknob", "rotateRight", input$rotatenum)
  })

  observeEvent(input$go, {
    inputknob_set_attr(id = "testknob", attr = input$attr, value = input$num)
  })

  observeEvent(input$get_value, {
    inputknob_get_attr(id = "testknob", attr = "value", cb = function(x) { shinyalert::shinyalert(x)})
  })
}

shinyApp(ui, server)
