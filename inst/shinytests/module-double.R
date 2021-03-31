library(shiny)
library(inputknob)

test_UI <- function(id) {
  ns <- NS(id)
  tagList(
    inputknob(
      id = ns("testknob"),
      value = 50, scale = 10, min = 0, max = 100,
      slot = "^",
      `slot-back-side` = tags$span('..'),
      `css-knob-size` = "150px"
    ),

    fluidRow(
      column(
        3,
        h3("Set attribute"),
        selectInput(ns("attr"), "Attribute", c("value", "scale", "max", "min")),
        numericInput(ns("num"), "value", 70),
        actionButton(ns("set"), "Set")
      ),
      column(
        3,
        h3("Get attribute"),
        selectInput(ns("attr_get"), "Attribute", c("value", "scale", "max", "min")),
        actionButton(ns("get"), "Show attribute"),
      ),
      column(
        3,
        h3("Call methods"),
        selectInput(ns("method"), "Method", c("rotateRight", "rotateLeft")),
        numericInput(ns("rotatenum"), "How many turns?", 0.1),
        actionButton(ns("call"), "Go")
      ),
      column(
        3,
        h3("Events"),
        div("knob-move-change:", textOutput(ns("event_change"), inline = TRUE)),
        div("knob-move-start:", textOutput(ns("event_start"), inline = TRUE)),
        div("knob-move-end:", textOutput(ns("event_end"), inline = TRUE))
      )
    )
  )
}

test <- function(input, output, session) {
  knob <- InputKnob$new("testknob")

  observeEvent(input$set, {
    fnx <- paste0("set_", input$attr)
    knob[[fnx]](input$num)
  })

  observeEvent(input$get, {
    fnx <- paste0("get_", input$attr_get)
    val <- knob[[fnx]]()
    shinyalert::shinyalert(val)
  })

  observeEvent(input$call, {
    fnx <- paste0("call_", input$method)
    knob[[fnx]](input$rotatenum)
  })

  output$event_change <- renderText({
    req(knob$event_knob_move_change())
    paste(Sys.time(), knob$event_knob_move_change())
  })
  output$event_start <- renderText({
    req(knob$event_knob_move_start())
    paste(Sys.time(), knob$event_knob_move_start())
  })
  output$event_end <- renderText({
    req(knob$event_knob_move_end())
    paste(Sys.time(), knob$event_knob_move_end())
  })
}

ui <- fluidPage(
  h1("<input-knob> web component Shiny demo - two modules"),
  test_UI("test"),
  test_UI("test2")
)

server <- function(input, output, session) {
  callModule(test, "test")
  callModule(test, "test2")
}

shinyApp(ui, server)
