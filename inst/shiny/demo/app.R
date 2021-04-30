library(shiny)
library(inputknob)

test_ui <- function(id) {
  ns <- NS(id)
  tagList(
    inputknob(
      id = ns("testknob"),
      value = 50, scale = 10, min = 0, max = 100,
      slot = "^",
      slot_back_side = tags$span('..'),
      css_knob_size = "150px"
    ),

    fluidRow(
      column(
        2,
        h3("Get attribute"),
        selectInput(ns("get_attr_id"), "Attribute", c("value", "scale", "max", "min")),
        actionButton(ns("get_attr"), "Show attribute"),
      ),
      column(
        2,
        h3("Set attribute"),
        selectInput(ns("set_attr_id"), "Attribute", c("value", "scale", "max", "min")),
        numericInput(ns("set_attr_val"), "value", 70),
        actionButton(ns("set_attr"), "Set")
      ),
      column(
        2,
        h3("Get property"),
        selectInput(ns("get_prop_id"), "Property", c("value", "scale", "max", "min")),
        actionButton(ns("get_prop"), "Show property"),
      ),
      column(
        2,
        h3("Set property"),
        selectInput(ns("set_prop_id"), "Property", c("value", "scale", "max", "min")),
        numericInput(ns("set_prop_val"), "Value", 50),
        actionButton(ns("set_prop"), "Set")
      ),
      column(
        2,
        h3("Call method"),
        selectInput(ns("method"), "Method", c("rotateRight", "rotateLeft")),
        numericInput(ns("rotatenum"), "How many turns?", 0.1),
        actionButton(ns("call"), "Go")
      ),
      column(
        2,
        h3("Events"),
        h4("knob-move-change:"), textOutput(ns("event_change")),
        h4("knob-move-start:"), textOutput(ns("event_start")),
        h4("knob-move-end:"), textOutput(ns("event_end"))
      )
    )
  )
}

test_server <- function(input, output, session) {
  knob <- InputKnob$new("testknob")

  observeEvent(input$get_attr, {
    fnx <- paste0("get_", input$get_attr_id)
    val <- knob[[fnx]]()
    shinyalert::shinyalert(text = val)
  })

  observeEvent(input$set_attr, {
    fnx <- paste0("set_", input$set_attr_id)
    knob[[fnx]](input$set_attr_val)
  })

  observeEvent(input$get_prop, {
    fnx <- paste0("get_", input$get_prop_id, "_prop")
    knob[[fnx]](function(x){ shinyalert::shinyalert(text = as.character(jsonlite::toJSON(x))) })
  })

  observeEvent(input$set_prop, {
    fnx <- paste0("set_", input$set_prop_id, "_prop")
    knob[[fnx]](input$set_prop_val)
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
  h1("<input-knob> web component Shiny demo"),
  test_ui("test")
)

server <- function(input, output, session) {
  callModule(test_server, "test")
}

shinyApp(ui, server)
