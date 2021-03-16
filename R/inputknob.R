#' Create a `<input-knob>` web component
#'
#' Binding to the `<input-knob>` web component version 1.0.0.
#'
#' @param id ID for the component. If not provided, an ID will be automatically
#' generated.
#' @param ... Content to place in the default (unnamed) slot.
#' @export
inputknob <- function(
  id = NULL,
  value = NULL,
  scale = NULL,
  min = NULL,
  max = NULL,
  ...,
  `slot-back-side` = NULL,
  `css-knob-size` = NULL
  ) {

  if (is.null(id)) {
    id <- paste0('input-knob-', sample(1e9, 1))
  }

  if (!is.null(`slot-back-side`)) {
    if (!inherits(`slot-back-side`, "shiny.tag")) {
      stop("inputknob: slot-back-side must be a valid HTML tag")
    }
    `slot-back-side` <- shiny::tagAppendAttributes(
      `slot-back-side`,
      slot = "back-side"
    )
  }

  style <- NULL
  if (!is.null(`css-knob-size`)) {
    style <- paste0("--", "knob-size", ":", `css-knob-size`, ";")
  }

  component_tag <-
    htmltools::attachDependencies(
      htmltools::tag('input-knob', varArgs = list(
        id = id,
        value = value,
        scale = scale,
        min = min,
        max = max,
        ...,
        `slot-back-side`,
        style = style
      )),
      deps
    )



  component_tag
}

#' @export
inputknob_set_attr <- function(id, attr, value) {
  if (!attr %in% POSSIBLE_ATTRS) {
    stop(attr, "is not a supported attribute to set")
  }
  session <- shiny::getDefaultReactiveDomain()
  session$sendCustomMessage('input-knob-attr', list(
    id = id,
    attr = attr,
    value = value
  ))
}

#' Retrieve the value of an attribute from the component
#'
#' @param cb Callback function that gets called when the value is retrieved.
#' The function has has one argument which holds the value of the attribute.
#' @export
inputknob_get_attr <- function(id, attr, cb) {
  if (!attr %in% POSSIBLE_ATTRS) {
    stop(attr, "is not a supported attribute to set")
  }
  session <- shiny::getDefaultReactiveDomain()
  cbid <- paste0("__inputknob-", sample(1e9, 1))
  session$sendCustomMessage('input-knob-get', list(
    id = id,
    attr = "value",
    cbid = cbid
  ))
  shiny::observeEvent(session$input[[cbid]], once = TRUE, {
    cb(session$input[[cbid]])
  })
}



#' Call a method on the component
#' @export
inputknob_call <- function(id, method, args = list()) {
  if (!method %in% POSSIBLE_METHODS) {
    stop(method, "is not a supported method to call")
  }

  session <- shiny::getDefaultReactiveDomain()
  session$sendCustomMessage('input-knob-call', list(
    id = id,
    method = method,
    args = args
  ))
}
