#' @import htmlwidgets
#'
#' @export
inputknob_nowork <- function(message, width = NULL, height = NULL, elementId = NULL) {

  # forward options using x
  x = list(
    message = message
  )

  deps <- list(
    htmltools::htmlDependency(
      name = "input-knob",
      version = "1.0.0",
      src = "inst/htmlwidgets",
      stylesheet = "input-knob.css",
      script = list(src = "input-knob.js", type = "module")
    )
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'inputknob',
    x,
    width = width,
    height = height,
    package = 'inputknob',
    elementId = elementId,
    dependencies = deps
  )
}

#' Shiny bindings for inputknob
#'
#' Output and render functions for using inputknob within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a inputknob
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name inputknob-shiny
#'
#' @export
inputknobOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'inputknob', width, height, package = 'inputknob')
}

#' @rdname inputknob-shiny
#' @export
renderInputknob <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, inputknobOutput, env, quoted = TRUE)
}
