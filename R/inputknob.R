#' @export
html_dependency_inputknob <- function() {
  list(
    dependency_shinywc(),
    htmltools::htmlDependency(
      name = "input-knob",
      version = "1.0.0",
      src = "wc/inputknob/lib",
      package = "shinywc",
      script = list(src = "input-knob.js", type = "module")
    )
  )
}

#' Create a `<input-knob>` web component
#'
#' Binding to the `<input-knob>` web component version 1.0.0.
#'
#' @param id ID for the component. If not provided, an ID will be automatically
#' generated.
#' @param value TODO
#' @param scale TODO
#' @param min Required
#' @param max Required
#' @param slot Content to place in the default (unnamed) slot.
#' @param slot_back_side TODO
#' @param css_knob_size TODO
#' @param ... Any additional HTML attributes to add to the element tag.
#' @export
inputknob <- function(
  id = NULL,
  value = NULL,
  scale = NULL,
  min = NULL,
  max = NULL,
  slot = NULL,
  slot_back_side = NULL,
  css_knob_size = NULL,
  ...
  ) {

  required_params(min, max)

  params <- eval(substitute(alist(...)))
  if (length(params) > 0) {
    if (is.null(names(params)) || any(names(params) == "")) {
      stop("inputknob: additional parameters must be named attributes", call. = FALSE)
    }
  }

  if (is.null(id)) {
    id <- paste0('input-knob-', sample(1e9, 1))
  }

  if (!is.null(slot_back_side)) {
    if (!inherits(slot_back_side, "shiny.tag")) {
      stop("input-knob: slot-back-side must be a valid HTML tag", call. = FALSE)
    }
    slot_back_side <- shiny::tagAppendAttributes(
      slot_back_side,
      slot = "back-side"
    )
  }

  style <- NULL
  if (!is.null(css_knob_size)) {
    style <- paste0("--", "knob-size", ":", css_knob_size, ";")
  }

  component_details <- list(
    name = "input-knob",
    attributes = list("value", "scale", "min", "max"),
    events = list("knob-move-change", "knob-move-start", "knob-move-end")
  )
  component_details <- jsonlite::toJSON(component_details, auto_unbox = TRUE)

  component_tag <- htmltools::tagList(
    htmltools::tag(
      'input-knob',
      .noWS = c("after-begin", "before-end"),
      varArgs = list(
        id = id,
        value = value,
        scale = scale,
        min = min,
        max = max,
        slot,
        slot_back_side,
        style = style,
        ...
      )
    ),
    htmltools::singleton(htmltools::tags$head(
      htmltools::tags$script(sprintf("shinywc.setupComponent(%s)", component_details))
    )),
    htmltools::tags$script(sprintf("shinywc.registerComponent('%s', '%s')", "input-knob", id))
  )
  htmltools::attachDependencies(component_tag, html_dependency_inputknob())
}

#' @export
InputKnob <- R6::R6Class(
  "InputKnob",

  private = list(
    .id = NULL,
    .id_noNS = NULL,
    .session = NULL,
    .attributes = list(),

    set_attr = function(attr, value) {
      private$.session$sendCustomMessage('shinywc-attr-set', list(
        id = private$.id,
        attr = attr,
        value = value
      ))
    },

    get_attr = function(attr) {
      if (!attr %in% names(private$.attributes)) {
        return(NULL)
      }
      private$.attributes[[attr]]
    },

    set_prop = function(prop, value) {
      private$.session$sendCustomMessage('shinywc-prop-set', list(
        id = private$.id,
        prop = prop,
        value = value
      ))
    },

    get_prop = function(prop, cb) {
      cbid_noNS <- paste0("__input-knob-", prop, "-", sample(1e9, 1))
      cbid <- private$.session$ns(cbid_noNS)
      private$.session$sendCustomMessage('shinywc-prop-get', list(
        id = private$.id,
        prop = prop,
        cbid = cbid
      ))
      shiny::observeEvent(private$.session$input[[cbid_noNS]], once = TRUE, {
        cb(private$.session$input[[cbid_noNS]])
      })
    },

    call_method = function(method, params = list()) {
      private$.session$sendCustomMessage('shinywc-call-method', list(
        id = private$.id,
        method = method,
        params = params
      ))
    }
  ),

  public = list(

    initialize = function(id, session = shiny::getDefaultReactiveDomain()) {
      if (is.null(session)) {
        stop("InputKnob can only be initialized in a Shiny environment")
      }
      private$.session <- session
      private$.id_noNS <- id
      private$.id <- private$.session$ns(private$.id_noNS)

      private$.session$sendCustomMessage('shinywc-init-component', list(
        id = private$.id
      ))

      shiny::observeEvent(private$.session$input[[paste0(private$.id_noNS, "_input-knob-attr-change")]], {
        evt <- private$.session$input[[paste0(private$.id_noNS, "_input-knob-attr-change")]]
        attr_name <- names(evt[1])
        attr_val <- evt[[1]]
        private$.attributes[[attr_name]] <- attr_val
      })
    },

    id = function() {
      private$.id_noNS
    },

    event_knob_move_change = function() {
      private$.session$input[[paste0(private$.id_noNS, "_event_knob-move-change")]]
    },
    event_knob_move_start = function() {
      private$.session$input[[paste0(private$.id_noNS, "_event_knob-move-start")]]
    },
    event_knob_move_end = function() {
      private$.session$input[[paste0(private$.id_noNS, "_event_knob-move-end")]]
    },

    get_value = function() {
      private$get_attr("value")
    },
    set_value = function(value) {
      private$set_attr("value", value)
    },
    get_scale = function() {
      private$get_attr("scale")
    },
    set_scale = function(value) {
      private$set_attr("scale", value)
    },
    get_min = function() {
      private$get_attr("min")
    },
    set_min = function(value) {
      private$set_attr("min", value)
    },
    get_max = function() {
      private$get_attr("max")
    },
    set_max = function(value) {
      private$set_attr("max", value)
    },

    get_value_prop = function(cb) {
      private$get_prop("value", cb)
    },
    set_value_prop = function(value) {
      private$set_prop("value", value)
    },
    get_scale_prop = function(cb) {
      private$get_prop("scale", cb)
    },
    set_scale_prop = function(value) {
      private$set_prop("scale", value)
    },
    get_min_prop = function(cb) {
      private$get_prop("min", cb)
    },
    set_min_prop = function(value) {
      private$set_prop("min", value)
    },
    get_max_prop = function(cb) {
      private$get_prop("max", cb)
    },
    set_max_prop = function(value) {
      private$set_prop("max", value)
    },

    call_rotateLeft = function(turns) {
      params <- list(turns)
      private$call_method("rotateLeft", params)
    },
    call_rotateRight = function(turns) {
      params <- list(turns)
      private$call_method("rotateRight", params)
    }

  )

)
