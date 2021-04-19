#' Create a `<input-knob>` web component
#'
#' Binding to the `<input-knob>` web component version 1.0.0.
#'
#' @param id ID for the component. If not provided, an ID will be automatically
#' generated.
#' @param slot Content to place in the default (unnamed) slot.
#' @param ... Any additional HTML attributes to add to the element tag.
#' @export
inputknob <- function(
  id = NULL,
  value = NULL,
  scale = NULL,
  min = NULL,
  max = NULL,
  slot = NULL,
  `slot-back-side` = NULL,
  `css-knob-size` = NULL,
  ...
  ) {

  params <- eval(substitute(alist(...)))
  if (length(params) > 0) {
    if (is.null(names(params)) || any(names(params) == "")) {
      stop("inputknob: additional parameters must be named attributes")
    }
  }

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
        `slot-back-side`,
        style = style,
        ...
      )
    ),
    htmltools::tags$script(paste0("shinywcInputKnob.checkInit('", id, "')"))
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
      private$.session$sendCustomMessage('input-knob-attr-set', list(
        id = private$.id,
        attr = attr,
        value = value
      ))
    },

    get_attr = function(attr) {
      if (!attr %in% names(private$.attributes)) {
        stop(attr, " is not in the list of known attributes")
      }
      private$.attributes[[attr]]
    },

    set_prop = function(prop, value) {
      private$.session$sendCustomMessage('input-knob-prop-set', list(
        id = private$.id,
        prop = prop,
        value = value
      ))
    },

    get_prop = function(prop, cb) {
      cbid_noNS <- paste0("__input-knob-", prop, "-", sample(1e9, 1))
      cbid <- private$.session$ns(cbid_noNS)
      private$.session$sendCustomMessage('input-knob-prop-get', list(
        id = private$.id,
        prop = prop,
        cbid = cbid
      ))
      shiny::observeEvent(private$.session$input[[cbid_noNS]], once = TRUE, {
        cb(private$.session$input[[cbid_noNS]])
      })
    },

    call_method = function(method, args = list()) {
      private$.session$sendCustomMessage('input-knob-call', list(
        id = private$.id,
        method = method,
        args = args
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

      private$.session$sendCustomMessage('input-knob-init', list(
        id = private$.id
      ))

      shiny::observeEvent(private$.session$input[[paste0(private$.id_noNS, "_knob-attr-change")]], {
        evt <- private$.session$input[[paste0(private$.id_noNS, "_knob-attr-change")]]
        attr_name <- names(evt[1])
        attr_val <- evt[[1]]
        private$.attributes[[attr_name]] <- attr_val
      })
    },

    id = function() {
      private$.id_noNS
    },

    event_knob_move_change = function() {
      private$.session$input[[paste0(private$.id_noNS, "_knob-move-change")]]
    },
    event_knob_move_start = function() {
      private$.session$input[[paste0(private$.id_noNS, "_knob-move-start")]]
    },
    event_knob_move_end = function() {
      private$.session$input[[paste0(private$.id_noNS, "_knob-move-end")]]
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

    call_rotateLeft = function(args) {
      private$call_method("rotateLeft", args)
    },
    call_rotateRight = function(args) {
      private$call_method("rotateRight", args)
    }

  )

)
