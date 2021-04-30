.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath("epiviz", system.file("wc", "epiviz", "lib", package = "shinywc"))
}

#' @export
html_dependency_genestrack <- function() {
  list(
    dependency_shinywc(),
    dependency_jqueryui(),
    dependency_webcomponentsjs(version = "1.1.0")
  )
}


#' @export
epivizGenesTrackUI <- function(
  id = NULL,
  json_data = NULL,
  chart_colors = NULL,
  ...
) {

  required_params(json_data)

  params <- eval(substitute(alist(...)))
  if (length(params) > 0) {
    if (is.null(names(params)) || any(names(params) == "")) {
      stop("genesTrackUI: additional parameters must be named attributes")
    }
  }

  if (is.null(id)) {
    id <- paste0('epiviz-genes-track-', sample(1e9, 1))
  }

  component_details <- list(
    name = "epiviz-genes-track",
    attributes = list("json-data", "chart-colors"),
    events = list("dimChanged", "hover", "unHover")
  )
  component_details <- jsonlite::toJSON(component_details, auto_unbox = TRUE)

  component_tag <- htmltools::tagList(
    htmltools::tag(
      'epiviz-genes-track',
      .noWS = c("after-begin", "before-end"),
      varArgs = list(
        id = id,
        `json-data` = json_data,
        `chart-colors` = chart_colors,
        ...
      )
    ),
    htmltools::singleton(htmltools::tags$head(
      htmltools::tags$script(sprintf("shinywc.setupComponent(%s)", component_details)),
      htmltools::tags$link(rel = "import", href = "epiviz/epiviz-components.html")
    )),
    htmltools::tags$script(sprintf("shinywc.registerComponent('%s', '%s')", "epiviz-genes-track", id))
  )
  htmltools::attachDependencies(component_tag, html_dependency_genestrack())
}

#' @export
EpivizGenesTrack <- R6::R6Class(
  "EpivizGenesTrack",

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
      cbid_noNS <- paste0("__epiviz-genes-track-", prop, "-", sample(1e9, 1))
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
        stop("EpivizGenesTrack can only be initialized in a Shiny environment")
      }
      private$.session <- session
      private$.id_noNS <- id
      private$.id <- private$.session$ns(private$.id_noNS)

      private$.session$sendCustomMessage('shinywc-init-component', list(
        id = private$.id
      ))

      shiny::observeEvent(private$.session$input[[paste0(private$.id_noNS, "_epiviz-genes-track-attr-change")]], {
        evt <- private$.session$input[[paste0(private$.id_noNS, "_epiviz-genes-track-attr-change")]]
        attr_name <- names(evt[1])
        attr_val <- evt[[1]]
        private$.attributes[[attr_name]] <- attr_val
      })
    },

    id = function() {
      private$.id_noNS
    },

    event_dimChanged = function() {
      private$.session$input[[paste0(private$.id_noNS, "_event_dimChanged")]]
    },
    event_hover = function() {
      private$.session$input[[paste0(private$.id_noNS, "_event_hover")]]
    },
    event_unHover = function() {
      private$.session$input[[paste0(private$.id_noNS, "_event_unHover")]]
    },

    get_json_data = function() {
      private$get_attr("json-data")
    },
    set_json_data = function(value) {
      private$set_attr("json-data", value)
    },
    get_chart_colors = function() {
      private$get_attr("chart-colors")
    },
    set_chart_colors = function(value) {
      private$set_attr("chart-colors", value)
    },
    get_chartColors_prop = function(cb) {
      private$get_prop("chartColors", cb)
    },
    set_chartColors_prop = function(value) {
      private$set_prop("chartColors", value)
    },
    get_chartSettings_prop = function(cb) {
      private$get_prop("chartSettings", cb)
    },
    set_chartSettings_prop = function(value) {
      private$set_prop("chartSettings", value)
    },
    call_hostHovered = function() {
      private$call_method("hostHovered")
    },
    call_hostUnhovered = function() {
      private$call_method("hostUnhovered")
    },
    call_hover = function(data) {
      private$call_method("hover", list(data))
    },
    call_unHover = function() {
      private$call_method("unHover")
    }
  )

)
