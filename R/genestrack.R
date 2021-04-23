#' @export
html_dependency_genestrack <- function() {
  list(
    htmltools::htmlDependency(
      name = "jquery-ui",
      version = "1.12.1",
      src = c(href = "//code.jquery.com/ui/1.12.1"),
      stylesheet = "themes/base/jquery-ui.css",
      script = "jquery-ui.js"
    ),
    htmltools::htmlDependency(
      name = "webcomponents-lite",
      version = "1.0.0",
      src = "wc/epiviz/lib",
      package = "inputknob",
      script = "webcomponents-lite.js"
    )
    # https://github.com/rstudio/htmltools/issues/210
    # ,htmltools::htmlDependency(
    #   name = "epiviz-components",
    #   version = "1.0.0",
    #   src = "wc/epiviz/lib",
    #   package = "inputknob",
    #   stylesheet = list(href = "epiviz-components.html", rel = "import")
    # )
    ,htmltools::htmlDependency(
      name = "epiviz-genes-track-bindings",
      version = "1.0.0",
      src = "wc/epiviz",
      package = "inputknob",
      script = "epiviz-genes-track-binding.js"
    )
  )
}


#' @export
epivizGenesTrackUI <- function(
  id = NULL,
  json_data = NULL,
  chart_colors = NULL,
  ...
) {

  params <- eval(substitute(alist(...)))
  if (length(params) > 0) {
    if (is.null(names(params)) || any(names(params) == "")) {
      stop("genesTrackUI: additional parameters must be named attributes")
    }
  }

  if (is.null(id)) {
    id <- paste0('epiviz-genes-track-', sample(1e9, 1))
  }
addResourcePath("epiviz", system.file("wc", "epiviz", "lib", package = "inputknob"))
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
    )
    , htmltools::tags$script(paste0("shinywcEpivizGenesTrack.checkInit('", id, "')"))
    , htmltools::tags$head(
      # https://github.com/rstudio/htmltools/issues/210
      htmltools::tags$link(rel = "import", href = "epiviz/epiviz-components.html")
    )
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
      private$.session$sendCustomMessage('epiviz-genes-track-attr-set', list(
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
      private$.session$sendCustomMessage('epiviz-genes-track-prop-set', list(
        id = private$.id,
        prop = prop,
        value = value
      ))
    },

    get_prop = function(prop, cb) {
      cbid_noNS <- paste0("__epiviz-genes-track-", prop, "-", sample(1e9, 1))
      cbid <- private$.session$ns(cbid_noNS)
      private$.session$sendCustomMessage('epiviz-genes-track-prop-get', list(
        id = private$.id,
        prop = prop,
        cbid = cbid
      ))
      shiny::observeEvent(private$.session$input[[cbid_noNS]], once = TRUE, {
        cb(private$.session$input[[cbid_noNS]])
      })
    },

    call_method = function(method, args = list()) {
      private$.session$sendCustomMessage('epiviz-genes-track-call', list(
        id = private$.id,
        method = method,
        args = args
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

      private$.session$sendCustomMessage('epiviz-genes-track-init', list(
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
      private$.session$input[[paste0(private$.id_noNS, "_evt_dimChanged")]]
    },
    event_hover = function() {
      private$.session$input[[paste0(private$.id_noNS, "_evt_hover")]]
    },
    event_unHover = function() {
      private$.session$input[[paste0(private$.id_noNS, "_evt_unHover")]]
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
    call_hover = function(args) {
      private$call_method("hover", args)
    },
    call_unHover = function() {
      private$call_method("unHover")
    }
  )

)
