#' @export
ShinywcProxy <- R6::R6Class(
  "ShinywcProxy",

  private = list(
    .tag = NULL,
    .id = NULL,
    .id_noNS = NULL,
    .session = NULL,
    .attributes = list()
  ),

  public = list(
    initialize = function(tag, id, session) {
      if (is.null(session)) {
        stop(tag, " proxy Can only be initialized in a Shiny environment", call. = FALSE)
      }
      private$.tag <- tag
      private$.session <- session
      private$.id_noNS <- id
      private$.id <- private$.session$ns(private$.id_noNS)

      private$.session$sendCustomMessage('shinywc-init-component', list(
        id = private$.id
      ))

      shiny::observeEvent(private$.session$input[[paste0(private$.id_noNS, "_", private$.tag, "-attr-change")]], {
        evt <- private$.session$input[[paste0(private$.id_noNS, "_", private$.tag, "-attr-change")]]
        attr_name <- names(evt[1])
        attr_val <- evt[[1]]
        private$.attributes[[attr_name]] <- attr_val
      })
    },

    id = function() {
      private$.id_noNS
    },

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
      cbid_noNS <- paste0("__", private$.tag, "-", prop, "-", sample(1e9, 1))
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
    },

    listen_event = function(event) {
      private$.session$input[[paste0(private$.id_noNS, "_event_", event)]]
    }
  )
)
