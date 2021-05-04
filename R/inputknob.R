#' #' @export
#' InputKnob <- R6::R6Class(
#'   "InputKnob",
#'
#'   private = list(
#'     .id = NULL,
#'     .id_noNS = NULL,
#'     .session = NULL,
#'     .attributes = list(),
#'
#'     set_attr = function(attr, value) {
#'       private$.session$sendCustomMessage('shinywc-attr-set', list(
#'         id = private$.id,
#'         attr = attr,
#'         value = value
#'       ))
#'     },
#'
#'     get_attr = function(attr) {
#'       if (!attr %in% names(private$.attributes)) {
#'         return(NULL)
#'       }
#'       private$.attributes[[attr]]
#'     },
#'
#'     set_prop = function(prop, value) {
#'       private$.session$sendCustomMessage('shinywc-prop-set', list(
#'         id = private$.id,
#'         prop = prop,
#'         value = value
#'       ))
#'     },
#'
#'     get_prop = function(prop, cb) {
#'       cbid_noNS <- paste0("__input-knob-", prop, "-", sample(1e9, 1))
#'       cbid <- private$.session$ns(cbid_noNS)
#'       private$.session$sendCustomMessage('shinywc-prop-get', list(
#'         id = private$.id,
#'         prop = prop,
#'         cbid = cbid
#'       ))
#'       shiny::observeEvent(private$.session$input[[cbid_noNS]], once = TRUE, {
#'         cb(private$.session$input[[cbid_noNS]])
#'       })
#'     },
#'
#'     call_method = function(method, params = list()) {
#'       private$.session$sendCustomMessage('shinywc-call-method', list(
#'         id = private$.id,
#'         method = method,
#'         params = params
#'       ))
#'     }
#'   ),
#'
#'   public = list(
#'
#'     initialize = function(id, session = shiny::getDefaultReactiveDomain()) {
#'       if (is.null(session)) {
#'         stop("InputKnob can only be initialized in a Shiny environment")
#'       }
#'       private$.session <- session
#'       private$.id_noNS <- id
#'       private$.id <- private$.session$ns(private$.id_noNS)
#'
#'       private$.session$sendCustomMessage('shinywc-init-component', list(
#'         id = private$.id
#'       ))
#'
#'       shiny::observeEvent(private$.session$input[[paste0(private$.id_noNS, "_input-knob-attr-change")]], {
#'         evt <- private$.session$input[[paste0(private$.id_noNS, "_input-knob-attr-change")]]
#'         attr_name <- names(evt[1])
#'         attr_val <- evt[[1]]
#'         private$.attributes[[attr_name]] <- attr_val
#'       })
#'     },
#'
#'     id = function() {
#'       private$.id_noNS
#'     },
#'
#'     event_knob_move_change = function() {
#'       private$.session$input[[paste0(private$.id_noNS, "_event_knob-move-change")]]
#'     },
#'     event_knob_move_start = function() {
#'       private$.session$input[[paste0(private$.id_noNS, "_event_knob-move-start")]]
#'     },
#'     event_knob_move_end = function() {
#'       private$.session$input[[paste0(private$.id_noNS, "_event_knob-move-end")]]
#'     },
#'
#'     get_value = function() {
#'       private$get_attr("value")
#'     },
#'     set_value = function(value) {
#'       private$set_attr("value", value)
#'     },
#'     get_scale = function() {
#'       private$get_attr("scale")
#'     },
#'     set_scale = function(value) {
#'       private$set_attr("scale", value)
#'     },
#'     get_min = function() {
#'       private$get_attr("min")
#'     },
#'     set_min = function(value) {
#'       private$set_attr("min", value)
#'     },
#'     get_max = function() {
#'       private$get_attr("max")
#'     },
#'     set_max = function(value) {
#'       private$set_attr("max", value)
#'     },
#'
#'     get_value_prop = function(cb) {
#'       private$get_prop("value", cb)
#'     },
#'     set_value_prop = function(value) {
#'       private$set_prop("value", value)
#'     },
#'     get_scale_prop = function(cb) {
#'       private$get_prop("scale", cb)
#'     },
#'     set_scale_prop = function(value) {
#'       private$set_prop("scale", value)
#'     },
#'     get_min_prop = function(cb) {
#'       private$get_prop("min", cb)
#'     },
#'     set_min_prop = function(value) {
#'       private$set_prop("min", value)
#'     },
#'     get_max_prop = function(cb) {
#'       private$get_prop("max", cb)
#'     },
#'     set_max_prop = function(value) {
#'       private$set_prop("max", value)
#'     },
#'
#'     call_rotateLeft = function(turns) {
#'       params <- list(turns)
#'       private$call_method("rotateLeft", params)
#'     },
#'     call_rotateRight = function(turns) {
#'       params <- list(turns)
#'       private$call_method("rotateRight", params)
#'     }
#'
#'   )
#'
#' )
