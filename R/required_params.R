required_params <- function(...) {
  params <- eval(substitute(alist(...)))
  env <- parent.frame()
  for (param in params) {
    param_name <- deparse(param)
    if (is.null(get(param_name, env))) {
      stop("Parameter `", param_name, "` is required", call. = FALSE)
    }
  }
}

