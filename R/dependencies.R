#' Add a dependency on jQuery UI
#'
#' Add this to the list of dependencies passed to [`shinywc_ui`].
#' @param version The version of jQuery UI to use
#' @export
dependency_jqueryui <- function(version = "1.12.1") {
  htmltools::htmlDependency(
    name = "jquery-ui",
    version = version,
    src = c(href = sprintf("//code.jquery.com/ui/%s", version)),
    stylesheet = "themes/base/jquery-ui.css",
    script = "jquery-ui.js"
  )
}

#' Add a dependency on webcomponentsjs polyfill
#'
#' Add this to the list of dependencies passed to [`shinywc_ui`].
#' @param version The version of webcomponentsjs polyfill to use
#' @export
dependency_webcomponentsjs <- function(version = "2.5.0") {
  if (startsWith(version, "0.") || startsWith(version, "1.")) {
    filename <- "webcomponents-lite"
  } else {
    filename <- "webcomponents-bundle"
  }
  htmltools::htmlDependency(
    name = "webcomponentsjs",
    version = version,
    src = c(href = sprintf("https://unpkg.com/@webcomponents/webcomponentsjs@%s", version)),
    script = sprintf("%s.js", filename)
  )
}

#' Add a dependency on the shinywc package
dependency_shinywc <- function() {
  htmltools::htmlDependency(
    name = "shinywc",
    version = as.character(utils::packageVersion("shinywc")),
    src = "srcjs",
    package = "shinywc",
    script = "shinywc.js"
  )
}
