#' @export
html_dependency_webcomponentsjs <- function() {
  htmltools::htmlDependency(
    name = "webcomponentsjs",
    version = "2.5.0",
    src = "https://unpkg.com/@webcomponents/webcomponentsjs@2.5.0/webcomponents-bundle.js"
  )
}
