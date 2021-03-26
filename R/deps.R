#' @export
inputknob_dependencies <- function() {
  list(
    htmltools::htmlDependency(
      name = "input-knob",
      version = "1.0.0",
      src = "wc/inputknob/lib",
      package = "inputknob",
      script = list(src = "input-knob.js", type = "module")
    ),
    htmltools::htmlDependency(
      name = "input-knob-bindings",
      version = "1.0.0",
      src = "wc/inputknob",
      package = "inputknob",
      script = "input-knob-binding.js"
    )
  )
}
