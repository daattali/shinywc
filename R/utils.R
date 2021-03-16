POSSIBLE_ATTRS <- c("value", "scale", "max", "min")
POSSIBLE_METHODS <- c("rotateLeft", "rotateRight")

deps <- list(
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
