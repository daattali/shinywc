clean_name <- function(x) {
  gsub("-", "_", x)
}

clean_slot_name <- function(x) {
  x <- clean_name(x)
  sprintf("slot_%s", x)
}

clean_style_name <- function(x) {
  x <- gsub("^--", "", x)
  x <- clean_name(x)
  sprintf("css_%s", x)
}

is_shinywc <- function(x) {
  inherits(x, "shiny.tag.list") && !is.null(htmltools::tagGetAttribute(x[[1]], "data-shinywc-component"))
}

is_shiny_tag <- function(x) {
  inherits(x, "shiny.tag") || is_shinywc(x)
}
