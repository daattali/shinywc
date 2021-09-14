clean_name <- function(x) {
  gsub("-", "_", x)
}

clean_package_name <- function(x){
  gsub("-", ".", x)
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
  inherits(x, "shiny.tag.list") &&
    inherits(x[[1]], "shiny.tag.list") &&
    !is.null(htmltools::tagGetAttribute(x[[1]][[1]], "data-shinywc-component"))
}

is_shiny_tag <- function(x) {
  inherits(x, "shiny.tag") || is_shinywc(x)
}

# Given a directory that contains an R package with a {shinywc} dependency,
# find the version of {shinywc}. If the given directory is not an R package
# that uses {shinywc}, return FALSE
get_shinywc_dep_major_version <- function(path = getwd()) {
  res <- tryCatch({
    desc <- suppressWarnings(read.dcf(file.path(path, "DESCRIPTION")))
    pkgs <- trimws(strsplit(as.character(desc[1, "Imports"]), ",")[[1]])
    shinywc_pkg <- grep("^shinywc", pkgs, value = TRUE)
    major <- get_package_major_version(shinywc_pkg)
    major
  }, error = function(e) {
    FALSE
  })
  res
}

#' Given a directory that contains an R package, extract the package name
get_package_name <- function(path = getwd()) {
  res <- tryCatch({
    desc <- suppressWarnings(read.dcf(file.path(path, "DESCRIPTION")))
    desc[1, "Package"][[1]]
  }, error = function(e) {
    FALSE
  })
  res
}

# Extract the major version number from a package version string
get_package_major_version <- function(s) {
  versions <- gregexpr("([0-9]+\\.)", s)
  if (versions[[1]][1] == -1) {
    stop("Could not parse version number from ", s, call. = FALSE)
  }
  major <- substring(s, versions[[1]][1], versions[[1]][1] + attr(versions[[1]], "match.length")[1] - 2)
  major
}
