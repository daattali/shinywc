#' @param force If `TRUE`, force a new component to be created even if an older
#' version already exists. If `FALSE` (default), always ask the user what to do
#' in such a case.
#' @export
create_shinywc <- function(path, schema, source, webcomponentsjs = FALSE, jqueryui = FALSE, force = FALSE, open = TRUE) {
  create_shinywc_helper(
    path = path,
    schema = schema,
    source = source,
    webcomponentsjs = webcomponentsjs,
    jqueryui = jqueryui,
    force = force,
    open = open
  )
}

create_shinywc_gui <- function(path, ...) {
  dots <- list(...)

  create_shinywc_helper(
    path = path,
    schema_file = dots$schema,
    source_file = dots$source,
    webcomponentsjs = FALSE,
    jqueryui = FALSE,
    force = FALSE,
    open = TRUE
  )
}

create_shinywc_helper <- function(path, schema_file, source_file, webcomponentsjs, jqueryui, force, open) {
  checkmate::assert_string(path, min.chars = 1)
  checkmate::assert_string(schema_file, min.chars = 1)
  checkmate::assert_string(source_file, min.chars = 1)
  checkmate::assert_logical(force, len = 1, any.missing = FALSE)
  checkmate::assert_logical(open, len = 1, any.missing = FALSE)

  path <- normalizePath(path, mustWork = FALSE)
  path <- gsub("[\\/]$", "", path)
  schema_file <- normalizePath(schema_file, mustWork = FALSE)
  source_file <- normalizePath(source_file, mustWork = FALSE)
  wc_dir <- file.path(path, "inst", "webcomponent")

  preexisting <- FALSE
  if (dir.exists(path)) {
    preexisting <- TRUE

    old_version <- get_shinywc_dep_major_version(path)
    if (isFALSE(old_version)) {
      stop("The path '", path, "' already exists but is not a shinywc R package", call. = FALSE)
    }

    cur_version <- get_package_major_version(as.character(utils::packageVersion("shinywc")))
    if (old_version != cur_version) {
      stop(
        sprintf("The current version of {shinywc} (%s.X.X) is different than the version (%s.X.X) used by '%s'.\nSince {shinywc} components may not work across different versions, please modify the DESCRIPTION file to the current version and test the existing component. If it still works, then re-run this function.", cur_version, old_version, path),
        call. = FALSE
      )
    }

    schema <- parse_schema(schema_file)
    if (file.exists(file.path(path, "R", paste0(schema$shinywc$clean, ".R")))) {
      if (!force) {
        res <- 2
        res <- utils::menu(choices = c("Yes", "No"), title = paste0("Component <", schema$name, "> already exists. Overwrite?"))
        if (res != 1) {
          message("Aborted")
          return(invisible())
        }
      }
    }
  } else {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(path, recursive = TRUE, force = TRUE))
    dir.create(wc_dir, showWarnings = FALSE, recursive = TRUE)
  }

  schema <- create_schema_file(schema_file, wc_dir, source_file, webcomponentsjs, jqueryui)

  if (preexisting) {
    schema$shinywc$pkgname <- get_package_name(path)
  }

  create_description_file(schema, path)
  create_readme_file(schema, path)

  if (!file.exists(source_file)) {
    stop("Could not find source file", call. = FALSE)
  }
  file.copy(from = source_file, to = wc_dir, overwrite = TRUE)

  create_ui_function_file(schema, file.path(path, "R"))
  create_proxy_function_file(schema, file.path(path, "R"))

  if (open && rstudioapi::isAvailable()) {
    rstudioapi::openProject(path, newSession = TRUE)
  }

  on.exit() # everything was successful, so remove the cleanup callback
  message("Created component <", schema$name, "> at '", path, "'")
  message("Remember to document (`devtools::document()`) and build (`devtools::build()`) the new package before using it")
  invisible(path)
}

create_schema_file <- function(schema_file, path, source_file, webcomponentsjs, jqueryui) {
  file.copy(from = schema_file, to = path, overwrite = TRUE)
  create_schema(schema_file, source_file, webcomponentsjs, jqueryui)
}

create_schema <- function(schema_file, source_file, webcomponentsjs, jqueryui) {
  schema <- parse_schema(schema_file)
  schema$shinywc$source_file <- basename(source_file)
  schema$shinywc$version <- as.character(packageVersion("shinywc"))
  if (fs::path_ext(source_file) == "js") {
    schema$shinywc$source_type_js <- TRUE
  } else if (fs::path_ext(source_file) == "html") {
    schema$shinywc$source_type_html <- TRUE
  } else {
    stop("Source file must have either `.js` extension (for JavaScript modules) or `.html` extension (for HTML imports)", call. = FALSE)
  }

  if (!isFALSE(webcomponentsjs)) {
    schema$shinywc$webcomponentsjs <- TRUE
    if (is.character(webcomponentsjs)) {
      schema$shinywc$webcomponentsjs_version <- webcomponentsjs
    }
  }
  if (!isFALSE(jqueryui)) {
    schema$shinywc$jqueryui <- TRUE
    if (is.character(jqueryui)) {
      schema$shinywc$jqueryui_version <- jqueryui
    }
  }
  schema
}

parse_schema <- function(schema_file) {
  tryCatch({
    if (!file.exists(schema_file)) {
      stop("Could not find schema file", call. = FALSE)
    }
    if (fs::path_ext(schema_file) != "json") {
      stop("Schema file must have `.json` extension", call. = FALSE)
    }

    schema <- readLines(paste(schema_file, collapse = " "))
    schema <- jsonlite::fromJSON(schema, simplifyVector = FALSE)

    checkmate::assert_atomic(schema$version, len = 1, any.missing = FALSE)
    checkmate::assert_list(schema$tags)

    schema <- schema$tags[[1]]
    checkmate::assert_string(schema$name, min.chars = 1)
    checkmate::assert_atomic(schema$version, len = 1, any.missing = FALSE)

    schema$shinywc <- list()
    schema$shinywc$pkgname <- clean_package_name(schema$name)
    schema$shinywc$clean <- clean_name(schema$name)

    if (is.null(schema$attributes)) {
      schema$attributes <- list()
    } else {
      checkmate::assert_list(schema$attributes, types = "list", any.missing = FALSE, unique = TRUE)
      for (idx in seq_along(schema$attributes)) {
        attribute <- schema$attributes[[idx]]
        checkmate::assert_string(attribute$name, min.chars = 1)
        schema$attributes[[idx]]$clean <- clean_name(attribute$name)
        if (!is.null(attribute$required)) {
          checkmate::assert_logical(attribute$required, len = 1, any.missing = FALSE)
        }
      }
    }

    if (is.null(schema$properties)) {
      schema$properties <- list()
    } else {
      checkmate::assert_list(schema$properties, types = "list", any.missing = FALSE, unique = TRUE)
      for (idx in seq_along(schema$properties)) {
        property <- schema$properties[[idx]]
        checkmate::assert_string(property$name, min.chars = 1)
        schema$properties[[idx]]$clean <- clean_name(property$name)
      }
    }

    if (is.null(schema$events)) {
      schema$events <- list()
    } else {
      checkmate::assert_list(schema$events, types = "list", any.missing = FALSE, unique = TRUE)
      for (idx in seq_along(schema$events)) {
        event <- schema$events[[idx]]
        checkmate::assert_string(event$name, min.chars = 1)
        schema$events[[idx]]$clean <- clean_name(event$name)
      }
    }

    if (is.null(schema$slots)) {
      schema$slots <- list()
    } else {
      checkmate::assert_list(schema$slots, types = "list", any.missing = FALSE, unique = TRUE)
      for (idx in seq_along(schema$slots)) {
        slot <- schema$slots[[idx]]
        checkmate::assert_string(slot$name, min.chars = 1)
        schema$slots[[idx]]$clean <- clean_slot_name(slot$name)
      }
    }

    if (is.null(schema$cssProperties)) {
      schema$cssProperties <- list()
    } else {
      checkmate::assert_list(schema$cssProperties, types = "list", any.missing = FALSE, unique = TRUE)
      for (idx in seq_along(schema$cssProperties)) {
        css <- schema$cssProperties[[idx]]
        checkmate::assert_string(css$name, min.chars = 1)
        schema$cssProperties[[idx]]$clean <- clean_style_name(css$name)
      }
    }

    if (is.null(schema$methods)) {
      schema$methods <- list()
    } else {
      checkmate::assert_list(schema$methods, types = "list", any.missing = FALSE, unique = TRUE)
      for (idx in seq_along(schema$methods)) {
        method <- schema$methods[[idx]]
        checkmate::assert_string(method$name, min.chars = 1)
        schema$methods[[idx]]$clean <- clean_name(method$name)
        if (!is.null(method$params)) {
          checkmate::assert_list(method$params, types = "character", any.missing = FALSE, unique = TRUE)
        }
      }
    }

    schema
  }, error = function(err) {
    stop("Problem with schema file (", err$message, ")", call. = FALSE)
  })
}

create_description_file <- function(schema, path) {
  create_template_file(schema, path, "DESCRIPTION", overwrite = FALSE)
}

create_readme_file <- function(schema, path) {
  create_template_file(schema, path, "README.md", overwrite = FALSE)
}

create_ui_function_file <- function(schema, path) {
  create_template_file(schema, path, "component_ui.mustache", paste0(schema$shinywc$clean, ".R"))
}

create_proxy_function_file <- function(schema, path) {
  create_template_file(schema, path, "component_proxy.mustache", paste0(schema$shinywc$clean, "_proxy.R"))
}

create_template_file <- function(schema, basepath, template_file, out_file = template_file, overwrite = TRUE) {
  out_file <- normalizePath(file.path(basepath, out_file), mustWork = FALSE)
  if (!overwrite && file.exists(out_file)) {
    return()
  }

  template <- readLines(system.file("templates", template_file, package = "shinywc"))
  result <- whisker::whisker.render(template, schema)
  result <- gsub("(,\\s*)?__SHINYWC_REMOVEME__", "", result)
  result <- remove_last_comma_in_list(result)
  dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
  writeLines(result, out_file)
}

# Remove the last non-comment line that ends in a comma (to sanitize a list)
remove_last_comma_in_list <- function(str) {
  while (grepl("__SHINYWC_START_LIST__", str)) {
    list_str <- sub("(.*)__SHINYWC_START_LIST__(.*)__SHINYWC_END_LIST__(.*)", "\\2", str)
    lines <- strsplit(list_str, "\n")[[1]]
    noncomment_lines <- which(grepl(",$", trimws(lines)) & !grepl("(\\s*)#", lines))
    if (length(noncomment_lines) > 0) {
      lines[max(noncomment_lines)] <- substring(lines[max(noncomment_lines)], 1, nchar(trimws(lines[max(noncomment_lines)], "right")) - 1)
    }
    list_str <- paste(lines, collapse = "\n")
    str <- sub("(.*)__SHINYWC_START_LIST__(.*)__SHINYWC_END_LIST__(.*)", paste0("\\1", list_str, "\\3"), str)
  }
  str
}
