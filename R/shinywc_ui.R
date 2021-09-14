#' Create a Web Component HTML tag to use in a Shiny app's UI
#'
#' It's recommended not to call this function from code you write yourself.
#' This function should be called from code generated automatically using
#' [`create_shinywc`].
#'
#' @param tag The tag name of the HTML tag.
#' @param params A list containing the component's parameters.
#' @param params_extra A list containing HTML attributes to add to the tag.
#' @param attributes A list of attributes the component supports.
#' @param required A list of required attributes.
#' @param events A list of events that the component fires.
#' @param slots A list of slot names the component supports.
#' @param styles A list of CSS styles the component uses.
#' @param dependencies A list of HTML dependencies.
#' @return A Shiny UI tag.
#' @export
shinywc_ui <- function(tag, params, params_extra = list(),
                       attributes = list(), required = list(),
                       events = list(), slots = list(), styles = list(),
                       dependencies = list()) {

  for (param in required) {
    if (is.null(params[[clean_name(param)]])) {
      stop(tag, ": Parameter `", clean_name(param), "` is required", call. = FALSE)
    }
  }

  if (length(params_extra) > 0) {
    if (is.null(names(params_extra)) || any(names(params_extra) == "")) {
      stop(tag, ": Additional parameters must be named attributes", call. = FALSE)
    }
  }

  id <- params[["id"]]
  if (is.null(id)) {
    id <- sprintf('%s-%s', tag, sample(1e9, 1))
  }

  slots_tags <- list()
  if (length(slots) > 0) {
    for (slot in slots) {
      slot_clean <- clean_slot_name(slot)
      if (!is.null(params[[slot_clean]])) {
        if (is_shiny_tag(params[[slot_clean]])) {
          params[[slot_clean]] <- htmltools::tagList(params[[slot_clean]])
        }
        if (!inherits(params[[slot_clean]], "shiny.tag.list") && !inherits(params[[slot_clean]], "list")) {
          stop(tag, ": Slot ", slot_clean, " must be a valid HTML tag, tagList, or list of tags", call. = FALSE)
        }

        for (slottag in params[[slot_clean]]) {
          if (is_shinywc(slottag)) {
            slottag[[1]][[1]] <- shiny::tagAppendAttributes(slottag[[1]][[1]], slot = slot)
          } else if (is_shiny_tag(slottag)) {
            slottag <- shiny::tagAppendAttributes(slottag, slot = slot)
          } else {
            stop(tag, ": Slot ", slot_clean, " must be a tagList or list containing HTML tags", call. = FALSE)
          }
          slots_tags <- c(
            slots_tags,
            list(slottag)
          )
        }
      }
    }
  }

  css <- NULL
  if (length(styles) > 0) {
    for (style in styles) {
      style_clean <- clean_style_name(style)
      if (!is.null(params[[style_clean]])) {
        css <- c(css, sprintf("%s:%s", style, params[[style_clean]]))
      }
    }
    if (length(css) > 0) {
      css <- paste(css, collapse = "; ")
    }
  }

  component_details <- list(
    name = tag,
    attributes = attributes,
    events = events
  )
  component_details <- jsonlite::toJSON(component_details, auto_unbox = TRUE)

  tag_params <- list(
    id = id,
    "data-shinywc-component" = "1"
  )
  for (attribute in attributes) {
    tag_params[[attribute]] <- params[[clean_name(attribute)]]
  }
  if (!is.null(params[["slot"]])) {
    tag_params <- c(tag_params, list(params[["slot"]]))
  }
  if (length(slots_tags) > 0) {
    tag_params <- c(tag_params, slots_tags)
  }
  if (!is.null(css)) {
    tag_params[["style"]] <- css
  }
  if (length(params_extra) > 0) {
    tag_params <- c(tag_params, params_extra)
  }

  component_tag <- htmltools::tag(
    tag,
    .noWS = c("after-begin", "before-end"),
    varArgs = tag_params
  )

  full_html <- htmltools::tagList(
    component_tag,
    htmltools::singleton(htmltools::tags$head(
      htmltools::tags$script(sprintf("shinywc.setupComponent(%s)", component_details))
    )),
    htmltools::tags$script(sprintf("shinywc.registerComponent('%s', '%s')", tag, id))
  )
  full_html <- htmltools::attachDependencies(
    full_html,
    dependency_shinywc()
  )
  if (length(dependencies) > 0) {
    full_html <- htmltools::attachDependencies(
      full_html,
      dependencies,
      append = TRUE
    )
  }
  full_html
}
