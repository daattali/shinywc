% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shinywc_ui.R
\name{shinywc_ui}
\alias{shinywc_ui}
\title{Create a Web Component HTML tag to use in a Shiny app's UI}
\usage{
shinywc_ui(
  tag,
  params,
  params_extra = list(),
  attributes = list(),
  required = list(),
  events = list(),
  slots = list(),
  styles = list(),
  dependencies = list()
)
}
\arguments{
\item{tag}{The tag name of the HTML tag.}

\item{params}{A list containing the component's parameters.}

\item{params_extra}{A list containing HTML attributes to add to the tag.}

\item{attributes}{A list of attributes the component supports.}

\item{required}{A list of required attributes.}

\item{events}{A list of events that the component fires.}

\item{slots}{A list of slot names the component supports.}

\item{styles}{A list of CSS styles the component uses.}

\item{dependencies}{A list of HTML dependencies.}
}
\value{
A Shiny UI tag.
}
\description{
It's recommended not to call this function from code you write yourself.
This function should be called from code generated automatically using
\code{\link{create_shinywc}}.
}
