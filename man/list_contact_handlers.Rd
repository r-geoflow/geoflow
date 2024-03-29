% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geoflow_handler_contact.R
\name{list_contact_handlers}
\alias{list_contact_handlers}
\title{list_contact_handlers}
\usage{
list_contact_handlers(raw)
}
\arguments{
\item{raw}{Default value is \code{FALSE)}, meaning the handlers will be listed as
\code{data.frame}. The output If \code{TRUE} the raw list of \link{geoflow_handler} 
is returned.}
}
\value{
an object of class \code{data.frame} (or \code{list} of \link{geoflow_handler} if raw = FALSE)
}
\description{
\code{list_contact_handlers} lists the contact handlers supported by geoflow.
}
\author{
Emmanuel Blondel, \email{emmanuel.blondel1@gmail.com}
}
