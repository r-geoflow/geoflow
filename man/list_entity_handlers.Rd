% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geoflow_handler_entity.R
\name{list_entity_handlers}
\alias{list_entity_handlers}
\title{list_entity_handlers}
\usage{
list_entity_handlers(raw)
}
\arguments{
\item{raw}{Default value is \code{FALSE}, meaning the handlers will be listed as
\code{data.frame}. The output If \code{TRUE} the raw list of \link{geoflow_handler} 
is returned.}
}
\value{
an object of class \code{data.frame} (or \code{list} of \link{geoflow_handler} if raw = FALSE)
}
\description{
\code{list_entity_handlers} lists the entity handlers supported by geoflow.
}
\author{
Emmanuel Blondel, \email{emmanuel.blondel1@gmail.com}
}
