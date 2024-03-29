% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geoflow_dictionary.R
\docType{class}
\name{geoflow_dictionary}
\alias{geoflow_dictionary}
\title{Geoflow dictionary class}
\format{
\code{\link{R6Class}} object.
}
\value{
Object of \code{\link{R6Class}} for modelling a dictionary
}
\description{
This class models a dictionary to be executed by geoflow
}
\details{
geoflow_dictionary
}
\author{
Emmanuel Blondel <emmanuel.blondel1@gmail.com>
}
\keyword{contact}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{source}}{dictionary source, object of class \code{data.frame}}

\item{\code{featuretypes}}{list of objects of class \code{geoflow_featuretype}}

\item{\code{registers}}{list of objects of class \code{geoflow_register}}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-geoflow_dictionary-new}{\code{geoflow_dictionary$new()}}
\item \href{#method-geoflow_dictionary-setSource}{\code{geoflow_dictionary$setSource()}}
\item \href{#method-geoflow_dictionary-getFeatureTypes}{\code{geoflow_dictionary$getFeatureTypes()}}
\item \href{#method-geoflow_dictionary-getFeatureTypeById}{\code{geoflow_dictionary$getFeatureTypeById()}}
\item \href{#method-geoflow_dictionary-addFeatureType}{\code{geoflow_dictionary$addFeatureType()}}
\item \href{#method-geoflow_dictionary-getRegisters}{\code{geoflow_dictionary$getRegisters()}}
\item \href{#method-geoflow_dictionary-getRegisterById}{\code{geoflow_dictionary$getRegisterById()}}
\item \href{#method-geoflow_dictionary-addRegister}{\code{geoflow_dictionary$addRegister()}}
\item \href{#method-geoflow_dictionary-clone}{\code{geoflow_dictionary$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-geoflow_dictionary-new"></a>}}
\if{latex}{\out{\hypertarget{method-geoflow_dictionary-new}{}}}
\subsection{Method \code{new()}}{
Initializes a \link{geoflow_dictionary} object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{geoflow_dictionary$new()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-geoflow_dictionary-setSource"></a>}}
\if{latex}{\out{\hypertarget{method-geoflow_dictionary-setSource}{}}}
\subsection{Method \code{setSource()}}{
Sets dictionnary source
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{geoflow_dictionary$setSource(source)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{source}}{object of class \code{data.frame}}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-geoflow_dictionary-getFeatureTypes"></a>}}
\if{latex}{\out{\hypertarget{method-geoflow_dictionary-getFeatureTypes}{}}}
\subsection{Method \code{getFeatureTypes()}}{
Get the list of \link{geoflow_featuretype} defined in the dictionary
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{geoflow_dictionary$getFeatureTypes()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
a \code{list} of \code{geoflow_featuretype}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-geoflow_dictionary-getFeatureTypeById"></a>}}
\if{latex}{\out{\hypertarget{method-geoflow_dictionary-getFeatureTypeById}{}}}
\subsection{Method \code{getFeatureTypeById()}}{
Get an object of class \link{geoflow_featuretype} given an ID
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{geoflow_dictionary$getFeatureTypeById(id)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{id}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
an object of class \link{geoflow_featuretype}, otherwise \code{NULL}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-geoflow_dictionary-addFeatureType"></a>}}
\if{latex}{\out{\hypertarget{method-geoflow_dictionary-addFeatureType}{}}}
\subsection{Method \code{addFeatureType()}}{
Adds a feature type to the dictionnary
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{geoflow_dictionary$addFeatureType(ft)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ft}}{object of class \link{geoflow_featuretype}}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-geoflow_dictionary-getRegisters"></a>}}
\if{latex}{\out{\hypertarget{method-geoflow_dictionary-getRegisters}{}}}
\subsection{Method \code{getRegisters()}}{
Get the list of registers associated with the dictionnary
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{geoflow_dictionary$getRegisters()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
a list of \link{geoflow_register} objects
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-geoflow_dictionary-getRegisterById"></a>}}
\if{latex}{\out{\hypertarget{method-geoflow_dictionary-getRegisterById}{}}}
\subsection{Method \code{getRegisterById()}}{
Get register by ID
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{geoflow_dictionary$getRegisterById(id)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{id}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
an object of class \link{geoflow_register}, otherwise \code{NULL}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-geoflow_dictionary-addRegister"></a>}}
\if{latex}{\out{\hypertarget{method-geoflow_dictionary-addRegister}{}}}
\subsection{Method \code{addRegister()}}{
Adds a register to the dictionnary
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{geoflow_dictionary$addRegister(register)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{register}}{object of class \link{geoflow_register}}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-geoflow_dictionary-clone"></a>}}
\if{latex}{\out{\hypertarget{method-geoflow_dictionary-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{geoflow_dictionary$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
