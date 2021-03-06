% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/schema.R
\name{schema}
\alias{schema}
\alias{props}
\alias{props2}
\alias{reqs}
\alias{enums}
\alias{types}
\alias{get_name_from_ref}
\title{schema}
\usage{
props(schema, type)

props2(schema, type)

reqs(schema, type)

enums(schema, type)

types(schema, type)

get_name_from_ref(type)
}
\arguments{
\item{schema}{imported json schema}

\item{type}{schema reference}
}
\value{
list
}
\description{
schema utility functions
}
\examples{

schema_file <- Sys.glob(file.path(system.file("schema/vega-lite", package = "vegawidget"),"*.json"))
VL_SCHEMA <- jsonlite::read_json(schema_file)
encoding_options <- props(VL_SCHEMA, list("$ref" = "#/definitions/Encoding"))
}
