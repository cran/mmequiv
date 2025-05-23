% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{mmequiv_after}
\alias{mmequiv_after}
\title{Get time remaining from Retry-After header}
\usage{
mmequiv_after(resp)
}
\arguments{
\item{resp}{A httr2 response object.}
}
\value{
A numeric value indicating the number of seconds remaining. May error if
the response doesn't contain a valid Retry-After header.
}
\description{
Get time remaining from Retry-After header
}
\keyword{internal}
