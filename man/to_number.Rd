% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/util.R
\name{to_number}
\alias{to_number}
\title{Turn strings into numbers. Also handles percentages.}
\usage{
to_number(x)
}
\arguments{
\item{x}{The string for converting}
}
\value{
The converted number or NA if it's not convertible.
}
\description{
Turn strings into numbers. Also handles percentages.
}
\examples{
to_number("9876")
to_number("19\%")
to_number("$12,345.50")
}

