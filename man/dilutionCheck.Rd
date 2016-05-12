% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dilutionCheck.R
\name{dilutionCheck}
\alias{dilutionCheck}
\title{Evaluate a QC dilution series}
\usage{
dilutionCheck(filepath, runinfo)
}
\arguments{
\item{filepath}{the filepath containing raw (\code{.mzML, .mzXML}) files}

\item{info}{a runinfo  \code{data.frame}. The \code{class} column should denote the dilution factor}

\item{nCores}{a numeric value for the number of cores to utilise}
}
\description{
Evaluate a QC dilution series
}
\author{
Tom Wilson \email{tpw2@aber.ac.uk}
}
