% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importCSV.R
\name{importCSV}
\alias{importCSV}
\title{importCSV: Import NETN forest data that are formatted as .csv files.}
\usage{
importCSV(path = NA, new_env = TRUE, zip_name = NA)
}
\arguments{
\item{path}{Quoted path of folder containing tables.}

\item{new_env}{Logical. Specifies which environment to store views in. If \code{TRUE}(Default), stores
views in VIEWS_NETN environment. If \code{FALSE}, stores views in global environment}

\item{zip_name}{Quoted string ending in .zip. If specified, function looks for the specified file name and
imports .csvs from the zip file. If not specified, function looks for and imports individual csvs. Note that
this takes slightly longer than loading individual .csvs, due to the unzipping process.}
}
\value{
NETN database views in specified environment
}
\description{
This function imports all views in the ANALYSIS schema of the NETN_Forest backend that have been
previously exported as .csvs or a zip file using the exportCSV() function. Each view is added to a VIEWS_NETN
environment in your workspace, or to your global environment based on whether new_env = TRUE or FALSE.
}
\examples{
\dontrun{
# Import individual csvs into global environment
importCSV(path = "C:/Forest_Health/exports/NETN", new_env = FALSE)

# Import zipped csvs into VIEWS_NETN environment
path <- "C:/Forest_Health/exports/NETN"
importCSV(path = path, zip_name = "NETN_Forest_20210406.zip")
}

}
