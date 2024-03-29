% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sumQuadGuilds.R
\name{sumQuadGuilds}
\alias{sumQuadGuilds}
\title{sumQuadGuilds: summarizes quadrat species data by guilds}
\usage{
sumQuadGuilds(
  park = "all",
  from = 2006,
  to = as.numeric(format(Sys.Date(), "\%Y")),
  QAQC = FALSE,
  panels = 1:4,
  locType = c("VS", "all"),
  speciesType = c("all", "native", "exotic", "invasive"),
  splitHerb = TRUE,
  ...
)
}
\arguments{
\item{park}{Combine data from all parks or one or more parks at a time. Valid inputs:
\describe{
\item{"all"}{Includes all parks in the network}
\item{"ACAD"}{Acadia NP only}
\item{"MABI"}{Marsh-Billings-Rockefeller NHP only}
\item{"MIMA"}{Minute Man NHP only}
\item{"MORR"}{Morristown NHP only}
\item{"ROVA"}{Roosevelt-Vanderbilt NHS only}
\item{"SAGA"}{Saint-Gaudens NHS only}
\item{"SARA"}{Saratoga NHP only}
\item{"WEFA"}{Weir Farm NHS only}}}

\item{from}{Year to start analysis, ranging from 2006 to current year}

\item{to}{Year to stop analysis, ranging from 2006 to current year}

\item{QAQC}{Allows you to remove or include QAQC events.
\describe{
  \item{FALSE}{Default. Only returns visits that are not QAQC visits}
  \item{TRUE}{Returns all visits, including QAQC visits}
}}

\item{panels}{Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
If more than one panel is selected, specify by c(1, 3), for example.}

\item{locType}{Allows you to only include plots that are part of the GRTS sample design or
include all plots, such as deer exclosures.
\describe{
\item{"VS"}{Only include plots that are part of the Vital Signs GRTS sample design}
\item{"all"}{Include all plots, such as plots in deer exclosures or test plots.}}}

\item{speciesType}{Allows you to filter on native, exotic or include all species.
\describe{
\item{"all"}{Default. Returns all species.}
\item{"native"}{Returns native species only}
\item{"exotic"}{Returns exotic species only}
\item{"invasive"}{Returns species on the Indicator Invasive List}
}}

\item{splitHerb}{TRUE/FALSE. If TRUE (Default), splits the herbaceous group into forb and fern. If FALSE,
then resulting data frame will be summarized for tree, shrub, herbaceous, and graminoid guilds.}

\item{...}{Other arguments passed to function.}
}
\value{
Returns a data frame with average quadrat cover, percent quadrat frequency and quadrat
frequency count for tree, shrub/vine, herbaceous, and graminoid for each plot visit. Data are either
summarized for all species, native only, exotic only, or invasive only.
}
\description{
This function summarizes output from joinQuadData and calculates average cover and quadrat frequency for each guild.
Average cover is corrected for number of quadrats sampled. Guilds are tree, shrub, forb, fern, and graminoid. If herbaceous guild
is split, then cover of ferns does not overlap with cover of herbaceous. If herbaceous guild is not split, then cover of herbaceous
guild includes fern and other herbaceous (but not graminoid) species cover. Only works for complete events, but does include plots
where a few quadrats were not sampled. Germinants are not included in summary.
}
\examples{
\dontrun{
importData()

# compile invasive quad data for all parks and most recent survey. Keep ferns in with herbs
inv_guilds <- sumQuadGuilds(speciesType = 'invasive', from = 2015, to = 2018, splitHerb = FALSE)

# compile native quad data for more recent survey in ACAD, with ferns and forbs split in separate guilds
ACAD_guilds <- sumQuadGuilds(speciesType = 'native', from = 2015, to = 2018, splitHerb = TRUE)
}

}
