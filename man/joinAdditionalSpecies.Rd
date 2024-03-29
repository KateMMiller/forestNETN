% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/joinAdditionalSpecies.R
\name{joinAdditionalSpecies}
\alias{joinAdditionalSpecies}
\title{joinAdditionalSpecies: compiles additional species data}
\usage{
joinAdditionalSpecies(
  park = "all",
  from = 2006,
  to = as.numeric(format(Sys.Date(), "\%Y")),
  QAQC = FALSE,
  panels = 1:4,
  locType = c("VS", "all"),
  eventType = c("complete", "all"),
  speciesType = c("all", "native", "exotic", "invasive")
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
\item{TRUE}{Returns all visits, including QAQC visits}}}

\item{panels}{Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
If more than one panel is selected, specify by c(1, 3), for example.}

\item{locType}{Allows you to only include plots that are part of the GRTS sample design or
include all plots, such as deer exclosures.
\describe{
\item{"VS"}{Only include plots that are part of the Vital Signs GRTS sample design}
\item{"all"}{Include all plots, such as plots in deer exclosures or test plots.}}}

\item{eventType}{Allows you to include only complete sampling events or all sampling events
\describe{
\item{"complete"}{Default. Only include sampling events for a plot that are complete.}
\item{"all}{Include all plot events with a record in tblCOMN.Event, including plots missing most of the data
associated with that event (eg ACAD-029.2010). This feature is currently hard-coded in the function.}}}

\item{speciesType}{Allows you to filter on native, exotic or include all species.
\describe{
\item{"all"}{Default. Returns all species.}
\item{"native"}{Returns native species only}
\item{"exotic"}{Returns exotic species only}
\item{"invasive"}{Returns species on the Indicator Invasive List}
}}
}
\value{
Returns a dataframe with a row for each species recorded during the timed search per visit.
}
\description{
This function compiles the additional species collected during a timed 15-minute plot search. This is
primarily an internal function used for QAQC and to generate the species lists for makeSppList().
}
\examples{
\dontrun{
importData()
# compile invasive species found in plot search in SARA for all years
SARA_quads <- joinAdditionalSpecies(park = 'SARA', speciesType = 'invasive')

# compile all species, including QAQC visits for parks in cycle 3
native_quads <- joinAdditionalSpecies(from = 2014, to = 2017, QAQC = TRUE)
}

}
