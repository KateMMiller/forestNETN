% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/joinSoilLabData.R
\name{joinSoilLabData}
\alias{joinSoilLabData}
\title{joinSoilLabData: compile and QC soil chemistry data by horizon.}
\usage{
joinSoilLabData(
  park = "all",
  from = 2007,
  to = as.numeric(format(Sys.Date(), "\%Y")),
  QAQC = FALSE,
  panels = 1:4,
  locType = c("VS", "all"),
  layer = c("all", "O", "A")
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

\item{from}{Year to start analysis, ranging from 2007 to current year.}

\item{to}{Year to stop analysis, ranging from 2007 to current year}

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

\item{layer}{Allows you to filter on soil horizons
\describe{
\item{"all"}{Default. Includes O and A horizons.}
\item{"O"}{Return only samples from the O horizon.}
\item{"A"}{Return only samples from the A horizon.}
}}
}
\value{
returns a dataframe containing each plot and visit with soil chemistry data for each horizon on a plot
Plots that weren't sampled during a given cycle are not returned. Horizon depths are averaged across samples.
Note that horizons that were combined after lab QC may be > 10 cm deep.
}
\description{
This function verifies whether O and A horizons were named corrected based on % Total Carbon (O = TC >= 20%).
For duplicate horizons on a plot, chemistry variables are corrected using weighted averages, with sample depth
as the weight. Must run importData first. Note that Earthworms are summarized in joinStandData(). Only works for complete
visits and plots that haven't been abandoned. Note that data starts at 2007 because 2006 methods were pretty different.
}
\examples{
\dontrun{
importData() #imports using default odbc
# join only O horizon data for most recent cycle in ACAD. Note soil chemistry data are typically a year behind plot data.
soil_ACAD_O <- joinSoilLabData(park = 'ACAD', from = 2015, to = 2018, layers = 'O')

# join all park data from all layers and all years
soil_df_all <- joinSoilLabData(from = 2007, to = 2018, layers = 'all')
}
}
