#' @title joinLocEvent: compile Location and Event data with filtering options.
#'
#' @importFrom stringr str_pad
#'
#' @description This function combines location and event data. Must run importData first.
#'
#' @param park Combine data from all parks or one or more parks at a time. Valid inputs:
#' \describe{
#' \item{"all"}{Includes all parks in the network}
#' \item{"ACAD"}{Acadia NP only}
#' \item{"MABI"}{Marsh-Billings-Rockefeller NHP only}
#' \item{"MIMA"}{Minute Man NHP only}
#' \item{"MORR"}{Morristown NHP only}
#' \item{"ROVA"}{Roosevelt-Vanderbilt NHS only}
#' \item{"SAGA"}{Saint-Gaudens NHS only}
#' \item{"SARA"}{Saratoga NHP only}
#' \item{"WEFA"}{Weir Farm NHS only}}
#' @param from Year to start analysis, ranging from 2006 to current year
#' @param to Year to stop analysis, ranging from 2006 to current year
#' @param QAQC Allows you to remove or include QAQC events.
#' \describe{
#' \item{FALSE}{Default. Only returns visits that are not QAQC visits}
#' \item{TRUE}{Returns all visits, including QAQC visits}}
#' @param abandonded Allows you to include (TRUE) or remove (FALSE; Default.) or abandoned plots.
#' \describe{
#' \item{FALSE}{Default. Only returns plots that were not rejected.}
#' \item{TRUE}{returns all records}}
#' @param locType Allows you to only include plots that are part of the GRTS sample design or include all plots, such as deer exclosures
#' \describe{
#' \item{"VS"}{Only include plots that are part of the Vital Signs GRTS sample design}
#' \item{"all"}{Include all plots, such as plots in deer exclosures or test plots.}}
#' @param eventType Allows you to include only complete sampling events or all sampling events
#' \describe{
#' \item{"complete"}{Default. Only include sampling events for a plot that are complete.}
#' \item{"all}{Include all plot events with a record in tblCOMN.Event, including plots missing most of the data
#' associated with that event (eg ACAD-029.2010). This feature is currently hard-coded in the function.}
#' }
#' @param panels Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
#' If more than one panel is selected, specify by c(1, 3), for example.
#'
#' @return returns a dataframe with location and visit events
#'
#' @examples
#' importCSV('./forest_csvs')
#' # Select most recent survey of data from WEFA
#' WEFA_data <- joinLocEvent(park = 'WEFA', panels = c(2,4), from = 2015, to = 2018)
#'
#' # Select data from cycle 3 for MABI and SAGA
#' cycle3 <- joinLocEvent(park = c("MABI", "SAGA"), from = 2014, to = 2017) # all parks is default
#'
#' # Select data from plots that had a QA/QC event in ACAD in 2018
#' ACAD_data <- joinLocEvent(park = 'ACAD', QAQC = TRUE, from = 2018)
#' QAQC_plots <- ACAD_data$Plot_Name[which(ACAD_data$Event_QAQC == TRUE)]
#' ACAD_QAQC <- ACAD_data %>% filter(Plot_Name %in% QAQC_plots)
#'
#' @export
#'

#------------------------
# Joins tbl_Locations and tbl_Events tables and filters by park, year, and plot/visit type
#------------------------
joinLocEvent<-function(park = "all", from = 2006, to = 2021, QAQC = FALSE, abandoned = FALSE, panels = 1:4,
                       locType = c('VS', 'all'), eventType = c('complete', 'all'), output = 'short', ...){

  # Match args and class
  locType <- match.arg(locType)
  eventType <- match.arg(eventType)
  park <- match.arg(park, several.ok = TRUE,
                     c("all", "ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA"))
  stopifnot(class(QAQC) == 'logical')
  stopifnot(class(abandoned) == 'logical')
  stopifnot(class(panels) == "integer", panels %in% c(1, 2, 3, 4))
  output <- match.arg(output, c("short", "verbose"))

  env <- if(exists("VIEWS_NETN")){VIEWS_NETN} else {.GlobalEnv}

  # Check if the views exist and stop if they don't
  tryCatch(plots <- get("COMN_Plots", envir = env),
           error = function(e){stop("COMN_Plots view not found. Please import views.")}
  )

  tryCatch(events <- get("COMN_Events", envir = env),
           error = function(e){stop("COMN_Events view not found. Please import views.")}
  )

  # Merge COMN_Plots and COMN_Events
  merge_names <- intersect(names(plots), names(events))
    # merge_names: "Park.Network", "Park.Unit", "Park.SubUnit", "PlotType.Code", "PlotType.Label",
    # "Panel.Code", "Panel.Label", "Plot.Code", "Plot.IsAbandoned", ExportDate
  merge_names <- merge_names[merge_names != "ExportDate"]
  plot_events <- merge(plots, events, by = merge_names, all.x = TRUE, all.y = TRUE)

  # Filter output based on function arguments
  plot_events <- if(output == 'short'){
    plot_events[, c("Park.Network", "Park.Unit", "Park.SubUnit", "PlotType.Code", "Panel.Code", "Plot.Code",
                    "Plot.IsAbandoned", "Plot.ID", "Plot.LegacyID", "Plot.xCoordinate", "Plot.yCoordinate", "Zone.Code",
                    "Physiography.Code", "Physiography.Label", "Physiography.Summary", "Plot.Aspect", "Plot.Orientation",
                    "Plot.GRTS", "Plot.IsOrientationChanged", "Plot.IsStuntedWoodland",
                    "Event.ID", "Event.LegacyID", "Event.StartDate", "Event.IsQAQC", "Event.StartYear",
                    "Plot.Notes", "Plot.Directions", "Event.Notes", "Event.StandNotes")]} else {plot_events}


  # microbenchmark::microbenchmarl(plot_events$Plot_Name <- paste(plot_events$Park.Unit,
  #                                sprintf("%03d", plot_events$Plot.Code), sep = "-"), #sprintf was 2x slower
  plot_events$Plot_Name <- paste(plot_events$Park.Unit,
                                  stringr::str_pad(plot_events$Plot.Code, 3, side = 'left', "0"),
                                 sep = "-")

  plot_events1 <- if(locType == 'VS'){subset(plot_events, PlotType.Code == "VS")
  } else if (locType=='all') {(plot_events)}

  plot_events2 <- if(abandoned == FALSE){subset(plot_events1, Plot.IsAbandoned == FALSE)
  } else if (abandoned == TRUE) {(plot_events1)}

  plot_events3 <- if(all(park == "all")){plot_events2
    } else {subset(plot_events2, Park.Unit %in% park)}

  plot_events4 <- if(QAQC == FALSE){subset(plot_events3, Event.IsQAQC == 0)
    } else {plot_events3}

  plot_events5 <- if(eventType == "complete"){
    subset(plot_events4, !(Plot_Name == 'ACAD-029' & Event.StartDate == '2010-07-07'))
    } else {plot_events4}

  plot_events6 <- plot_events5[plot_events5$Panel.Code %in% c(panels), ]
  plot_events7 <- plot_events6[plot_events6$Event.StartYear %in% c(from:to), ]

  plot_events7$cycle[plot_events7$Event.StartYear %in% c(2006:2009)] <- 1
  plot_events7$cycle[plot_events7$Event.StartYear %in% c(2010:2013)] <- 2
  plot_events7$cycle[plot_events7$Event.StartYear %in% c(2014:2017)] <- 3
  plot_events7$cycle[plot_events7$Event.StartYear %in% c(2018:2022)] <- 4

  return(plot_events7)
} # end of function

