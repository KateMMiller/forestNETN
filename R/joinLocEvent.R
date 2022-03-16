#' @title joinLocEvent: compile Location and Event data with filtering options.
#'
#' @importFrom dplyr filter full_join select
#' @importFrom stringr str_pad
#' @importFrom magrittr %>%
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
#'
#' @param from Year to start analysis, ranging from 2006 to current year
#' @param to Year to stop analysis, ranging from 2006 to current year
#'
#' @param QAQC Allows you to remove or include QAQC events.
#' \describe{
#' \item{FALSE}{Default. Only returns visits that are not QAQC visits}
#' \item{TRUE}{Returns all visits, including QAQC visits}}
#'
#' @param abandoned Allows you to include (TRUE) or remove (FALSE; Default.) or abandoned plots.
#' \describe{
#' \item{FALSE}{Default. Only returns plots that were not rejected.}
#' \item{TRUE}{returns all records}}
#'
#' @param locType Allows you to only include plots that are part of the GRTS sample design or include all plots, such as deer exclosures
#' \describe{
#' \item{"VS"}{Only include plots that are part of the Vital Signs GRTS sample design}
#' \item{"all"}{Include all plots, such as plots in deer exclosures or test plots.}}
#'
#' @param eventType Allows you to include only complete sampling events or all sampling events
#' \describe{
#' \item{"complete"}{Default. Only include sampling events for a plot that are complete.}
#' \item{"all}{Include all plot events with a record in tblCOMN.Event, including plots missing most of the data
#' associated with that event (eg ACAD-029.2010). This feature is currently hard-coded in the function.}}
#'
#' @param panels Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
#' If more than one panel is selected, specify by c(1, 3), for example.
#'
#' @param output Allows you to return all columns or just the most important columns for analysis. Valid
#' inputs are "short" and "verbose".
#'
#' @return returns a dataframe with location and visit events
#'
#' @examples
#' \dontrun{
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
#' }
#'
#' @export
#'

#------------------------
# Joins Plots and Events views and filters by park, year, and plot/visit type
#------------------------
joinLocEvent<-function(park = "all", from = 2006, to = 2021, QAQC = FALSE, abandoned = FALSE, panels = 1:4,
                       locType = c('VS', 'all'), eventType = c('complete', 'all'),
                       output = c('short', 'verbose')){

  # Match args and class
  locType <- match.arg(locType)
  eventType <- match.arg(eventType)
  park <- match.arg(park, several.ok = TRUE,
                     c("all", "ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA"))
  stopifnot(class(from) == "numeric", from >= 2006)
  stopifnot(class(to) == "numeric", to >= 2006)
  stopifnot(class(QAQC) == 'logical')
  stopifnot(class(abandoned) == 'logical')
  stopifnot(panels %in% c(1, 2, 3, 4))
  output <- match.arg(output, c("short", "verbose"))

  env <- if(exists("VIEWS_NETN")){VIEWS_NETN} else {.GlobalEnv}

  # Check if the views exist and stop if they don't
  tryCatch(plots <- get("Plots_NETN", envir = env),
           error = function(e){stop("Plots_NETN view not found. Please import views.")}
  )

  tryCatch(events <- get("Events_NETN", envir = env),
           error = function(e){stop("Events_NETN view not found. Please import views.")}
  )

  # Merge COMN_Plots and COMN_Events
  plots <- plots %>% select(-ExportDate)
  events <- events %>% select(-ExportDate)
  merge_names <- intersect(names(plots), names(events))
    # merge_names: "Plot_Name", "Network", "ParkUnit", "ParkSubUnit", "PlotTypeCode", "PlotTypeLabel",
    # "PanelCode", "PanelLabel", "PlotCode", "IsAbandoned"

  plot_events <- full_join(plots, events, by = merge_names)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  # Filter output based on function arguments
  plot_events <- if(output == 'short'){
    plot_events[, c("Plot_Name", "Network", "ParkUnit", "ParkSubUnit", "PlotTypeCode", "PanelCode", "PlotCode",
                    "IsAbandoned", "PlotID", "PlotLegacyID", "xCoordinate", "yCoordinate", "ZoneCode",
                    "PhysiographyCode", "PhysiographyLabel", "PhysiographySummary", "Aspect",
                    "Orientation", "GRTS", "IsOrientationChanged", "IsStuntedWoodland",
                    "EventID", "EventLegacyID", "IsQAQC", "SampleYear", "SampleDate",
                    "PlotNotes", "Directions", "EventNotes", "StandNotes")]} else {plot_events}


  # microbenchmark::microbenchmarl(plot_events$Plot_Name <- paste(plot_events$Park.Unit,
  #                                sprintf("%03d", plot_events$PlotCode), sep = "-"), #sprintf was 2x slower


  plot_events1 <- if(locType == 'VS'){filter(plot_events, PlotTypeCode == "VS")
  } else if (locType=='all') {(plot_events)}

  plot_events2 <- if(abandoned == FALSE){filter(plot_events1, IsAbandoned == FALSE)
  } else if (abandoned == TRUE) {(plot_events1)}

  plot_events3 <- if(any(park == "all")){plot_events2
  } else {filter(plot_events2, ParkUnit %in% park)}

  plot_events4 <- if(QAQC == FALSE){filter(plot_events3, IsQAQC == 0)
    } else {plot_events3}

  plot_events5 <- if(eventType == "complete"){
    filter(plot_events4, !(Plot_Name == 'ACAD-029' & SampleDate == '2010-07-07'))
    } else {plot_events4}

  plot_events6 <- plot_events5[plot_events5$PanelCode %in% c(panels), ]
  plot_events7 <- plot_events6[plot_events6$SampleYear %in% c(from:to), ]

  plot_events7$cycle[plot_events7$SampleYear %in% c(2006:2009)] <- 1
  plot_events7$cycle[plot_events7$SampleYear %in% c(2010:2013)] <- 2
  plot_events7$cycle[plot_events7$SampleYear %in% c(2014:2017)] <- 3
  plot_events7$cycle[plot_events7$SampleYear %in% c(2018:2021)] <- 4
    # need to update for 2022

  # Adding ACAD MDI Units to ParkSubUnit column
  MDI_West <- c('ACAD-016', 'ACAD-017', 'ACAD-018', 'ACAD-019', 'ACAD-024',
                'ACAD-025', 'ACAD-026', 'ACAD-027', 'ACAD-029', 'ACAD-031',
                'ACAD-032', 'ACAD-033', 'ACAD-038', 'ACAD-041', 'ACAD-042',
                'ACAD-049', 'ACAD-050', 'ACAD-053', 'ACAD-054', 'ACAD-055',
                'ACAD-057', 'ACAD-060', 'ACAD-061', 'ACAD-062', 'ACAD-063',
                'ACAD-071', 'ACAD-072', 'ACAD-073', 'ACAD-076', 'ACAD-080',
                'ACAD-095', 'ACAD-096', 'ACAD-101', 'ACAD-102', 'ACAD-103',
                'ACAD-104', 'ACAD-105', 'ACAD-106', 'ACAD-107', 'ACAD-108',
                'ACAD-113', 'ACAD-114', 'ACAD-119', 'ACAD-120', 'ACAD-122',
                'ACAD-125', 'ACAD-135', 'ACAD-136', 'ACAD-137', 'ACAD-138',
                'ACAD-139', 'ACAD-140', 'ACAD-141', 'ACAD-143', 'ACAD-147',
                'ACAD-154', 'ACAD-155', 'ACAD-156', 'ACAD-159', 'ACAD-160',
                'ACAD-162', 'ACAD-168', 'ACAD-172', 'ACAD-174', 'ACAD-175')
  MDI_East <- c('ACAD-004', 'ACAD-005', 'ACAD-006', 'ACAD-007', 'ACAD-008',
                'ACAD-009', 'ACAD-010', 'ACAD-011', 'ACAD-012', 'ACAD-013',
                'ACAD-014', 'ACAD-015', 'ACAD-028', 'ACAD-030', 'ACAD-034',
                'ACAD-035', 'ACAD-037', 'ACAD-039', 'ACAD-040', 'ACAD-043',
                'ACAD-044', 'ACAD-045', 'ACAD-046', 'ACAD-047', 'ACAD-048',
                'ACAD-051', 'ACAD-052', 'ACAD-056', 'ACAD-058', 'ACAD-059',
                'ACAD-068', 'ACAD-074', 'ACAD-075', 'ACAD-077', 'ACAD-078',
                'ACAD-079', 'ACAD-081', 'ACAD-082', 'ACAD-083', 'ACAD-084',
                'ACAD-085', 'ACAD-086', 'ACAD-087', 'ACAD-088', 'ACAD-089',
                'ACAD-092', 'ACAD-093', 'ACAD-094', 'ACAD-109', 'ACAD-110',
                'ACAD-111', 'ACAD-112', 'ACAD-115', 'ACAD-116', 'ACAD-117',
                'ACAD-118', 'ACAD-121', 'ACAD-123', 'ACAD-124', 'ACAD-126',
                'ACAD-127', 'ACAD-128', 'ACAD-129', 'ACAD-130', 'ACAD-131',
                'ACAD-132', 'ACAD-133', 'ACAD-134', 'ACAD-142', 'ACAD-144',
                'ACAD-148', 'ACAD-149', 'ACAD-150', 'ACAD-151', 'ACAD-152',
                'ACAD-153', 'ACAD-157', 'ACAD-158', 'ACAD-163', 'ACAD-170',
                'ACAD-171', 'ACAD-173')

  plot_events7$ParkSubUnit[plot_events7$Plot_Name %in% MDI_West] <- "ACAD_MDI_West"
  plot_events7$ParkSubUnit[plot_events7$Plot_Name %in% MDI_East] <- "ACAD_MDI_East"


  return(data.frame(plot_events7))
} # end of function

