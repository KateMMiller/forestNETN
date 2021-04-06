#' @include joinLocEvent.R
#'
#' @title joinQuadNotes: compiles quadrat and species-level notes from Quadrat Data and Species tabs.
#'
#' @importFrom dplyr arrange filter left_join mutate rename select
#' @importFrom magrittr %>%
#'
#' @description This function combines qudrat-level notes from Quadrat Data and Quadrat Species tabs and
#' species level notes in the Quadrat Species Tab.
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
#' @param locType Allows you to only include plots that are part of the GRTS sample design or
#' include all plots, such as deer exclosures.
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
#' @return Returns a dataframe with all quadrat-related notes. Only returns visits with notes. The Note_Info
#' column is either the quadrat or the species the note was recorded for. The Sample_Info column is either the
#' sample qualifier for the quadrat, where SS = successfully sampled, NS = not sampled, NP = no species present,
#' NC = not collected by protocol, and PM = permanently missing. If the Sample_Info column has "Collected" listed
#' it means the species with the note was also collected.
#'
#' @examples
#' importData()
#' # compile quadrat data for invasive species in SARA for 2019
#' SARA_quads <- joinQuadNotes(park = 'SARA', from = 2019, to = 2019)
#'
#'
#' @export
#'
#------------------------
# Joins quadrat tables and filters by park, year, and plot/visit type
#------------------------

joinQuadNotes <- function(park = 'all', from = 2006, to = 2021, QAQC = FALSE, panels = 1:4,
                          locType = c('VS', 'all'), eventType = c('complete', 'all'), ...){

  # Match args and class
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA"))
  stopifnot(class(from) == "numeric", from >= 2006)
  stopifnot(class(to) == "numeric", to >= 2006)
  stopifnot(class(QAQC) == 'logical')
  stopifnot(panels %in% c(1, 2, 3, 4))
  locType <- match.arg(locType)
  eventType <- match.arg(eventType)

  options(scipen = 100)
  env <- if(exists("VIEWS_NETN")){VIEWS_NETN} else {.GlobalEnv}

  # Prepare quad datasets
  tryCatch(quadspp <- get("NETN_QuadSpecies", envir = env) %>%
                      select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, SQQuadSppCode,
                             QuadratCode, SQQuadSppNotes, ScientificName, IsCollected, QuadSppNote),
           error = function(e){stop("NETN_QuadSpecies view not found. Please import view.")})

  tryCatch(quadchr <- get("COMN_QuadCharacter", envir = env) %>%
                      select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, SQQuadCharCode,
                             QuadratCode, SQQuadCharNotes),
           error = function(e){stop("COMN_QuadCharacter view not found. Please import view.")}
  )

  # subset with EventID from plot_events to make function faster
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short')) %>%
                 select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
                        EventID, StartYear, cycle, IsQAQC)

  pe_list <- unique(plot_events$EventID)

  quadspp_evs <- filter(quadspp, EventID %in% pe_list) %>%
                   left_join(plot_events, ., by = intersect(names(plot_events), names(.)))

  quadchr_evs <- filter(quadchr, EventID %in% pe_list) %>%
                   left_join(plot_events, ., by = intersect(names(plot_events), names(.)))

  # Split quadrat-level notes from species-level notes for easier compiling
  spp_notes <- quadspp_evs %>% select(-SQQuadSppCode, -QuadratCode, -SQQuadSppNotes) %>%
                               filter(!is.na(QuadSppNote)) %>%
                               mutate(Note_Type = "Species Notes",
                                      Sample_Info = ifelse(IsCollected == 1, "Collected", NA)) %>%
                               unique() %>% rename(Note_Info = ScientificName,
                                                   Notes = QuadSppNote) %>%
                               select(-IsCollected)

  quadspp_notes <- quadspp_evs %>% select(-ScientificName, -IsCollected, -QuadSppNote) %>%
                                   mutate(Note_Type = "SQQ Species") %>%
                                   filter(!is.na(SQQuadSppNotes)) %>% unique() %>%
                                   rename(Sample_Info = SQQuadSppCode, Notes = SQQuadSppNotes,
                                          Note_Info = QuadratCode)

  quadchr_notes <- quadchr_evs %>% mutate(Note_Type = "SQQ Character") %>%
                                   filter(!is.na(SQQuadCharNotes)) %>% unique() %>%
                                   rename(Sample_Info = SQQuadCharCode, Notes = SQQuadCharNotes,
                                          Note_Info = QuadratCode)

  quad_notes <- rbind(quadspp_notes, quadchr_notes, spp_notes) %>% filter(!is.na(Notes)) %>% unique() %>%
                arrange(Plot_Name, StartYear, IsQAQC, Note_Info, Note_Type) %>%
                select(Plot_Name:IsQAQC, Note_Type, Sample_Info, Note_Info, Notes)

  return(data.frame(quad_notes))

  }
