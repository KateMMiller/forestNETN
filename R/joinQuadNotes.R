#' @include joinLocEvent.R
#'
#' @title joinQuadNotes: compiles quadrat and species-level notes from Quadrat Data and Species tabs.
#'
#' @importFrom dplyr arrange filter left_join mutate rename select
#' @importFrom magrittr %>%
#'
#' @description This function combines quadrat-level notes from Quadrat Data and Quadrat Species tabs and
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
#' @param panels Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
#' If more than one panel is selected, specify by c(1, 3), for example.
#'
#' @param ... Other arguments passed to function.
#'
#' @return Returns a dataframe with all quadrat-related notes. Only returns visits with notes. The Note_Info
#' column is either the quadrat or the species the note was recorded for. The Sample_Info column is either the
#' sample qualifier for the quadrat, where SS = successfully sampled, NS = not sampled, NP = no species present,
#' NC = not collected by protocol, and PM = permanently missing. If the Sample_Info column has "Collected" listed
#' it means the species with the note was also collected.
#'
#' @examples
#' \dontrun{
#' importData()
#' # compile quadrat data for invasive species in WEFA for 2019
#' WEFA_quadnotes <- joinQuadNotes(park = 'WEFA', from = 2019, to = 2019)
#'}
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
  tryCatch(quadspp <- get("QuadSpecies_NETN", envir = env) %>%
                      select(PlotID, EventID, ScientificName, QuadSppNote, IsCollected),
           error = function(e){stop("QuadSpecies_NETN view not found. Please import view.")})

  tryCatch(quadnotes <- get("QuadNotes_NETN", envir = env) %>%
                        select(PlotID, EventID, SQQuadCharCode, SQQuadCharNotes,
                               SQQuadSppNotes, QuadratCode, QuadratNote) %>%
                               unique(),
           error = function(e){stop("QuadNotes_NETN view not found. Please import view.")})

  # subset with EventID from plot_events to make function faster
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short', ...)) %>%
                 select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
                        EventID, SampleYear, cycle, IsQAQC)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  pe_list <- unique(plot_events$EventID)

  quadspp_evs <- filter(quadspp, EventID %in% pe_list) %>%
                 left_join(plot_events, ., by = intersect(names(plot_events), names(.)))

  quadnotes_evs <- filter(quadnotes, EventID %in% pe_list) %>%
                   left_join(plot_events, ., by = intersect(names(plot_events), names(.)))

  # Species-level notes
  spp_notes <- quadspp_evs %>% mutate(Note_Type = "Quad_Species",
                                      Sample_Info = ifelse(IsCollected == TRUE, "Collected", NA_character_)) %>%
                               rename(Note_Info = ScientificName, Notes = QuadSppNote) %>%
                               select(-IsCollected) %>% na.omit(Notes)

  # SQ Species quad-level notes
  sq_spp_notes <- quadnotes_evs %>% select(Plot_Name:IsQAQC,
                                           Sample_Info = SQQuadCharCode,
                                           Note_Info = QuadratCode,
                                           Notes = SQQuadSppNotes) %>%
                                    mutate(Note_Type = "Quad_SQ_Species") %>%
                                    select(names(spp_notes)) %>% na.omit(Notes)

  # SQ Char quad-level notes
  sq_char_notes <- quadnotes_evs %>% select(Plot_Name:IsQAQC,
                                            Sample_Info = SQQuadCharCode,
                                            Note_Info = QuadratCode,
                                            Notes = SQQuadCharNotes) %>%
                                     mutate(Note_Type = "Quad_SQ_Character") %>%
                                     select(names(spp_notes)) %>% na.omit(Notes)

  # SQ generic quad-level notes
  gen_notes <- quadnotes_evs %>% select(Plot_Name:IsQAQC,
                                            Sample_Info = SQQuadCharCode,
                                            Note_Info = QuadratCode,
                                            Notes = QuadratNote) %>%
                                 mutate(Note_Type = "Quad_General") %>%
                                 select(names(spp_notes)) %>% na.omit(Notes)


  quad_notes <- rbind(spp_notes, sq_spp_notes, sq_char_notes, gen_notes) %>%
                unique() %>%
                arrange(Plot_Name, SampleYear, IsQAQC, Note_Info, Note_Type) %>%
                select(Plot_Name:IsQAQC, Note_Type, Sample_Info, Note_Info, Notes)

  return(data.frame(quad_notes))

  }
