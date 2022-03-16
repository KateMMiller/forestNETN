#' @include joinLocEvent.R
#'
#' @title joinMicroNotes: compiles plot, stand,
#'
#' @importFrom dplyr arrange filter inner_join mutate select
#' @importFrom magrittr %>%
#'
#' @description This function combines microplot notes from Saplings, Seedlings, and Shrubs.
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
#' \item{"all}{Include all plot events with a record in tblCOMN.Event, including plots missing
#' most of the data associated with that event (eg ACAD-029.2010). This feature is currently
#' hard-coded in the function.}}
#'
#' @param panels Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
#' If more than one panel is selected, specify by c(1, 3), for example.
#'
#' @return Returns a dataframe with all microplot-related notes. Only returns records with notes.
#'
#' @examples
#' \dontrun{
#' importData()
#' # compile microplot notes for invasive species in SARA for 2018
#' SARA_quads <- joinMicroNotes(park = 'SARA', from = 2018, to = 2018)
#'}
#'
#' @export
#'

joinMicroNotes <- function(park = 'all', from = 2006, to = 2021, QAQC = FALSE, panels = 1:4,
                          locType = c('VS', 'all'), eventType = c('complete', 'all')){

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

  tryCatch(saps_vw <- get("MicroplotSaplings_NETN", envir = env) %>%
             select(Plot_Name, PlotID, EventID, MicroplotCode, SQSaplingNotes, SaplingNote),
           error = function(e){stop("MicroplotSaplings_NETN view not found. Please import view.")})

  tryCatch(seeds_vw <- get("MicroplotSeedlings_NETN", envir = env) %>%
             select(Plot_Name, PlotID, EventID, MicroplotCode, SQSeedlingNotes, SeedlingSizeNote) %>%
             unique(),
           error = function(e){stop("MicroplotSeedlings_NETN view not found. Please import view.")})

  tryCatch(shrubs_vw <- get("MicroplotShrubs_NETN", envir = env) %>%
             select(Plot_Name, PlotID, EventID, MicroplotCode, SQShrubNotes, ShrubNote),
           error = function(e){stop("MicroplotShrubs_NETN view not found. Please import view.")})

  plot_events <- joinLocEvent(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                              locType = locType, eventType = eventType, output = 'verbose') %>%
    select(Plot_Name, PlotID, EventID, SampleYear, IsQAQC)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  saps_long <- saps_vw %>% rename(Micro_SQ_Sapling = SQSaplingNotes,
                                  Micro_Sapling = SaplingNote) %>%
                           pivot_longer(cols = c(Micro_SQ_Sapling, Micro_Sapling),
                                        names_to = "Note_Type",
                                        values_to = "Notes",
                                        values_drop_na = TRUE) %>%
                           mutate(Sample_Info = paste0("Microplot: ", MicroplotCode)) %>%
                           select(-MicroplotCode)

  seeds_long <- seeds_vw %>% rename(Micro_SQ_Seedling = SQSeedlingNotes,
                                   Micro_Seedling = SeedlingSizeNote) %>%
                            pivot_longer(cols = c(Micro_SQ_Seedling, Micro_Seedling),
                                         names_to = "Note_Type",
                                         values_to = "Notes",
                                         values_drop_na = TRUE) %>%
                            mutate(Sample_Info = paste0("Microplot: ", MicroplotCode)) %>%
                            select(-MicroplotCode)

  shrubs_long <- shrubs_vw %>% rename(Micro_SQ_Shrub = SQShrubNotes,
                                      Micro_Shrub = ShrubNote) %>%
                               pivot_longer(cols = c(Micro_SQ_Shrub, Micro_Shrub),
                                            names_to = "Note_Type",
                                            values_to = "Notes",
                                            values_drop_na = TRUE) %>%
                               mutate(Sample_Info = paste0("Microplot: ", MicroplotCode)) %>%
                               select(-MicroplotCode)

  micro_comb <- rbind(saps_long, seeds_long, shrubs_long) %>% unique()

  micro_evs <- inner_join(plot_events, micro_comb,
                          by = intersect(names(plot_events), names(micro_comb))) %>%
               select(Plot_Name, PlotID, EventID, SampleYear, IsQAQC, Note_Type, Sample_Info, Notes) %>%
               arrange(Plot_Name, SampleYear, IsQAQC, Note_Type, Sample_Info)

  return(micro_evs)
}
