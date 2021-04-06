#' @include joinLocEvent.R
#' @title joinMicroSaplings: compiles sapling data collected in microplots
#'
#' @importFrom dplyr arrange filter full_join group_by left_join select summarize
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#'
#' @description This function compiles sapling data collected in microplots, with a record for each sapling measured,
#' and its DBH. If no saplings were observed, returns "None present" for ScientificName and 0 for Count. If a record has
#' a blank ScientificName and associated data, it means it's a missing value. These are rare, but mostly occur in data <2011.
#' For the few plots with > 10 saplings of a given species within a microplot, the DBH is an average of measured sapling DBHs,
#' and the count is the number of saplings that were counted and not measured for DBH. In every case, the Count column is >1,
#' and can be filtered out, if needed by filtering Count == 1. Must run importData first.
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
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' \item{"invasive"}{Returns species on the Indicator Invasive List}
#' }
#'
#' @param canopyForm Allows you to filter on species growth form
#' \describe{
#' \item{"all"}{Returns all species, including low canopy species.}
#' \item{"canopy"}{Default. Returns canopy-forming species only}
#'}
#'
#' @param numMicros Allows you to select 1, 2, or 3 microplots of data to summarize
#'
#' @return returns a dataframe with sapling data
#'
#' @examples
#' importCSV(zip_name = "NETN_Forest_20210405.zip")
#' # compile sapling data for all parks and all species in most cycle 3
#' regen_data <- joinMicroSaplings(canopyForm = 'all', from = 2014, to = 2017)
#'
#' # compile sapling data for only canopy-forming (default) and native species in SAGA for all years
#' SAGA_regen <- joinMicroSaplings(park = 'SAGA', speciesType = 'native')
#'
#' # compile only 1 microplot of data for ACAD native canopy-forming species for all but first year
#' ACAD_regen_m1 <- joinMicroSaplings(park = 'ACAD', speciesType = 'native', numMicros = 1, from = 2007)
#'
#' @export
#'
#------------------------
# Joins microplot tables and filters by park, year, and plot/visit type
#------------------------
joinMicroSaplings <- function(park = 'all', from = 2006, to = 2021, QAQC = FALSE, panels = 1:4,
                               locType = c('VS', 'all'), eventType = c('complete', 'all'),
                               speciesType = c('all', 'native', 'exotic', 'invasive'),
                               canopyForm = c('canopy', 'all'), numMicros = 3, ...){

  # Match args and class
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA"))
  stopifnot(class(from) == "numeric", from >= 2006)
  stopifnot(class(to) == "numeric", to >= 2006)
  stopifnot(class(QAQC) == 'logical')
  stopifnot(panels %in% c(1, 2, 3, 4))
  stopifnot(numMicros %in% c(1, 2, 3))
  locType <- match.arg(locType)
  eventType <- match.arg(eventType)
  speciesType <- match.arg(speciesType)
  canopyForm <- match.arg(canopyForm)

  options(scipen = 100) # for TSNs

  env <- if(exists("VIEWS_NETN")){VIEWS_NETN} else {.GlobalEnv}

  # Prepare the microplot data
  tryCatch(saps_vw <- get("NETN_MicroplotSaplings", envir = env) %>%
             select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, SQSaplingCode,
                    MicroplotCode, TSN, ScientificName, DBHcm),
           error = function(e){stop("NETN_MicroplotSaplings view not found. Please import view.")})

  tryCatch(saps_cnt <- get("NETN_MicroplotSaplingsCount", envir = env) %>%
             select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC,
                    MicroplotCode, TSN, ScientificName, SaplingCount) %>% filter(SaplingCount > 0),
           error = function(e){stop("NETN_MicroplotSaplingCount view not found. Please import view.")})

  taxa_wide <- force(prepTaxa())

  # subset with EventID from plot_events to make function faster
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short')) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
           EventID, StartDate, StartYear, cycle, IsQAQC)

  pe_list <- unique(plot_events$EventID)

  sap_evs <- filter(saps_vw, EventID %in% pe_list) %>%
    left_join(plot_events, ., by = intersect(names(plot_events), names(.))) %>%
    filter(!(StartYear == 2006 & MicroplotCode %in% c("UL", "B"))) # drop quads not sampled in 2006

  sap_cnt_evs <- filter(saps_cnt, EventID %in% pe_list)

  sap_tax <- left_join(sap_evs,
                       taxa_wide[, c("TSN", "ScientificName", "CanopyExclusion", "Exotic",
                                      "InvasiveNETN")],
                       by = c("TSN", "ScientificName"))

  sap_tax$Count <- ifelse(sap_tax$SQSaplingCode == "NP", 0,
                     ifelse(sap_tax$SQSaplingCode == "SS", 1,
                            NA_real_))
  sap_tax$ScientificName[is.na(sap_tax$ScientificName) & !is.na(sap_tax$Count)] <- "None present"


  sap_tax$ScientificName[sap_tax$SQSaplingCode == "NP"] <- "None present"
  sap_tax$Count[sap_tax$SQSaplingCode == "NP"] <- 0

  sap_tax$CanopyExclusion[sap_tax$ScientificName == "None present"] <- FALSE
  sap_tax$Exotic[sap_tax$ScientificName == "None present"] <- FALSE
  sap_tax$InvasiveNETN[sap_tax$ScientificName == "None present"] <- FALSE

  sap_mic <- if(numMicros == 3) {sap_tax
  } else if(numMicros == 2) {filter(sap_tax, MicroplotCode %in% c('UR','B')) #randomly determined this
  } else if(numMicros == 1) {filter(sap_tax, MicroplotCode == "UR")}

  sap_can <- if(canopyForm == "canopy"){filter(sap_mic, CanopyExclusion == FALSE)
  } else {sap_mic}

  sap_nat <- switch(speciesType,
                     "all" = sap_can,
                     "native" = filter(sap_can, Exotic == FALSE),
                     "exotic" = filter(sap_can, Exotic == TRUE),
                     "invasive" = filter(sap_can, InvasiveNETN == TRUE)) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
           PlotCode, PlotID, EventID, IsQAQC, StartYear, cycle, SQSaplingCode, MicroplotCode,
           TSN, ScientificName, CanopyExclusion, Exotic, InvasiveNETN, DBHcm, Count)


  # Find plot visits that were filtered out based on nummicros, canopy form or nativity to rbind with seed_nat
  # for full dataset
  exp_df <- data.frame(MicroplotCode = rep(c("UR", "UL", "B"), times = length(from:to)),
                       StartYear = rep(from:to, times = 3))

  visits <- plot_events %>% select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
                                   PlotCode, PlotID, EventID, IsQAQC, StartYear, cycle) %>% unique() %>%
    filter(!(EventID %in% c(257, 710))) # dropping ACAD-029-2010 & SAGA-008-2008

  bad_visits <- sap_tax %>% filter(SQSaplingCode %in% c("ND", "NS") |
                                       is.na(Count)) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
           PlotCode, PlotID, EventID, IsQAQC, StartYear, cycle, SQSaplingCode, MicroplotCode,
           TSN, ScientificName, CanopyExclusion, Exotic, InvasiveNETN, DBHcm, Count)


  # Need to properly add back in the visits with issues
  # First, none of the early species records without counts were exotic or invasive
  # ACAD-029-2010 (eID 710) and SAGA-008-2010 (eID 257) have SQs of ND (should be NS)

  exp_df2 <- full_join(visits, exp_df, by = "StartYear") %>%
    filter(!(StartYear == 2006 & MicroplotCode %in% c("UL", "B"))) %>%
    filter(!(EventID %in% bad_visits$EventID))

  sap_exp2 <- full_join(sap_nat %>% filter((!SQSaplingCode %in% c("ND", "NS")) &
                                               (!is.na(Count))),
                         exp_df2, by = intersect(names(sap_nat), names(exp_df2)))

  sap_exp2$SQSaplingCode[is.na(sap_exp2$SQSaplingCode)] <- "NP"
  sap_exp2$ScientificName[is.na(sap_exp2$ScientificName)] = "None present"
  sap_exp2$CanopyExclusion[is.na(sap_exp2$CanopyExclusion)] = FALSE
  sap_exp2$Exotic[is.na(sap_exp2$Exotic)] = FALSE
  sap_exp2$InvasiveNETN[is.na(sap_exp2$InvasiveNETN)] = FALSE
  sap_exp2$Count[is.na(sap_exp2$Count)] <- 0

  sap_comb <- rbind(sap_exp2, bad_visits)
  # Need to add the sapling count data to full dataset. Will average DBH of saplings recorded
  # for that plot/visit/species/microplot combination and make that the DBHcm
  sap_cnt_u <- unique(sap_cnt_evs[, c("EventID", "MicroplotCode", "TSN", "ScientificName", "SaplingCount")])

  sap_cnt1 <- left_join(sap_cnt_u, sap_comb, by = intersect(names(sap_cnt_u), names(sap_comb))) %>%
              group_by(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
                       PlotCode, PlotID, EventID, IsQAQC, StartYear, cycle, SQSaplingCode, MicroplotCode,
                       TSN, ScientificName, CanopyExclusion, Exotic, InvasiveNETN) %>%
              summarize(DBHcm = round(mean(DBHcm, na.rm = T), 1),
                        Count = first(SaplingCount),
                        .groups = 'drop')


  sap_final <- rbind(sap_comb, sap_cnt1) %>% arrange(Plot_Name, StartYear, IsQAQC, MicroplotCode, ScientificName)

  return(data.frame(sap_final))
} # end of function


