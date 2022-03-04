#' @include joinLocEvent.R
#' @include prepTaxa.R
#'
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
#' \item{"all"}{Default. Returns all species, including low canopy species.}
#' \item{"canopy"}{Returns canopy-forming species only}
#'}
#'
#' @param numMicros Allows you to select 1, 2, or 3 microplots of data to summarize
#'
#' @param ... Other arguments passed to function.
#'
#' @return returns a dataframe with sapling data
#'
#' @examples
#' \dontrun{
#' importCSV(zip_name = "NETN_Forest_20210405.zip")
#' # compile sapling data for all parks and all species in most cycle 3
#' regen_data <- joinMicroSaplings(canopyForm = 'all', from = 2014, to = 2017)
#'
#' # compile sapling data for only canopy-forming (default) and native species in SAGA for all years
#' SAGA_regen <- joinMicroSaplings(park = 'SAGA', speciesType = 'native')
#'
#' # compile only 1 microplot of data for ACAD native canopy-forming species for all but first year
#' ACAD_regen_m1 <- joinMicroSaplings(park = 'ACAD', speciesType = 'native', numMicros = 1, from = 2007)
#' }
#' @export
#'
#------------------------
# Joins microplot tables and filters by park, year, and plot/visit type
#------------------------
joinMicroSaplings <- function(park = 'all', from = 2006, to = 2021, QAQC = FALSE, panels = 1:4,
                               locType = c('VS', 'all'), eventType = c('complete', 'all'),
                               speciesType = c('all', 'native', 'exotic', 'invasive'),
                               canopyForm = c('all', 'canopy'), numMicros = 3, ...){

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
  tryCatch(saps_vw <- get("MicroplotSaplings_NETN", envir = env) %>%
             select(Plot_Name, PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, SampleYear,
                    SampleDate, IsQAQC, SQSaplingCode, MicroplotCode, TSN, ScientificName, DBHcm),
           error = function(e){stop("MicroplotSaplings_NETN view not found. Please import view.")})

  tryCatch(saps_cnt <- get("MicroplotSaplingsCount_NETN", envir = env) %>%
             select(Plot_Name, PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, SampleYear, SampleDate,
                    IsQAQC, MicroplotCode, TSN, ScientificName, SaplingCount) %>% filter(SaplingCount > 0),
           error = function(e){stop("MicroplotSaplingCount_NETN view not found. Please import view.")})

  taxa_wide <- force(prepTaxa())

  # subset with EventID from plot_events to make function faster
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short')) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
           EventID, SampleYear, SampleDate, cycle, IsQAQC)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  pe_list <- unique(plot_events$EventID)

  sap_evs <- filter(saps_vw, EventID %in% pe_list) %>%
    left_join(plot_events, ., by = intersect(names(plot_events), names(.))) #%>%
    #filter(!(SampleYear == 2006 & MicroplotCode %in% c("UL", "B"))) # drop quads not sampled in 2006

  sap_cnt_evs <- filter(saps_cnt, EventID %in% pe_list)

  sap_tax <- left_join(sap_evs,
                       taxa_wide[, c("TSN", "ScientificName", "CanopyExclusion", "Exotic",
                                      "InvasiveNETN")],
                       by = c("TSN", "ScientificName"))

  sap_tax$Count <- ifelse(sap_tax$SQSaplingCode == "NP", 0,
                     ifelse(sap_tax$SQSaplingCode == "SS", 1,
                            NA_real_))

  sap_tax$ScientificName[sap_tax$SQSaplingCode == "NP"] <- "None present"

  # Create the left data.frame to join back to after filtering species types
  sap_left <- sap_tax %>% select(Plot_Name:MicroplotCode) %>% unique() #%>%
  # group_by(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode,
  #          PlotID, EventID, SampleYear, cycle, IsQAQC) %>%
  # mutate(nummicros = length(MicroplotCode)) # All plots have expected # micros
  # table(sap_left$nummicros) # all 3


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
           PlotCode, PlotID, EventID, IsQAQC, SampleYear, SampleDate, cycle, SQSaplingCode, MicroplotCode,
           TSN, ScientificName, CanopyExclusion, Exotic, InvasiveNETN, DBHcm, Count)

  # join filtered data back to full plot/visit/microplot list
  sap_comb <- left_join(sap_left, sap_nat, by = intersect(names(sap_left), names(sap_nat)))
  # table(complete.cases(sap_comb[,17])) #184 rows with missing SN values.
  # table(complete.cases(sap_comb[,22])) #184 rows with missing SN values.

  # Use SQs to fill blank ScientificNames after filtering
  sap_comb$ScientificName[is.na(sap_comb$ScientificName) &
                             (sap_comb$SQSaplingCode %in% c("SS", "NP"))] = "None present"
  sap_comb$ScientificName[is.na(sap_comb$ScientificName) &
                             (sap_comb$SQSaplingCode %in% c("ND", "NS"))] = "Not Sampled"
  sap_comb$Count[(sap_comb$ScientificName %in% c("None present")) & is.na(sap_comb$Count)] <- 0

  # Need to add the sapling count data to full dataset. Will average DBH of saplings recorded
  # for that plot/visit/species/microplot combination and make that the DBHcm
  sap_cnt_u <- unique(sap_cnt_evs[, c("EventID", "MicroplotCode", "TSN", "ScientificName", "SaplingCount")])

  sap_cnt1 <- left_join(sap_cnt_u, sap_comb, by = intersect(names(sap_cnt_u), names(sap_comb))) %>%
              group_by(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
                       PlotCode, PlotID, EventID, IsQAQC, SampleYear, SampleDate, cycle, SQSaplingCode, MicroplotCode,
                       TSN, ScientificName, CanopyExclusion, Exotic, InvasiveNETN) %>%
              summarize(DBHcm = round(mean(DBHcm, na.rm = T), 1),
                        Count = first(SaplingCount),
                        .groups = 'drop')

  sap_cmic <- if(numMicros == 3) {sap_cnt1
  } else if(numMicros == 2) {filter(sap_cnt1, MicroplotCode %in% c('UR','B')) #randomly determined this
  } else if(numMicros == 1) {filter(sap_cnt1, MicroplotCode == "UR")}

  sap_ccan <- if(canopyForm == "canopy"){filter(sap_cmic, CanopyExclusion == FALSE)
  } else {sap_cmic}

  sap_cnat <- switch(speciesType,
                    "all" = sap_ccan,
                    "native" = filter(sap_ccan, Exotic == FALSE),
                    "exotic" = filter(sap_ccan, Exotic == TRUE),
                    "invasive" = filter(sap_ccan, InvasiveNETN == TRUE)) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
           PlotCode, PlotID, EventID, IsQAQC, SampleYear, SampleDate, cycle, SQSaplingCode, MicroplotCode,
           TSN, ScientificName, CanopyExclusion, Exotic, InvasiveNETN, DBHcm, Count)


  sap_final <- rbind(sap_comb, sap_cnat) %>% arrange(Plot_Name, SampleYear, IsQAQC, MicroplotCode, ScientificName)

  return(data.frame(sap_final))
} # end of function


