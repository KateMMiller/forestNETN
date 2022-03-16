#' @include joinLocEvent.R
#' @include prepTaxa.R
#'
#' @title joinMicroSeedlings: compiles seedling data collected in microplots
#'
#' @importFrom dplyr arrange filter full_join left_join select
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#'
#' @description This function combines seedling data collected in microplots. If no seedlings were observed, returns
#' "None present" for ScientificName and 0 for seedling densities. If a record has a blank ScientificName and associated
#' data, it means it's a missing value. These are rare, but mostly occur in data <2011. Must run importData first.
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
#' @return returns a dataframe with seedling densities
#'
#' @examples
#' \dontrun{
#' importCSV('./forest_csvs/')
#' # compile seedling data for all parks and all species in most cycle 3
#' regen_data <- joinMicroSeedlings(canopyForm = 'all', from = 2014, to = 2017)
#'
#' # compile seedling data for only canopy-forming (default) and native species in SAGA for all years
#' SAGA_regen <- joinMicroSeedlings(park = 'SAGA', speciesType = 'native')
#'
#' # compile only 1 microplot of data for ACAD native canopy-forming species for all but first year
#' ACAD_regen_m1 <- joinMicroSeedlings(park = 'ACAD', speciesType = 'native', numMicros = 1, from = 2007)
#' }
#' @export
#'
#------------------------
# Joins microplot tables and filters by park, year, and plot/visit type
#------------------------
joinMicroSeedlings <- function(park = 'all', from = 2006, to = 2021, QAQC = FALSE, panels = 1:4,
                               locType = c('VS', 'all'), eventType = c('complete', 'all'),
                               speciesType = c('all', 'native', 'exotic', 'invasive'),
                               canopyForm = c('all', 'canopy'), numMicros = 3){

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
  tryCatch(seeds_vw <- get("MicroplotSeedlings_NETN", envir = env) %>%
             select(Plot_Name, PlotID, EventID, SQSeedlingCode, SQSeedlingNotes, MicroplotCode,
                    TSN, ScientificName, Seedlings_15_30cm, Seedlings_30_100cm,
                    Seedlings_100_150cm, Seedlings_Above_150cm, SeedlingSizeNote),
           error = function(e){stop("MicroplotSeedlings_NETN view not found. Please import view.")})

  taxa_wide <- force(prepTaxa())

  # subset with EventID from plot_events to make function faster
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short')) %>%
                 select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
                        EventID, SampleYear, SampleDate, cycle, IsQAQC)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  pe_list <- unique(plot_events$EventID)

  seed_evs <- filter(seeds_vw, EventID %in% pe_list) %>%
              left_join(plot_events, ., by = intersect(names(plot_events), names(.)))

  seed_tax <- left_join(seed_evs,
                        taxa_wide[, c("TSN", "ScientificName", "CanopyExclusion", "Exotic",
                                      "InvasiveNETN")],
                        by = c("TSN", "ScientificName"))

  seed_tax$ScientificName[seed_tax$SQSeedlingCode == "NP"] <- "None present"
  seed_tax$Count[seed_tax$SQSeedlingCode == "NP"] <- 0

  # Create the left data.frame to join back to after filtering species types
  seed_left <- seed_tax %>% select(Plot_Name:MicroplotCode) %>% unique()

  seed_tax$ScientificName[seed_tax$SQSeedlingCode == "NS"] <- "Not Sampled"

  sd_cols <- c("Seedlings_15_30cm", "Seedlings_30_100cm", "Seedlings_100_150cm",
               "Seedlings_Above_150cm")

  seed_tax$tot_seeds = ifelse(!(seed_tax$SQSeedlingCode %in% c("ND", "NS")) &
                                 !is.na(seed_tax$ScientificName),
                               rowSums(seed_tax[, sd_cols], na.rm = T),
                               NA)

  seed_mic <- if(numMicros == 3) {seed_tax
       } else if(numMicros == 2) {filter(seed_tax, MicroplotCode %in% c('UR','B')) #randomly determined this
       } else if(numMicros == 1) {filter(seed_tax, MicroplotCode == "UR")}

  seed_can <- if(canopyForm == "canopy"){seed_mic %>% filter(CanopyExclusion == FALSE)
                } else {seed_mic}

  seed_nat <- switch(speciesType,
                     "all" = seed_can,
                     "native" = filter(seed_can, Exotic == FALSE),
                     "exotic" = filter(seed_can, Exotic == TRUE),
                     "invasive" = filter(seed_can, InvasiveNETN == TRUE)) %>%
              select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
                     PlotCode, PlotID, EventID, IsQAQC, SampleYear, cycle, SQSeedlingCode, MicroplotCode,
                     TSN, ScientificName, CanopyExclusion, Exotic, InvasiveNETN,
                     Seedlings_15_30cm, Seedlings_30_100cm, Seedlings_100_150cm,
                     Seedlings_Above_150cm, tot_seeds)

  # join filtered data back to full plot/visit/microplot list
  seed_comb <- left_join(seed_left, seed_nat, by = intersect(names(seed_left), names(seed_nat)))

  # Use SQs to fill blank ScientificNames after filtering
  seed_comb$ScientificName[is.na(seed_comb$ScientificName) &
                             (seed_comb$SQSeedlingCode %in% c("SS", "NP"))] = "None present"
  seed_comb$ScientificName[is.na(seed_comb$ScientificName) &
                             (seed_comb$SQSeedlingCode %in% c("ND", "NS"))] = "Not Sampled"

  #Fill NAs for None present
  seed_comb$Seedlings_15_30cm[(seed_comb$ScientificName == "None present") & is.na(seed_comb$Seedlings_15_30cm)] <- 0
  seed_comb$Seedlings_30_100cm[(seed_comb$ScientificName == "None present") & is.na(seed_comb$Seedlings_30_100cm)] <- 0
  seed_comb$Seedlings_100_150cm[(seed_comb$ScientificName == "None present") & is.na(seed_comb$Seedlings_100_150cm)] <- 0
  seed_comb$Seedlings_Above_150cm[(seed_comb$ScientificName == "None present") & is.na(seed_comb$Seedlings_Above_150cm)] <- 0
  seed_comb$tot_seeds[(seed_comb$ScientificName == "None present") & is.na(seed_comb$tot_seeds)] <- 0

  # Clean up filtered columns and NS
  seed_comb$Seedlings_15_30cm[(seed_comb$SQSeedlingCode == "NS")] <- NA_real_
  seed_comb$Seedlings_30_100cm[(seed_comb$SQSeedlingCode == "NS")] <- NA_real_
  seed_comb$Seedlings_100_150cm[(seed_comb$SQSeedlingCode == "NS")] <- NA_real_
  seed_comb$Seedlings_Above_150cm[(seed_comb$SQSeedlingCode == "NS")] <- NA_real_
  seed_comb$tot_seeds[(seed_comb$SQSeedlingCode == "NS")] <- NA_real_
  seed_comb$CanopyExclusion[seed_comb$SQSeedlingCode == "NS"] <- NA
  seed_comb$Exotic[seed_comb$SQSeedlingCode == "NS"] <- NA
  seed_comb$InvasiveNETN[seed_comb$SQSeedlingCode == "NS"] <- NA

  seed_final <- seed_comb %>% arrange(Plot_Name, SampleYear, IsQAQC, MicroplotCode, ScientificName)

  return(data.frame(seed_final))
} # end of function


