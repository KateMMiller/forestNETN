#' @include joinLocEvent.R
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
#' \item{"all"}{Returns all species, including low canopy species.}
#' \item{"canopy"}{Default. Returns canopy-forming species only}
#'}
#'
#' @param numMicros Allows you to select 1, 2, or 3 microplots of data to summarize
#'
#' @return returns a dataframe with seedling densities
#'
#' @examples
#' importCSV('./forest_csvs/')
#' # compile seedling data for all parks and all species in most cycle 3
#' regen_data <- joinMicroSeedlings(canopyForm = 'all', from = 2014, to = 2017)
#'
#' # compile seedling data for only canopy-forming (default) and native species in SAGA for all years
#' SAGA_regen <- joinMicroSeedlings(park = 'SAGA', speciesType = 'native')
#'
#' # compile only 1 microplot of data for ACAD native canopy-forming species for all but first year
#' ACAD_regen_m1 <- joinMicroSeedlings(park = 'ACAD', speciesType = 'native', numMicros = 1, from = 2007)
#'
#' @export
#'
#------------------------
# Joins microplot tables and filters by park, year, and plot/visit type
#------------------------
joinMicroSeedlings <- function(park = 'all', from = 2006, to = 2021, QAQC = FALSE, panels = 1:4,
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
  tryCatch(seeds_vw <- get("NETN_MicroplotSeedlings", envir = env) %>%
             select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, SQSeedlingCode,
                    MicroplotCode, TSN, ScientificName, SizeClassCode, SizeClassLabel, Count),
           error = function(e){stop("NETN_MicroplotSeedlings view not found. Please import view.")})

  taxa_wide <- force(prepTaxa())

  # subset with EventID from plot_events to make function faster
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short')) %>%
                 select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
                        xCoordinate, yCoordinate, EventID, StartDate, StartYear, cycle, IsQAQC)

  pe_list <- unique(plot_events$EventID)

  seed_evs <- filter(seeds_vw, EventID %in% pe_list) %>%
              left_join(plot_events, ., by = intersect(names(plot_events), names(.))) %>%
              filter(!(StartYear == 2006 & MicroplotCode %in% c("UL", "B"))) # drop quads not sampled in 2006

  seed_tax <- left_join(seed_evs,
                        taxa_wide[, c("TSN", "ScientificName", "CanopyExclusion", "Exotic",
                                      "InvasiveNETN")],
                        by = c("TSN", "ScientificName"))

  seed_tax$sizeclass <- NA
  seed_tax$sizeclass[seed_tax$SizeClassCode == 1] <- "sd_15_30cm"
  seed_tax$sizeclass[seed_tax$SizeClassCode == 2] <- "sd_30_100cm"
  seed_tax$sizeclass[seed_tax$SizeClassCode == 3] <- "sd_100_150cm"
  seed_tax$sizeclass[seed_tax$SizeClassCode == 4] <- "sd_p150cm"

  seed_tax$ScientificName[seed_tax$SQSeedlingCode == "NP"] <- "None present"
  seed_tax$Count[seed_tax$SQSeedlingCode == "NP"] <- 0

  seed_wide <- seed_tax %>% select(-SizeClassCode, - SizeClassLabel) %>%
                            pivot_wider(names_from = sizeclass,
                                        values_from = Count,
                                        values_fill = NA_real_)

  # Fill seedling size columns with 0, if their ScientificName isn't NA
  seed_wide$sd_15_30cm[(!is.na(seed_wide$ScientificName)) & is.na(seed_wide$sd_15_30cm)] <- 0
  seed_wide$sd_30_100cm[(!is.na(seed_wide$ScientificName)) & is.na(seed_wide$sd_30_100cm)] <- 0
  seed_wide$sd_100_150cm[(!is.na(seed_wide$ScientificName)) & is.na(seed_wide$sd_100_150cm)] <- 0
  seed_wide$sd_p150cm[(!is.na(seed_wide$ScientificName)) & is.na(seed_wide$sd_p150cm)] <- 0

  sd_cols <- c("sd_15_30cm", "sd_30_100cm", "sd_100_150cm", "sd_p150cm")
  seed_wide$tot_seeds = ifelse(!(seed_wide$SQSeedlingCode %in% c("ND", "NS")) &
                                 !is.na(seed_wide$ScientificName),
                               rowSums(seed_wide[, sd_cols], na.rm = T),
                               NA)

  seed_wide$CanopyExclusion[seed_wide$ScientificName == "None present"] <- FALSE
  seed_wide$Exotic[seed_wide$ScientificName == "None present"] <- FALSE
  seed_wide$InvasiveNETN[seed_wide$ScientificName == "None present"] <- FALSE

  seed_mic <- if(numMicros == 3) {seed_wide
       } else if(numMicros == 2) {filter(seed_wide, MicroplotCode %in% c('UR','B')) #randomly determined this
       } else if(numMicros == 1) {filter(seed_wide, MicroplotCode == "UR")}

  seed_can <- if(canopyForm == "canopy"){filter(seed_mic, CanopyExclusion == FALSE)
                } else {seed_mic}

  seed_nat <- switch(speciesType,
                     "all" = seed_can,
                     "native" = filter(seed_can, Exotic == FALSE),
                     "exotic" = filter(seed_can, Exotic == TRUE),
                     "invasive" = filter(seed_can, InvasiveNETN == TRUE)) %>%
              select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
                     PlotCode, PlotID, EventID, IsQAQC, StartYear, cycle, SQSeedlingCode, MicroplotCode,
                     TSN, ScientificName, CanopyExclusion, Exotic, InvasiveNETN,
                     sd_15_30cm, sd_30_100cm, sd_100_150cm, sd_p150cm, tot_seeds)


  # Find plot visits that were filtered out based on nummicros, canopy form or nativity to rbind with seed_nat
  # for full dataset
  exp_df <- data.frame(MicroplotCode = rep(c("UR", "UL", "B"), times = length(from:to)),
                       StartYear = rep(from:to, times = 3))

  visits <- plot_events %>% select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
                                   PlotCode, PlotID, EventID, IsQAQC, StartYear, cycle) %>% unique() %>%
                            filter(!(EventID %in% c(257, 710))) # dropping ACAD-029-2010 & SAGA-008-2008

  bad_visits <- seed_wide %>% filter(SQSeedlingCode %in% c("ND", "NS") |
                                    is.na(tot_seeds)) %>%
                  select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
                         PlotCode, PlotID, EventID, IsQAQC, StartYear, cycle, SQSeedlingCode, MicroplotCode,
                         TSN, ScientificName, CanopyExclusion, Exotic, InvasiveNETN,
                         sd_15_30cm, sd_30_100cm, sd_100_150cm, sd_p150cm, tot_seeds)

  # Need to properly add back in the visits with issues
  # First, none of the early species records without counts were exotic or invasive
  # ACAD-029-2010 (eID 710) and SAGA-008-2010 (eID 257) have SQs of ND (should be NS)

  exp_df2 <- full_join(visits, exp_df, by = "StartYear") %>%
             filter(!(StartYear == 2006 & MicroplotCode %in% c("UL", "B"))) %>%
             filter(!(EventID %in% bad_visits$EventID))

  seed_exp2 <- full_join(seed_nat %>% filter((!SQSeedlingCode %in% c("ND", "NS")) &
                                               (!is.na(tot_seeds))),
                         exp_df2, by = intersect(names(seed_nat), names(exp_df2)))

  seed_exp2$SQSeedlingCode[is.na(seed_exp2$SQSeedlingCode)] <- "NP"
  seed_exp2$ScientificName[is.na(seed_exp2$ScientificName)] = "None present"
  seed_exp2$CanopyExclusion[is.na(seed_exp2$CanopyExclusion)] = FALSE
  seed_exp2$Exotic[is.na(seed_exp2$Exotic)] = FALSE
  seed_exp2$InvasiveNETN[is.na(seed_exp2$InvasiveNETN)] = FALSE
  seed_exp2$sd_15_30cm[is.na(seed_exp2$sd_15_30cm)] <- 0
  seed_exp2$sd_30_100cm[is.na(seed_exp2$sd_30_100cm)] <- 0
  seed_exp2$sd_100_150cm[is.na(seed_exp2$sd_100_150cm)] <- 0
  seed_exp2$sd_p150cm[is.na(seed_exp2$sd_p150cm)] <- 0
  seed_exp2$tot_seeds[is.na(seed_exp2$tot_seeds)] <- 0

  seed_comb <- rbind(seed_exp2, bad_visits)
  seed_final <- seed_comb %>% arrange(Plot_Name, StartYear, IsQAQC, MicroplotCode, ScientificName)

  return(data.frame(seed_final))
} # end of function


