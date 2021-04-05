#' @include joinLocEvent.R
#' @include joinMicroSaplings.R
#' @include joinMicroSeedlings.R
#' @title joinRegenData: compiles seedling and sapling data
#'
#' @importFrom dplyr  anti_join left_join filter select
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @description This function combines seedling and sapling data collected in microplots, and
#' calculates the stocking index. Each row represents a species observed per visit. If no seedlings
#' or saplings were observed, function returns "None present" for ScientificName and 0 for densities.
#' If a record has a blank ScientificName and associated data, it means it's a missing value. These are
#' rare, but mostly occur in data <2011. Note that the stocking index only includes saplings < 2.5cm DBH,
#' but the sapling density returned is all saplings > 1cm and <10cm DBH. Must run importData first.
#'
#'#' @param park Combine data from all parks or one or more parks at a time. Valid inputs:
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
#' @param units Calculates seedling and sapling densities based on different units.
#' \describe{
#' \item{"micro"}{Default. Returns seedling and sapling densities per microplot.}
#' \item{"ha"}{Returns seedling and sapling densities per hectare}
#' \item{"acres"}{Returns densities per acre}
#'}
#'
#' @return returns a dataframe with seedling and sapling densities, and stocking index metrics
#' for each species observed per visit.
#'
#' @examples
#' importCSV('./forest_csvs/')
#' # compile seedling and sapling data for all parks and all species in cycle 3
#' regen_data <- joinRegenData(canopyForm = 'all', from = 2014, to = 2017)
#'
#' # compile regen data for only canopy-forming (default) and native species in SAGA for all years
#' SAGA_regen <- joinRegenData(park = 'SAGA', speciesType = 'native')
#'
#' # compile only 1 microplot of data for ACAD native canopy-forming species for all but first year
#' ACAD_regen_m1 <- joinRegenData(park = 'ACAD', speciesType = 'native', numMicros = 1, from = 2007)
#'
#' @export
#'
#------------------------
# Joins microplot tables and filters by park, year, and plot/visit type
#------------------------
joinRegenData <- function(park = 'all', from = 2006, to = 2021, QAQC = FALSE, panels = 1:4,
                          locType = c('VS', 'all'), eventType = c('complete', 'all'),
                          speciesType = c('all', 'native', 'exotic', 'invasive'),
                          canopyForm = c('canopy', 'all'), numMicros = 3,
                          units = c("micro", "ha", "acres"), ...){

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
  units <- match.arg(units)

  # Prepare the seedling data
  seeds <- joinMicroSeedlings(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                              locType = locType, eventType = eventType, speciesType = speciesType,
                              canopyForm = canopyForm, numMicros = numMicros) %>%
           select(-tot_seeds, -SQSeedlingCode) %>% filter(!ScientificName %in% "None present")

  seeds_long <- seeds %>% pivot_longer(cols = c(sd_15_30cm, sd_30_100cm, sd_100_150cm, sd_p150cm),
                                       names_to = "SizeClass",
                                       values_to = "Count")

 # Prepare the sapling data
  saps <- joinMicroSaplings(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                            locType = locType, eventType = eventType, speciesType = speciesType,
                            canopyForm = canopyForm, numMicros = numMicros) %>%
          select(-SQSaplingCode) %>% filter(!ScientificName %in% "None present") %>%
          mutate(SizeClass = ifelse(DBHcm <= 2.5, "Sapling_SI", "Sapling"))

  sap_sum <- saps %>% group_by(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
                               PlotCode, PlotID, EventID, IsQAQC, StartYear, cycle, MicroplotCode,
                               TSN, ScientificName, CanopyExclusion, Exotic, InvasiveNETN, SizeClass)  %>%
                      summarize(Count = sum(Count), .groups = 'drop')


  reg_long <- rbind(seeds_long, sap_sum)

  reg_wide <- reg_long %>% pivot_wider(names_from = "SizeClass",
                                       values_from = "Count",
                                       values_fill = NA_real_)

  # Fill 0s for plots without issues. In this case, if the ScientificName isn't NA, 0s are safe.
  reg_wide$sd_15_30cm[(!is.na(reg_wide$ScientificName)) & is.na(reg_wide$sd_15_30cm)] <- 0
  reg_wide$sd_30_100cm[(!is.na(reg_wide$ScientificName)) & is.na(reg_wide$sd_30_100cm)] <- 0
  reg_wide$sd_100_150cm[(!is.na(reg_wide$ScientificName)) & is.na(reg_wide$sd_100_150cm)] <- 0
  reg_wide$sd_p150cm[(!is.na(reg_wide$ScientificName)) & is.na(reg_wide$sd_p150cm)] <- 0
  reg_wide$Sapling[(!is.na(reg_wide$ScientificName)) & is.na(reg_wide$Sapling)] <- 0
  reg_wide$Sapling_SI[(!is.na(reg_wide$ScientificName)) & is.na(reg_wide$Sapling_SI)] <- 0

  # Summarise data at plot level. We lose the Microplot name, but average over # microplots selected in next step
  reg_sum <- reg_wide %>% group_by(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
                                   PlotCode, PlotID, EventID, IsQAQC, StartYear, cycle,
                                   TSN, ScientificName, CanopyExclusion, Exotic, InvasiveNETN) %>%
                          summarize(num_micros = ifelse(StartYear == 2006, 1, numMicros),
                                    seed_15_30cm = sum(sd_15_30cm), # leaving na.rm = F, so problem plots return NA
                                    seed_30_100cm = sum(sd_30_100cm),
                                    seed_100_150cm = sum(sd_100_150cm),
                                    seed_p150cm = sum(sd_p150cm),
                                    sap_stems = sum(Sapling),
                                    sap_stems_SI = sum(Sapling_SI),
                                    .groups = 'drop')

  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short')) %>%
                 select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
                        EventID, StartDate, StartYear, cycle, IsQAQC)

  missing_evs <- anti_join(plot_events, reg_sum, by = intersect(names(plot_events), names(reg_sum))) %>% select(-StartDate)
  missing_evs <- missing_evs %>% mutate(TSN = NA,
                                        ScientificName = "None present",
                                        CanopyExclusion = FALSE,
                                        Exotic = FALSE,
                                        InvasiveNETN = FALSE,
                                        num_micros = ifelse(StartYear == 2006, 1, 3),
                                        seed_15_30cm = 0,
                                        seed_30_100cm = 0,
                                        seed_100_150cm = 0,
                                        seed_p150cm = 0,
                                        sap_stems = 0,
                                        sap_stems_SI = 0)

  reg_all <- rbind(reg_sum, missing_evs)

  reg_stock <- reg_all %>% mutate(stock = ((1*seed_15_30cm) + (2*seed_30_100cm) + (20*seed_100_150cm) +
                                            (50* seed_p150cm) + (50*sap_stems_SI))/num_micros,
                                  seed_15_30cm = seed_15_30cm/num_micros,
                                  seed_30_100cm = seed_30_100cm/num_micros,
                                  seed_100_150cm = seed_100_150cm/num_micros,
                                  seed_p150cm = seed_p150cm/num_micros,
                                  seed_den = ((seed_15_30cm + seed_30_100cm + seed_100_150cm +
                                                seed_p150cm))/ num_micros,
                                  sap_den = sap_stems/num_micros,
                                  regen_den = (seed_den + sap_den))

  reg_units <- switch(units,
                      "micro" = reg_stock,
                      "ha" = reg_stock %>%
                          mutate(seed_15_30cm = (seed_15_30cm * 10000)/(pi*4),
                                 seed_30_100cm = (seed_30_100cm * 10000)/(pi*4),
                                 seed_100_150cm = (seed_100_150cm * 10000)/(pi*4),
                                 seed_p150cm = (seed_p150cm * 10000)/(pi*4),
                                 seed_den = (seed_den * 10000)/(pi*4),
                                 sap_den = (sap_den * 10000)/(pi*4),
                                 regen_den = (rege_den * 10000)/(pi*4)),
                      'acres' = reg_stock %>%
                          mutate(seed_15_30cm = (seed_15_30cm * 4046.856)/(pi*4),
                                 seed_30_100cm = (seed_30_100cm * 4046.856)/(pi*4),
                                 seed_100_150cm = (seed_100_150cm * 4046.856)/(pi*4),
                                 seed_p150cm = (seed_p150cm * 4046.856)/(pi*4),
                                 seed_den = (seed_den * 4046.856)/(pi*4),
                                 sap_den = (sap_den * 4046.856)/(pi*4),
                                 regen_den = (rege_den * 4046.856)/(pi*4))
  )


  reg_final <- reg_units %>% arrange(Plot_Name, StartYear, IsQAQC, ScientificName)

  return(reg_final)
} # end of function

