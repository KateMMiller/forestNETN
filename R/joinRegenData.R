#' @include joinLocEvent.R
#' @include joinMicroSaplings.R
#' @include joinMicroSeedlings.R
#'
#' @title joinRegenData: compiles seedling and sapling data
#'
#' @importFrom dplyr  anti_join left_join filter select
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @description This function combines seedling and sapling data collected in microplots, and
#' calculates the stocking index. Each row represents a species observed per visit. If no seedlings
#' or saplings were observed, function returns "None present" for ScientificName and 0 for densities.
#' Permanently Missing species tallies for species or whole plots have NAs. These are rare, have always been
#' low canopy species initially recorded as shrubs, and mostly occur in data <2011. Note that the stocking
#' index only includes saplings < 2.5cm DBH, but the sapling density returned is all saplings > 1cm and
#' <10cm DBH. For the few plots with > 10 saplings of a given species in a microplot, their counts are
#' included in the stocking index only if the average DBH of saplings measured is <=2.5 cm. This may
#' underestimate the stocking index for those plots, but their index values are still way higher than
#' most plots. Must run importData first.
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
#' @param units Calculates seedling and sapling densities based on different units.
#' \describe{
#' \item{"micro"}{Default. Returns seedling and sapling densities per microplot.}
#' \item{"sq.m"}{Returns seedling and sapling densities per square meter}
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
                          canopyForm = c('all', 'canopy'), numMicros = 3,
                          units = c("micro", "sq.m", "ha", "acres"), ...){

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
  seeds_raw <- joinMicroSeedlings(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                                  locType = locType, eventType = eventType, speciesType = speciesType,
                                  canopyForm = canopyForm, numMicros = numMicros) %>%
               select(-tot_seeds) %>% filter(!ScientificName %in% "None present")

  not_sampled_sds <- seeds_raw %>% filter(SQSeedlingCode %in% c("ND", "NS")) %>% select(EventID)

  seeds_long <- seeds_raw %>% select(-SQSeedlingCode) %>%
                              pivot_longer(cols = c(sd_15_30cm, sd_30_100cm, sd_100_150cm, sd_p150cm),
                                       names_to = "SizeClass",
                                       values_to = "Count")

 # Prepare the sapling data
  saps_raw <- joinMicroSaplings(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                               locType = locType, eventType = eventType, speciesType = speciesType,
                               canopyForm = canopyForm, numMicros = numMicros) %>%
              filter(!ScientificName %in% "None present") %>%
              mutate(SizeClass = ifelse(DBHcm <= 2.5, "Sapling_SI", "Sapling"))

  not_sampled_saps <- saps_raw %>% filter(SQSaplingCode %in% c("ND", "NS")) %>% select(EventID)

  sap_sum <- saps_raw %>% select(-SQSaplingCode) %>%
                          group_by(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
                               PlotCode, PlotID, EventID, IsQAQC, StartYear, StartDate, cycle, MicroplotCode,
                               TSN, ScientificName, CanopyExclusion, Exotic, InvasiveNETN, SizeClass)  %>%
                          summarize(Count = sum(Count), .groups = 'drop')


  reg_long <- rbind(seeds_long, sap_sum)

  reg_wide <- reg_long %>% pivot_wider(names_from = "SizeClass",
                                       values_from = "Count",
                                       values_fill = NA_real_) %>% select(-MicroplotCode)

  # Helps to add all events back for MIDN (didn't need this for NETN)
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short')) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
           EventID, StartYear, StartDate, cycle, IsQAQC)

  reg_wide2 <- left_join(plot_events, reg_wide, by = intersect(names(plot_events), names(reg_wide)))

  # Fill 0s for plots without issues using the not_sampled_evs
  # Also safe because we counted the number of quadrats and microplots already. Some 0s
  # might be here that shouldn't, but the summary metrics will be correct
  not_sampled_evs <- unique(rbind(not_sampled_sds, not_sampled_saps))

  reg_wide2$sd_15_30cm[(!reg_wide2$EventID %in% not_sampled_evs$EventID) & is.na(reg_wide2$sd_15_30cm)] <- 0
  reg_wide2$sd_30_100cm[(!reg_wide2$EventID %in% not_sampled_evs$EventID) & is.na(reg_wide2$sd_30_100cm)] <- 0
  reg_wide2$sd_100_150cm[(!reg_wide2$EventID %in% not_sampled_evs$EventID) & is.na(reg_wide2$sd_100_150cm)] <- 0
  reg_wide2$sd_p150cm[(!reg_wide2$EventID %in% not_sampled_evs$EventID) & is.na(reg_wide2$sd_p150cm)] <- 0
  reg_wide2$Sapling[(!reg_wide2$EventID %in% not_sampled_evs$EventID)  & is.na(reg_wide2$Sapling)] <- 0
  reg_wide2$Sapling_SI[(!reg_wide2$EventID %in% not_sampled_evs$EventID) & is.na(reg_wide2$Sapling_SI)] <- 0
  reg_wide2$ScientificName[(!reg_wide2$EventID %in% not_sampled_evs$EventID)
                           & is.na(reg_wide2$ScientificName)] <- "None present"
  reg_wide2$ScientificName[(reg_wide2$EventID %in% not_sampled_evs$EventID)
                           & is.na(reg_wide2$ScientificName)] <- "Not Sampled"
  reg_wide2$num_micros <- ifelse(reg_wide2$StartYear == 2006, 1, numMicros)

  # Summarise data at plot level. We lose the Microplot name, but average over # microplots selected in next step
  reg_sum <- reg_wide2 %>% group_by(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
                                    PlotCode, PlotID, EventID, IsQAQC, StartYear, StartDate, cycle,
                                    TSN, ScientificName, CanopyExclusion, Exotic, InvasiveNETN) %>%
                           summarize(num_micros = first(num_micros),
                                     seed_15_30cm = sum(sd_15_30cm), # leaving na.rm = F, so problem plots return NA
                                     seed_30_100cm = sum(sd_30_100cm),
                                     seed_100_150cm = sum(sd_100_150cm),
                                     seed_p150cm = sum(sd_p150cm),
                                     sap_stems = sum(Sapling) + sum(Sapling_SI),
                                     sap_stems_SI = sum(Sapling_SI),
                                     .groups = 'drop')

  #length(unique(reg_sum$EventID)) #1280

  reg_stock <- reg_sum %>% mutate(stock = ((1*seed_15_30cm) + (2*seed_30_100cm) + (20*seed_100_150cm) +
                                            (50*seed_p150cm) + (50*sap_stems_SI))/num_micros,
                                  seed_15_30cm = seed_15_30cm/num_micros,
                                  seed_30_100cm = seed_30_100cm/num_micros,
                                  seed_100_150cm = seed_100_150cm/num_micros,
                                  seed_p150cm = seed_p150cm/num_micros,
                                  seed_den = ((seed_15_30cm + seed_30_100cm + seed_100_150cm +
                                                seed_p150cm))/num_micros,
                                  sap_den = sap_stems/num_micros,
                                  sap_den_SI = sap_stems_SI/num_micros,
                                  regen_den = (seed_den + sap_den)) %>% select(-sap_stems, -sap_stems_SI)

  reg_units <- switch(units,
                      "micro" = reg_stock,
                      "sq.m" = reg_stock %>%
                          mutate(seed_15_30cm = (seed_15_30cm)/(pi*4),
                                 seed_30_100cm = (seed_30_100cm)/(pi*4),
                                 seed_100_150cm = (seed_100_150cm)/(pi*4),
                                 seed_p150cm = (seed_p150cm)/(pi*4),
                                 seed_den = (seed_den)/(pi*4),
                                 sap_den = (sap_den)/(pi*4),
                                 sap_den_SI = (sap_den_SI)/(pi*4),
                                 regen_den = (regen_den)/(pi*4)),
                      "ha" = reg_stock %>%
                          mutate(seed_15_30cm = (seed_15_30cm * 10000)/(pi*4),
                                 seed_30_100cm = (seed_30_100cm * 10000)/(pi*4),
                                 seed_100_150cm = (seed_100_150cm * 10000)/(pi*4),
                                 seed_p150cm = (seed_p150cm * 10000)/(pi*4),
                                 seed_den = (seed_den * 10000)/(pi*4),
                                 sap_den = (sap_den * 10000)/(pi*4),
                                 sap_den_SI = (sap_den_SI * 10000)/(pi*4),
                                 regen_den = (regen_den * 10000)/(pi*4)),
                      'acres' = reg_stock %>%
                          mutate(seed_15_30cm = (seed_15_30cm * 4046.856)/(pi*4),
                                 seed_30_100cm = (seed_30_100cm * 4046.856)/(pi*4),
                                 seed_100_150cm = (seed_100_150cm * 4046.856)/(pi*4),
                                 seed_p150cm = (seed_p150cm * 4046.856)/(pi*4),
                                 seed_den = (seed_den * 4046.856)/(pi*4),
                                 sap_den = (sap_den * 4046.856)/(pi*4),
                                 sap_den_SI = (sap_den_SI * 4046.856)/(pi*4),
                                 regen_den = (regen_den * 4046.856)/(pi*4))
  )

  cols_to_NA <- c("num_micros", "seed_15_30cm", "seed_30_100cm", "seed_100_150cm", "seed_p150cm",
                  "stock", "seed_den", "sap_den", "sap_den_SI", "regen_den")

  reg_units[reg_units$ScientificName == "Permanently Missing", cols_to_NA] <- NA

  reg_final <- reg_units %>% arrange(Plot_Name, StartYear, IsQAQC, ScientificName)

  return(data.frame(reg_final))
} # end of function

