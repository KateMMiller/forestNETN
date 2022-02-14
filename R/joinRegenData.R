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
#' @param ... Other arguments passed to function.
#'
#' @return returns a dataframe with seedling and sapling densities, and stocking index metrics
#' for each species observed per visit.
#'
#' @examples
#' \dontrun{
#' importCSV('./forest_csvs/')
#' # compile seedling and sapling data for all parks and all species in cycle 3
#' regen_data <- joinRegenData(canopyForm = 'all', from = 2014, to = 2017)
#'
#' # compile regen data for only canopy-forming (default) and native species in SAGA for all years
#' SAGA_regen <- joinRegenData(park = 'SAGA', speciesType = 'native')
#'
#' # compile only 1 microplot of data for ACAD native canopy-forming species for all but first year
#' ACAD_regen_m1 <- joinRegenData(park = 'ACAD', speciesType = 'native', numMicros = 1, from = 2007)
#'}
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
               select(-tot_seeds)

  # Set up plots missing all seedling data and calculate number of microplots sampled
  num_samp_micros_seed <- seeds_raw %>% select(Plot_Name, StartYear, IsQAQC, EventID, SQSeedlingCode, MicroplotCode) %>%
                            unique() %>% mutate(samp_micro = ifelse(SQSeedlingCode %in% c("NP", "SS"), 1, 0),
                                                not_samp_micro = ifelse(SQSeedlingCode == "NS", 1, 0)) %>% #View() %>%
                            group_by(Plot_Name, StartYear, IsQAQC, EventID) %>%
                            summarize(num_micros_seed = sum(samp_micro),
                                      not_samp_events = ifelse(sum(not_samp_micro) == 3, 1, 0),
                                      .groups = 'drop')

  # Plots missing all seedling data
  not_samp_evs_seed <- num_samp_micros_seed %>% filter(not_samp_events == 1) %>%
    select(Plot_Name, StartYear, IsQAQC, EventID)

  # Number of microplots for visits not missing all seedling data
  num_micros_seeds <- num_samp_micros_seed %>% filter(not_samp_events != 1) %>%
    select(Plot_Name, StartYear, IsQAQC, EventID, num_micros_seed)

  seeds_long <- seeds_raw %>% filter(!SQSeedlingCode %in% c("NP", "NS")) %>% # drop b/c saplings might have species
                              select(-SQSeedlingCode) %>%
                              pivot_longer(cols = c(sd_15_30cm, sd_30_100cm, sd_100_150cm, sd_p150cm),
                                       names_to = "SizeClass",
                                       values_to = "Count")

 # Prepare the sapling data
  saps_raw <- joinMicroSaplings(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                               locType = locType, eventType = eventType, speciesType = speciesType,
                               canopyForm = canopyForm, numMicros = numMicros) %>%
              mutate(SizeClass = ifelse(DBHcm <= 2.5, "Sapling_SI", "Sapling"))

  # Set up plots missing all sapling data and calculate number of microplots sampled
  num_samp_micros_sap <- saps_raw %>% select(Plot_Name, StartYear, IsQAQC, EventID, SQSaplingCode, MicroplotCode) %>%
    unique() %>% mutate(samp_micro = ifelse(SQSaplingCode %in% c("NP", "SS"), 1, 0),
                        not_samp_micro = ifelse(SQSaplingCode == "NS", 1, 0)) %>% #View() %>%
    group_by(Plot_Name, StartYear, IsQAQC, EventID) %>%
    summarize(num_micros_sap = sum(samp_micro),
              not_samp_events = ifelse(sum(not_samp_micro) == 3, 1, 0),
              .groups = 'drop')

  # Plots missing all sapling data
  not_samp_evs_sap <- num_samp_micros_sap %>% filter(not_samp_events == 1) %>%
    select(Plot_Name, StartYear, IsQAQC, EventID)

  # Number of microplots for visits not missing all sapling data
  num_micros_saps <- num_samp_micros_sap %>% filter(not_samp_events != 1) %>%
    select(Plot_Name, StartYear, IsQAQC, EventID, num_micros_sap)

  # Summarize saplings before rbind
  sap_sum <- saps_raw %>% filter(!(SQSaplingCode %in% c("NP", "NS"))) %>%  # drop b/c saplings might have species
    select(-SQSaplingCode) %>%
    group_by(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
             PlotCode, PlotID, EventID, IsQAQC, StartYear, StartDate, cycle, MicroplotCode,
             TSN, ScientificName, CanopyExclusion, Exotic, InvasiveNETN, SizeClass)  %>%
    summarize(Count = sum(Count), .groups = 'drop')

  # Combine seedling and sapling data
  reg_long <- rbind(seeds_long, sap_sum) #%>% filter(!(ScientificName %in% c("Not present", "Not Sampled")))

  size_classes <- c("sd_15_30cm", "sd_30_100cm", "sd_100_150cm", "sd_p150cm", "Sapling", "Sapling_SI")

  reg_wide <- reg_long %>% pivot_wider(names_from = "SizeClass",
                                       values_from = "Count",
                                       values_fill = 0) #%>% select(-MicroplotCode)

  # Fixes for size classes not represented in filtered regen
  all_cols <- unique(c(names(reg_wide), size_classes))
  missing_cols <- setdiff(all_cols, names(reg_wide))
  reg_wide[missing_cols] <- 0

  # Add plot events left join
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short', ...)) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
           EventID, StartYear, StartDate, cycle, IsQAQC)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  reg_wide2 <- left_join(plot_events, reg_wide, by = intersect(names(plot_events), names(reg_wide)))

  # pull in NS and num_micros
  num_micros_comb <- full_join(num_micros_seeds, num_micros_saps,
                               by = intersect(names(num_micros_seeds), names(num_micros_saps)))

  reg_wide3 <- full_join(reg_wide2, num_micros_comb, by = intersect(names(reg_wide2), names(num_micros_comb)))

  reg_wide3$ScientificName[is.na(reg_wide3$ScientificName) &
                             !is.na(reg_wide3$num_micros_seed) &
                             !is.na(reg_wide3$num_micros_sap)] <- "None present"
  reg_wide3[, size_classes][is.na(reg_wide3[, size_classes])] <- 0

  not_samp_evs <- full_join(not_samp_evs_seed, not_samp_evs_sap, c("Plot_Name", "StartYear", "IsQAQC", "EventID"))
  reg_wide3[, size_classes][reg_wide3$EventID %in% not_samp_evs$EventID,] <- NA_real_ # These are the NS events
  reg_wide3$ScientificName[reg_wide3$EventID %in% not_samp_evs$EventID] <- "Not Sampled"

  # Summarise data at plot level. We lose the Microplot name, but average over # microplots selected in next step
  reg_sum <- reg_wide3 %>% group_by(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
                                    PlotCode, PlotID, EventID, IsQAQC, StartYear, StartDate, cycle,
                                    TSN, ScientificName, CanopyExclusion, Exotic, InvasiveNETN) %>%
                           summarize(num_micros_seed = first(num_micros_seed),
                                     num_micros_sap = first(num_micros_sap),
                                     seed_15_30cm = sum(sd_15_30cm, na.rm = T),
                                     seed_30_100cm = sum(sd_30_100cm, na.rm = T),
                                     seed_100_150cm = sum(sd_100_150cm, na.rm = T),
                                     seed_p150cm = sum(sd_p150cm, na.rm = T),
                                     sap_stems = sum(Sapling, na.rm = T) + sum(Sapling_SI, na.rm = T),
                                     sap_stems_SI = sum(Sapling_SI, na.rm = T),
                                     .groups = 'drop')

  diff_micros <- reg_sum %>% filter(num_micros_seed != num_micros_sap) %>%
    select(Plot_Name, StartYear, IsQAQC, num_micros_seed, num_micros_sap) %>% unique()

  if(nrow(diff_micros) > 0){warning(
    paste0("There following visits have differing numbers of seedling and sapling microplots. Stocking index based on sapling microplots: \n"),
    paste0(paste(capture.output(data.frame(diff_micros)), collapse = "\n")))}

  reg_stock <- reg_sum %>% mutate(stock = ((1*seed_15_30cm) + (2*seed_30_100cm) + (20*seed_100_150cm) +
                                            (50*seed_p150cm) + (50*sap_stems_SI))/num_micros_sap,
                                  seed_15_30cm = seed_15_30cm/num_micros_seed,
                                  seed_30_100cm = seed_30_100cm/num_micros_seed,
                                  seed_100_150cm = seed_100_150cm/num_micros_seed,
                                  seed_p150cm = seed_p150cm/num_micros_seed,
                                  seed_den = ((seed_15_30cm + seed_30_100cm + seed_100_150cm +
                                                seed_p150cm)), # already/num_micros
                                  sap_den = sap_stems/num_micros_seed,
                                  sap_den_SI = sap_stems_SI/num_micros_seed,
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

  #table(complete.cases(reg_units$ScientificName)) # All T
  reg_final <- reg_units %>% arrange(Plot_Name, StartYear, IsQAQC, ScientificName)

  return(data.frame(reg_final))
} # end of function

