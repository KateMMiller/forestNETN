#' @include joinLocEvent.R
#' @include joinAdditionalSpecies.R
#' @include joinMicroShrubData.R
#' @include joinQuadSpecies.R
#' @include joinRegenData.R
#' @include joinTreeData.R
#' @include joinTreeVineSpecies.R
#' @include prepTaxa.R
#'
#' @title sumSpeciesList: summarize a species list for each plot visit
#'
#' @importFrom dplyr arrange group_by filter full_join left_join select summarize
#' @importFrom magrittr %>%
#' @importFrom purrr reduce
#'
#' @description This function summarizes all species data collected in a plot visit, including live trees,
#' microplots, quadrats, and additional species lists.
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
#' @param numMicros Allows you to select 1, 2, or 3 microplots of data to summarize
#'
#' @param ... Other arguments passed to function.
#'
#' @return Returns a dataframe with species list for each plot.
#'
#' @examples
#' \dontrun{
#' importData()
#'
#' # Compile number of invasive species found per plot cycle 3 recent survey for all parks
#' inv_spp <- sumSppList(speciesType = 'invasive', from = 2015, to = 2018)
#' inv_spp$present <- ifelse(is.na(inv_spp$ScientificName), 0, 1)
#' num_inv_per_plot <- inv_spp %>% group_by(Plot_Name) %>% summarize(numspp = sum(present, na.rm = T))
#'
#' # Compile species list for SARA in 2019
#' SARA_spp <- sumSppList(park = 'SARA', from = 2019)
#'}
#'
#' @export
#'
#------------------------
# Joins quadrat tables and filters by park, year, and plot/visit type
#------------------------
sumSpeciesList <- function(park = 'all', from = 2006, to = as.numeric(format(Sys.Date(), "%Y")),
                           QAQC = FALSE, panels = 1:4,
                        locType = c('VS', 'all'), eventType = c('complete', 'all'),
                        numMicros = 3,
                        speciesType = c('all', 'native', 'exotic', 'invasive'),...){

  # Match args and class
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA"))
  stopifnot(class(from) == "numeric", from >= 2006)
  stopifnot(class(to) == "numeric", to >= 2006)
  stopifnot(class(QAQC) == 'logical')
  stopifnot(panels %in% c(1, 2, 3, 4))
  locType <- match.arg(locType)
  eventType <- match.arg(eventType)
  speciesType <- match.arg(speciesType)

  options(scipen = 100)

  # Set up data
  arglist <- list(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                  locType = locType, eventType = eventType)

  plot_events <-   plot_events <- joinLocEvent(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                                               locType = locType, eventType = eventType, output = 'verbose', ...) %>%
                   select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
                          EventID, SampleYear, SampleDate, cycle, IsQAQC)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  taxa_wide <- prepTaxa()

  # Trees
  tree_spp <- do.call(joinTreeData, c(arglist, list(status = 'live', speciesType = speciesType)))

  tree_sum <- tree_spp %>% group_by(Plot_Name, PlotID, EventID, IsQAQC, SampleYear, TSN, ScientificName) %>%
                           summarize(BA_cm2 = sum(BA_cm2, na.rm = TRUE),
                                     DBH_mean = mean(DBHcm, na.rm = TRUE),
                                     tree_stems = sum(num_stems),
                                     .groups = 'drop') %>%
                           filter(ScientificName != "None present")
  # Regen
  regen_spp <- do.call(joinRegenData,
                       c(arglist, list(canopyForm = "all", speciesType = speciesType, numMicros = numMicros)))

  regen_sum <- regen_spp %>% select(Plot_Name, PlotID, EventID, IsQAQC, SampleYear, TSN, ScientificName, seed_den,
                                    sap_den, stock) %>%
                             filter(ScientificName != "None present")
  # Shrubs
  shrubs <- do.call(joinMicroShrubData, c(arglist, list(speciesType = speciesType, valueType = 'midpoint')))

  shrub_sum <- shrubs %>% select(Plot_Name, PlotID, EventID, IsQAQC, SampleYear,
                                 TSN, ScientificName, shrub_avg_cov, shrub_pct_freq) %>%
                          filter(ScientificName != "None present")

  # Quad species without germinants
  quadspp <- suppressWarnings(do.call(joinQuadSpecies,
                                      c(arglist, list(speciesType = speciesType,
                                                      valueType = 'averages',
                                                      returnNoCover = TRUE)))
  )
  quad_sum <- quadspp %>% filter(IsGerminant == 0) %>%
                          select(Plot_Name, PlotID, EventID, IsQAQC, SampleYear, TSN,
                                 ScientificName, quad_avg_cov, quad_pct_freq) %>%
                          filter(ScientificName != "None present")

  # Additional Species
  addspp <- do.call(joinAdditionalSpecies, c(arglist, list(speciesType = speciesType)))

  addspp_sum <- addspp %>% select(Plot_Name, PlotID, EventID, IsQAQC, SampleYear, TSN,
                                  ScientificName, addspp_present) %>%
                           filter(ScientificName != "None present")

  sppdata_list <- list(tree_sum, regen_sum, shrub_sum, quad_sum, addspp_sum)

  spp_comb <- sppdata_list %>% reduce(full_join,
                                      by = c("Plot_Name", "PlotID", "EventID", "IsQAQC",
                                             "SampleYear", "TSN", "ScientificName"))
  # For the 22 species that were recorded in quadrats but had 0 cover for all quadrats,
  # change addspp_present to 1
  # table(spp_comb$quad_pct_freq, spp_comb$addspp_present)
  spp_comb$addspp_present[spp_comb$quad_pct_freq == 0] <- 1


  spp_evs <- left_join(plot_events,
                       spp_comb, by = intersect(names(plot_events), names(spp_comb)))

  spp_evs$ScientificName[is.na(spp_evs$ScientificName)] <- "None present"

  na_cols <- c("BA_cm2", "DBH_mean", "tree_stems", "sap_den", "seed_den", "stock",
               "shrub_avg_cov", "shrub_pct_freq",
               "quad_avg_cov", "quad_pct_freq", "addspp_present")

  spp_evs[, na_cols][is.na(spp_evs[, na_cols])] <- 0

  spp_final <- spp_evs %>% arrange(Plot_Name, SampleYear, IsQAQC, ScientificName)

  return(data.frame(spp_final))

  } # end of function
