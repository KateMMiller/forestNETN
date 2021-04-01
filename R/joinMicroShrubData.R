#' @include joinLocEvent.R
#' @include prepTaxa.R
#'
#' @title joinMicroShrubData: compiles shrub data collected in microplots
#'
#' @importFrom dplyr between case_when filter left_join mutate select
#' @importFrom magrittr %>%
#'
#' @description This function compiles shrub data collected in the microplots. Note that in 2006, only 1 microplot
#' was sampled. Note that from 2006 to 2009, stem tallies were recorded instead of % cover. For records < 2010,
#' microplot percent frequency is summarized by species, but average percent cover is NA. Must run importData() first.
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
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' \item{"invasive"}{Returns species on the Indicator Invasive List}
#' }
#'
#' @param valueType Allows you to return cover class midpoints (numeric) or cover class ranges (text)
#' \describe{
#' \item{"all"}{Default. Returns columns for midpoint and cover classes for each quad}
#' \item{"midpoint"}{Default. Returns numeric cover class midpoints, with Pct prefix, for each
#' microplot/species combination.}
#' \item{"classes"}{Returns the text cover class definitions, with Txt prefix, for each microplot/
#' species combination.}
#' \item{"averages"}{Returns only the plot-level average cover and percent frequency per species.}
#' }
#'
#' @return returns a dataframe with shrub data collected in microplots
#'
#' @examples
#' importData()
#' # native shrubs in MORR all years
#' native_shrubs <- joinMicroShrubData(park ='MORR', speciesType = 'native')
#'
#' # all parks with exotic shrubs in most recent survey
#' exotic_shrubs <- joinMicroShrubData(from = 2015, to = 2018, speciesType = 'exotic')
#'
#' @export
#'
#------------------------
# Joins microplot tables and filters by park, year, and plot/visit type
#------------------------
joinMicroShrubData <- function(park = 'all', from = 2006, to = 2021, QAQC = FALSE, panels = 1:4,
                               locType = c('VS', 'all'), eventType = c('complete', 'all'),
                               speciesType = c('all', 'native', 'exotic', 'invasive'),
                               valueType = c('all', 'midpoint', 'classes', 'averages'), ...){

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
  valueType <- match.arg(valueType)

  options(scipen = 100) # for TSNs

  env <- if(exists("VIEWS_NETN")){VIEWS_NETN} else {.GlobalEnv}

  # Prepare the quadrat data
  tryCatch(shrubs <- get("COMN_MicroplotShrubs", envir = env) %>%
             select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, SQShrubCode,
                    MicroplotCode, TSN, ScientificName, CoverClassCode, CoverClassLabel, SQShrubNotes, ShrubNote),
           error = function(e){stop("COMN_MicroplotShrubs view not found. Please import view.")})

  taxa_wide <- force(prepTaxa())

  # subset with EventID from plot_events to make function faster
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short')) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
           xCoordinate, yCoordinate, EventID, StartDate, StartYear, cycle, IsQAQC)

  pe_list <- unique(plot_events$EventID)

  shrub_evs <- filter(shrubs, EventID %in% pe_list) %>%
               left_join(plot_events, .,
                         by = intersect(names(plot_events), names(.))) %>%
               filter(!(StartYear == 2006 & MicroplotCode %in% c("UL", "B"))) # drop quads not sampled in 2006

  shrub_tax <- left_join(shrub_evs,
                         taxa_wide[, c("TSN", "ScientificName", "Exotic", "InvasiveNETN", "Shrub", "Vine")],
                         by = c("TSN", "ScientificName"))

  shrub_filt <- switch(speciesType,
                       'native' = filter(shrub_tax, Exotic == FALSE),
                       'exotic' = filter(shrub_tax, Exotic == TRUE),
                       'invasive' = filter(shrub_tax, InvasiveNETN == TRUE),
                       'all' = shrub_tax)


  shrub_mic1 <- shrub_filt %>% mutate(Pct_Cov1 = case_when(CoverClassCode == "1" ~ 0.1,
                                                         CoverClassCode == "2" ~ 3,
                                                         CoverClassCode == "3" ~ 7.5,
                                                         CoverClassCode == "4" ~ 17.5,
                                                         CoverClassCode == "5" ~ 37.5,
                                                         CoverClassCode == "6" ~ 62.5,
                                                         CoverClassCode == "7" ~ 85,
                                                         CoverClassCode == "8" ~ 97.5,
                                                         CoverClassCode == "PM" ~ NA_real_,
                                                         TRUE ~ 0),
                                    Pct_Cov = case_when(StartYear < 2010 ~ NA_real_,
                                                         StartYear >= 2010 & SQShrubCode %in% c("NS", "ND") ~ NA_real_,
                                                         TRUE ~ Pct_Cov1),
                                    Txt_Cov = case_when(between(StartYear, 2006, 2009) ~ paste("Not Collected"),
                                                         StartYear > 2009 & SQShrubCode == "NP" ~ "0%",
                                                         StartYear > 2009 & SQShrubCode %in% c("NS", "ND") ~ "Not Sampled",
                                                         TRUE ~ paste(CoverClassLabel)),
                                    ScientificName = ifelse(SQShrubCode == "NP" & is.na(ScientificName),
                                                            "None present", ScientificName)) %>%
                                    select(-Pct_Cov1, -ShrubNote)

  shrub_mic1$Txt_Cov <- ifelse(shrub_mic1$Txt_Cov == "-<1%", "<1%", shrub_mic1$Txt_Cov)

  # table(shrub_mic1$StartYear, shrub_mic1$Pct_Cov, useNA = 'always')
  # table(shrub_mic1$StartYear, shrub_mic1$Txt_Cov, useNA = 'always')

  shrub_wide <- shrub_mic1 %>% select(-SQShrubCode, -SQShrubNotes, -CoverClassCode, -CoverClassLabel) %>%
                               pivot_wider(names_from = "MicroplotCode",
                                           values_from = c("Pct_Cov", "Txt_Cov"))


  shrub_pres <- shrub_mic1 %>% group_by(PlotID, EventID, TSN, ScientificName) %>%
                               summarize(num_pres = sum(!is.na(MicroplotCode)),
                                         .groups = 'drop')

  micro_samp <- shrub_mic1 %>% select(PlotID, EventID, MicroplotCode) %>% unique() %>%
    group_by(PlotID, EventID) %>% summarize(num_micros = n(), .groups = 'drop')

  shrub_comb1 <- left_join(shrub_wide, shrub_pres, by = intersect(names(shrub_wide), names(shrub_pres)))

  shrub_comb2 <- shrub_comb1 %>% left_join(., micro_samp, by = intersect(names(.), names(micro_samp)))

  shrub_comb3 <- shrub_comb2 %>%
                   mutate(
                          Pct_Cov_UR = case_when(is.na(Pct_Cov_UR) & num_micros == 3 & StartYear > 2009 ~ 0,
                                                 TRUE ~ Pct_Cov_UR),
                          Pct_Cov_B = case_when(is.na(Pct_Cov_B) & num_micros == 3 & StartYear > 2009 ~ 0,
                                                TRUE ~ Pct_Cov_B),
                          Pct_Cov_UL = case_when(is.na(Pct_Cov_UL) & num_micros == 3 & StartYear > 2009 ~ 0,
                                                 TRUE ~ Pct_Cov_UL),

                          Txt_Cov_UR = case_when(is.na(Txt_Cov_UR) & num_micros >= 1 &
                                                   between(StartYear, 2006, 2009) ~ "Not Collected",
                                                 is.na(Txt_Cov_UR) & num_micros == 3 & StartYear > 2009 ~ "0%",
                                                 TRUE ~ Txt_Cov_UR),
                          Txt_Cov_UL = case_when(is.na(Txt_Cov_UL) & StartYear == 2006 ~ "Not Sampled",
                                                 is.na(Txt_Cov_UL) & num_micros == 3 &
                                                   between(StartYear, 2007, 2009) ~ "Not Collected",
                                                 is.na(Txt_Cov_UL) & num_micros == 3 & StartYear > 2009 ~ "0%",
                                                 TRUE ~ Txt_Cov_UL),
                          Txt_Cov_B = case_when(is.na(Txt_Cov_B) & StartYear == 2006 ~ "Not Sampled",
                                                 is.na(Txt_Cov_B) & num_micros == 3 &
                                                  between(StartYear, 2007, 2009) ~ "Not Collected",
                                                 is.na(Txt_Cov_B) & num_micros == 3 & StartYear > 2009 ~ "0%",
                                                 TRUE ~ Txt_Cov_B),
                          num_pres = ifelse(ScientificName == 'None present', 0, num_pres))

  shrub_comb3$shrub_avg_cov <- as.numeric(NA)
  shrub_comb3$shrub_avg_cov <- rowSums(shrub_comb3[, c("Pct_Cov_UR", "Pct_Cov_UL", "Pct_Cov_B")], na.rm = TRUE)/ shrub_comb3$num_micros
  shrub_comb3$shrub_pct_freq <- as.numeric(NA)
  shrub_comb3$shrub_pct_freq <- shrub_comb3$num_pres/shrub_comb3$num_micros * 100

  # Clean up column name order
  req_cols <- c("Plot_Name", "Network", "ParkUnit", "ParkSubUnit", "PlotTypeCode", "PanelCode",
                "PlotCode", "PlotID", "EventID", "IsQAQC", "StartYear", "cycle",
                "TSN", "ScientificName")

  taxa_cols <- c("Exotic", "InvasiveNETN", "Shrub", "Vine")

  pct_cols <- c("Pct_Cov_UR", "Pct_Cov_UL", "Pct_Cov_B")
  txt_cols <- c("Txt_Cov_UR", "Txt_Cov_UL", "Txt_Cov_B")
  avg_cols <- c("shrub_avg_cov", "shrub_pct_freq")

  shrub_final <- switch(valueType,
                        "midpoint" = shrub_comb3[, c(req_cols, pct_cols, avg_cols, taxa_cols)],
                        "classes" = shrub_comb3[, c(req_cols, txt_cols, avg_cols, taxa_cols)],
                        "all" = shrub_comb3[, c(req_cols, pct_cols, txt_cols, avg_cols, taxa_cols)],
                        "averages" = shrub_comb3[, c(req_cols, avg_cols, taxa_cols)])


  return(shrub_final)
} # end of function


