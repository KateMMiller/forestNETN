#' @include joinLocEvent.R
#' @include prepTaxa.R
#'
#' @title joinQuadSpecies: compiles quadrat species data
#'
#' @importFrom dplyr anti_join case_when group_by filter full_join left_join row_number select summarize ungroup
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#'
#' @description This function combines quadrat species data with species names and allows you to filter on species types, park,
#' years, and visit type. Note that the Shrub guild also includes woody vine species. Species-level notes are returned. For
#' quadrat-specific notes, use the joinQuadNotes() function.
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
#' @param valueType Allows you to return cover class midpoints (numeric) or cover class ranges (text)
#' \describe{
#' \item{"all"}{Default. Returns columns for midpoint and cover classes for each quad}
#' \item{"midpoint"}{Default. Returns numeric cover class midpoints, with Pct prefix.}
#' \item{"classes"}{Returns the text cover class definitions, with Txt prefix.}
#' \item{"averages"}{Returns only the plot-level average cover and percent frequency.}
#' }
#'
#' @return Returns a dataframe with a row for each species/visit combination for quadrat data
#'
#' @examples
#' importData()
#' # compile quadrat data for invasive species in SARA for all years
#' SARA_quads <- joinQuadSpecies(park = 'SARA', speciesType = 'invasive')
#'
#' # compile native species only for all parks in cycle 3
#' native_quads <- joinQuadSpecies(speciesType = 'native', from = 2014, to = 2017)
#'
#' @export
#'
#------------------------
# Joins quadrat tables and filters by park, year, and plot/visit type
#------------------------
joinQuadSpecies <- function(park = 'all', from = 2006, to = 2021, QAQC = FALSE, panels = 1:4,
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

  options(scipen = 100)
  env <- if(exists("VIEWS_NETN")){VIEWS_NETN} else {.GlobalEnv}

  # Prepare the quadrat data
  tryCatch(quadspp <- get("NETN_QuadSpecies", envir = env) %>%
             select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, SQQuadSppCode,
                    QuadratCode, TSN, ScientificName, CoverClassCode, CoverClassLabel, IsGerminant,
                    ConfidenceClassCode, IsCollected, QuadSppNote),
           error = function(e){stop("NETN_QuadSpecies view not found. Please import view.")})

  taxa_wide <- force(prepTaxa())

  # subset with EventID from plot_events to make function faster
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short')) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
           EventID, StartYear, StartDate, cycle, IsQAQC)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  pe_list <- unique(plot_events$EventID)

  quadspp_evs <- filter(quadspp, EventID %in% pe_list)
  names(quadspp_evs)[names(quadspp_evs) == "ConfidenceClassCode"] <- "Confidence"

  # join with taxa data, so can filter for smaller dataset early
  quadspp_tax <- left_join(quadspp_evs, taxa_wide[, c("TSN", "ScientificName", "Exotic", "InvasiveNETN")],
                           by = c("TSN", "ScientificName"))

  quad_check <- quadspp_tax %>% group_by(PlotID, EventID, ParkUnit, PlotCode, StartYear, IsQAQC,
                                          TSN, ScientificName, IsGerminant) %>%
                                 summarize(num_quads = sum(SQQuadSppCode == "SS", na.rm = T),
                                           .groups = 'drop') %>%
                                 filter(num_quads > 8) %>% select(-num_quads)

  if(nrow(quad_check) > 0){
    warning(paste("The following records have duplicate species records and will be combined: \n"),
            paste(capture.output(data.frame(quad_check)), collapse = "\n"))
    }

  # Set up midpoints- this approach was much faster than case_when/dplyr
  quadspp_tax$CovClass_num <- suppressWarnings(as.numeric(quadspp_tax$CoverClassCode))

  # Cover class conversions
  quadspp_tax <- quadspp_tax %>% mutate(Pct_Cov = case_when(CovClass_num == 0 ~ 0,
                                                            CovClass_num == 1 ~ 0.1,
                                                            CovClass_num == 2 ~ 1.5,
                                                            CovClass_num == 3 ~ 3.5,
                                                            CovClass_num == 4 ~ 7.5,
                                                            CovClass_num == 5 ~ 17.5,
                                                            CovClass_num == 6 ~ 37.5,
                                                            CovClass_num == 7 ~ 62.5,
                                                            CovClass_num == 8 ~ 85,
                                                            CovClass_num == 9 ~ 97.5,
                                                            TRUE ~ NA_real_),
                                        Txt_Cov = case_when(SQQuadSppCode == "NS" ~ "Not Sampled",
                                                            SQQuadSppCode == "SS" &
                                                              CoverClassLabel == "-<1%" ~ "1%",
                                                            SQQuadSppCode == "SS" &
                                                              CoverClassLabel != "-<1%" ~ CoverClassLabel))

  quadspp_tax$Sampled <- ifelse(quadspp_tax$SQQuadSppCode %in% c("SS", "NP"), 1, 0)

  quadspp_tax$freq <- ifelse(quadspp_tax$Pct_Cov > 0, 1, 0) # If Pct_Cov is NA, returns NA

  quad_sum <- quadspp_tax %>% group_by(PlotID, EventID, TSN, ScientificName, IsGerminant) %>%
                              summarize(num_quads = sum(Sampled, na.rm = T),
                                        quad_avg_cov = sum(Pct_Cov, na.rm = T)/num_quads,
                                        quad_pct_freq = (sum(freq, na.rm = T)/num_quads)*100,
                                        .groups = 'drop') %>%
                                        ungroup()

  # Setting up df for filling out left join after filter
  quad_sq <- quadspp_tax %>% mutate(SQ = ifelse(SQQuadSppCode %in% c("NP", "SS"), 1, 0)) %>%
                             select(PlotID, EventID, SQ, QuadratCode) %>% unique() %>%
                             pivot_wider(names_from = QuadratCode,
                                         values_from = SQ,
                                         values_fill = 0,
                                         names_prefix = "SQ_",
                                          )

  quad_sq$num_quad_sq <- rowSums(quad_sq[,c("SQ_UC", "SQ_UR", "SQ_MR", "SQ_BR",
                                          "SQ_BC", "SQ_BL", "SQ_ML", "SQ_UL")])

  plot_quad_lj <- left_join(plot_events, quad_sq, by = c("PlotID", "EventID"))

  # manual cleanup to drop ROVA-008-2011 extra row because of NS for BL
  quadspp_tax2 <- quadspp_tax %>% filter(!(PlotCode == 008 & ParkUnit == "ROVA" & is.na(ScientificName)))

  quadspp_filt <- switch(speciesType,
                         'native' = filter(quadspp_tax2, Exotic == FALSE),
                         'exotic' = filter(quadspp_tax2, Exotic == TRUE),
                         'invasive' = filter(quadspp_tax2, InvasiveNETN == TRUE),
                         'all' = quadspp_tax2)

  # Spread quadrats wide
  quadspp_wide <- quadspp_filt %>% select(-freq, -Sampled, -CoverClassCode, -CoverClassLabel, -CovClass_num) %>%
                                   pivot_wider(names_from = QuadratCode,
                                               values_from = c(Pct_Cov, Txt_Cov),
                                               values_fill = list(Pct_Cov = 0, Txt_Cov = "0%"))
  quadspp_comb <- full_join(quadspp_wide, quad_sum,
                            intersect(names(quadspp_wide), names(quad_sum)))

  quadspp_comb2 <- left_join(plot_quad_lj, quadspp_comb,
                             by = intersect(names(plot_quad_lj), names(quadspp_comb)))

  quadspp_comb3 <- left_join(quadspp_comb2,
                             taxa_wide %>% select(TSN, Tree, TreeShrub, Shrub, Vine, Herbaceous,
                                                 Graminoid, FernAlly),
                             by = c("TSN"))

  quadspp_comb3$ScientificName[is.na(quadspp_comb3$ScientificName)] <- "None present"

  na_cols <- c("Exotic", "InvasiveNETN", "quad_avg_cov", "quad_pct_freq",
               "Tree", "TreeShrub", "Shrub", "Vine", "Herbaceous",
               "Graminoid", "FernAlly")

  quadspp_comb3[ , na_cols][is.na(quadspp_comb3[, na_cols])] <- 0

  # select columns based on specified valueType
  req_cols <- c("Plot_Name", "Network", "ParkUnit", "ParkSubUnit", "PlotTypeCode", "PanelCode",
                "PlotCode", "PlotID", "EventID", "IsQAQC", "StartYear", "StartDate", "cycle",
                "SQQuadSppCode", "TSN", "ScientificName", "Confidence", "IsGerminant", "num_quads",
                "quad_avg_cov", "quad_pct_freq")

  pct_cols <- c("Pct_Cov_UC", "Pct_Cov_UR", "Pct_Cov_MR", "Pct_Cov_BR",
                "Pct_Cov_BC", "Pct_Cov_BL", "Pct_Cov_ML", "Pct_Cov_UL")

  txt_cols <- c("Txt_Cov_UC", "Txt_Cov_UR", "Txt_Cov_MR", "Txt_Cov_BR",
                "Txt_Cov_BC", "Txt_Cov_BL", "Txt_Cov_ML", "Txt_Cov_UL")

  taxa_cols <- c("Exotic", "InvasiveNETN", "Tree", "TreeShrub", "Shrub", "Vine", "Herbaceous",
                 "Graminoid", "FernAlly")

  # Convert NAs to 0 except quads with SQ NS to NA
  quadspp_comb3[, pct_cols][is.na(quadspp_comb3[, pct_cols])] <- 0
  quadspp_comb3[, txt_cols][is.na(quadspp_comb3[, txt_cols])] <- "0%"

  # Don't have time to figure out the fancy way to do this right now
  quadspp_comb3$Pct_Cov_UC[quadspp_comb3$SQ_UC == 0] <- NA
  quadspp_comb3$Txt_Cov_UC[quadspp_comb3$SQ_UC == 0] <- "Not Sampled"
  quadspp_comb3$Pct_Cov_UR[quadspp_comb3$SQ_UR == 0] <- NA
  quadspp_comb3$Txt_Cov_UR[quadspp_comb3$SQ_UR == 0] <- "Not Sampled"
  quadspp_comb3$Pct_Cov_MR[quadspp_comb3$SQ_MR == 0] <- NA
  quadspp_comb3$Txt_Cov_MR[quadspp_comb3$SQ_MR == 0] <- "Not Sampled"
  quadspp_comb3$Pct_Cov_BR[quadspp_comb3$SQ_BR == 0] <- NA
  quadspp_comb3$Txt_Cov_BR[quadspp_comb3$SQ_BR == 0] <- "Not Sampled"
  quadspp_comb3$Pct_Cov_BC[quadspp_comb3$SQ_BC == 0] <- NA
  quadspp_comb3$Txt_Cov_BC[quadspp_comb3$SQ_BC == 0] <- "Not Sampled"
  quadspp_comb3$Pct_Cov_BL[quadspp_comb3$SQ_BL == 0] <- NA
  quadspp_comb3$Txt_Cov_BL[quadspp_comb3$SQ_BL == 0] <- "Not Sampled"
  quadspp_comb3$Pct_Cov_ML[quadspp_comb3$SQ_ML == 0] <- NA
  quadspp_comb3$Txt_Cov_ML[quadspp_comb3$SQ_ML == 0] <- "Not Sampled"
  quadspp_comb3$Pct_Cov_UL[quadspp_comb3$SQ_UL == 0] <- NA
  quadspp_comb3$Txt_Cov_UL[quadspp_comb3$SQ_UL == 0] <- "Not Sampled"
  quadspp_comb3$ScientificName[quadspp_comb3$num_quad_sq == 0] <- "Not Sampled"

  quadspp_final <- switch(valueType,
                         "midpoint" = quadspp_comb3[, c(req_cols, pct_cols, taxa_cols, "QuadSppNote")],
                         "classes" = quadspp_comb3[, c(req_cols, txt_cols, taxa_cols, "QuadSppNote")],
                         "all" = quadspp_comb3[, c(req_cols, pct_cols, txt_cols, taxa_cols, "QuadSppNote")],
                         "averages" = quadspp_comb3[, c(req_cols, taxa_cols)])

  return(data.frame(quadspp_final))
  } # end of function

