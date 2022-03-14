#' @include joinLocEvent.R
#' @include prepTaxa.R
#'
#' @title joinQuadSpecies: compiles quadrat species data
#'
#' @importFrom dplyr across anti_join case_when group_by filter full_join left_join rename rename_with row_number select summarize ungroup
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect all_of
#' @importFrom stringr str_replace
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
#' @param returnNoCover Logical. If FALSE (default), drops species with 0% cover across all quadrats. If TRUE,
#' includes species percent cover across all quadrats. Argument is helpful for generating a plot species list
#' (use TRUE) or calculating average cover (use FALSE).
#'
#' @param ... Other arguments passed to function.
#'
#' @return Returns a dataframe with a row for each species/visit combination for quadrat data
#'
#' @examples
#' \dontrun{
#' importData()
#' # compile quadrat data for invasive species in SARA for all years
#' SARA_quads <- joinQuadSpecies(park = 'SARA', speciesType = 'invasive')
#'
#' # compile native species only for all parks in cycle 3
#' native_quads <- joinQuadSpecies(speciesType = 'native', from = 2014, to = 2017)
#' }
#' @export
#'
#------------------------
# Joins quadrat tables and filters by park, year, and plot/visit type
#------------------------
joinQuadSpecies <- function(park = 'all', from = 2006, to = 2021, QAQC = FALSE, panels = 1:4,
                            locType = c('VS', 'all'), eventType = c('complete', 'all'),
                            speciesType = c('all', 'native', 'exotic', 'invasive'),
                            valueType = c('all', 'midpoint', 'classes', 'averages'),
                            returnNoCover = FALSE,
                            ...){
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
  stopifnot(class(returnNoCover) == 'logical')

  options(scipen = 100)
  env <- if(exists("VIEWS_NETN")){VIEWS_NETN} else {.GlobalEnv}

  # Prepare the quadrat data
  tryCatch(quadspp <- get("QuadSpecies_NETN", envir = env) %>%
             select(Plot_Name, PlotID, EventID, SQQuadSum, UC_SQ, UR_SQ, MR_SQ, BR_SQ, BC_SQ,
                    BL_SQ, ML_SQ, UL_SQ, TSN, ScientificName, UC, UR, MR, BR, BC,
                    BL, ML, UL, UC_txt, UR_txt, MR_txt, BR_txt, BC_txt, BL_txt,
                    ML_txt, UL_txt, IsGerminant, ConfidenceClassCode, IsCollected, QuadSppNote),
           error = function(e){stop("QuadSpecies_NETN view not found. Please import view.")})

  taxa_wide <- force(prepTaxa())

  # subset with EventID from plot_events to make function faster
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short')) %>%
                  select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
                         PlotCode, PlotID, EventID, SampleYear, SampleDate, cycle, IsQAQC)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  pe_list <- unique(plot_events$EventID)

  quad_list <- c("UC", "UR", "MR", "BR", "BC", "BL", "ML", "UL")
  quad_txt_list <- c("UC_txt", "UR_txt", "MR_txt", "BR_txt", "BC_txt", "BL_txt", "ML_txt", "UL_txt")
  quad_sq_list <- c("UC_SQ", "UR_SQ", "MR_SQ", "BR_SQ", "BC_SQ", "BL_SQ", "ML_SQ", "UL_SQ")

  quadspp_evs <- filter(quadspp, EventID %in% pe_list) %>%
                 mutate(missing_cover = ifelse(rowSums(across(all_of(quad_list)), na.rm = T) == 0, TRUE, FALSE))

  names(quadspp_evs)[names(quadspp_evs) == "ConfidenceClassCode"] <- "Confidence"

  quadspp_lj <- left_join(plot_events, quadspp_evs,
                          by = c("Plot_Name", "PlotID", "EventID")) %>%
                select(Plot_Name:IsQAQC, UC_SQ:UL_SQ, SQQuadSum) %>% unique()


  # join with taxa data, so can filter for smaller dataset early
  quadspp_tax <- left_join(quadspp_evs, taxa_wide[, c("TSN", "ScientificName", "Exotic", "InvasiveNETN")],
                           by = c("TSN", "ScientificName"))

  quadspp_sum <- quadspp_tax %>% mutate(
    across(.col = c(UC, UR, MR, BR, BC, BL, ML, UL),
           .names = "Pct_Cov_{col}",
           ~case_when(.x == 0 ~ 0,
                      .x == 1 ~ 0.1,
                      .x == 2 ~ 1.5,
                      .x == 3 ~ 3.5,
                      .x == 4 ~ 7.5,
                      .x == 5 ~ 17.5,
                      .x == 6 ~ 37.5,
                      .x == 7 ~ 62.5,
                      .x == 8 ~ 85,
                      .x == 9 ~ 97.5,
                      TRUE ~ NA_real_))) %>%
    mutate(across(.col = c(UC_txt, UR_txt, MR_txt, BR_txt, BC_txt,
                           BL_txt, ML_txt, UL_txt),
                  ~ifelse(. == "-<1%", "<1%", .)))

  quadspp_sum <- quadspp_sum %>%
    rename_with(.col = contains("_txt"),
                .fn = ~paste0("Txt_Cov_", substr(.x, 1, 2)))

  quadspp_sum$num_quads <- rowSums(!is.na(quadspp_sum[, c("UC", "UR", "MR", "BR",
                                                          "BC", "BL", "ML", "UL")]),
                                   na.rm = T)

  quadspp_sum$quad_avg_cov <- rowSums(quadspp_sum[, c("Pct_Cov_UC", "Pct_Cov_UR", "Pct_Cov_MR",
                                                      "Pct_Cov_BR", "Pct_Cov_BC", "Pct_Cov_BL",
                                                      "Pct_Cov_ML", "Pct_Cov_UL")],
                                      na.rm = T)/quadspp_sum$num_quads

  quadspp_sum$quad_pct_freq <- apply(quadspp_sum[, c("UC", "UR", "MR", "BR",
                                                    "BC", "BL", "ML", "UL")],
                                     1, function(x) sum(ifelse(x > 0, 1, 0), na.rm = T))/
                                     quadspp_sum$num_quads * 100

  quadspp_filt <- switch(speciesType,
                         'native' = filter(quadspp_sum, Exotic == FALSE),
                         'exotic' = filter(quadspp_sum, Exotic == TRUE),
                         'invasive' = filter(quadspp_sum, InvasiveNETN == TRUE),
                         'all' = quadspp_sum) %>%
                  select(-all_of(quad_sq_list), -SQQuadSum)

  # Join the plot, visit, SQ info back after species filter
  quadspp_comb <- left_join(quadspp_lj, quadspp_filt,
                            by = c("Plot_Name", "PlotID", "EventID"))

  quadspp_comb2 <- left_join(quadspp_comb,
                             taxa_wide %>% select(TSN, Tree, TreeShrub, Shrub, Vine,
                                                  Herbaceous, Graminoid, FernAlly),
                             by = c("TSN"))

  quadspp_comb2 <- quadspp_comb2 %>% mutate(
    ScientificName = case_when(is.na(ScientificName) & num_quads > 0 ~ "None present",
                               is.na(ScientificName) & num_quads == 0 ~ "Not Sampled",
                               TRUE ~ paste(ScientificName)),
    quad_avg_cov = ifelse(is.na(quad_avg_cov) & num_quads > 0, 0, quad_avg_cov),
    quad_pct_freq = ifelse(is.na(quad_pct_freq) & num_quads > 0, 0, quad_pct_freq))

  # Change "Permanently Missing in txt cover fields to "Not Sampled" where that's the case.
  # Makes the results more informative. ACAD-029-2010 is EventID 710. That stays PM
  # Don't have time to figure out the fancy way to do this right now
  quadspp_comb2$Txt_Cov_UC[quadspp_comb2$UC_SQ == 'NS' & quadspp_comb2$EventID != 710] <- "Not Sampled"
  quadspp_comb2$Txt_Cov_UR[quadspp_comb2$UR_SQ == 'NS' & quadspp_comb2$EventID != 710] <- "Not Sampled"
  quadspp_comb2$Txt_Cov_MR[quadspp_comb2$MR_SQ == 'NS' & quadspp_comb2$EventID != 710] <- "Not Sampled"
  quadspp_comb2$Txt_Cov_BR[quadspp_comb2$BR_SQ == 'NS' & quadspp_comb2$EventID != 710] <- "Not Sampled"
  quadspp_comb2$Txt_Cov_BC[quadspp_comb2$BC_SQ == 'NS' & quadspp_comb2$EventID != 710] <- "Not Sampled"
  quadspp_comb2$Txt_Cov_BL[quadspp_comb2$BL_SQ == 'NS' & quadspp_comb2$EventID != 710] <- "Not Sampled"
  quadspp_comb2$Txt_Cov_ML[quadspp_comb2$ML_SQ == 'NS' & quadspp_comb2$EventID != 710] <- "Not Sampled"
  quadspp_comb2$Txt_Cov_UL[quadspp_comb2$UL_SQ == 'NS' & quadspp_comb2$EventID != 710] <- "Not Sampled"

  na_cols <- c("Exotic", "InvasiveNETN", "Tree", "TreeShrub", "Shrub", "Vine",
               "Herbaceous", "Graminoid", "FernAlly")

  quadspp_comb2[ , na_cols][is.na(quadspp_comb2[, na_cols])] <- 0

  cov_rename <- function(txt, col){paste(txt, substr(col, 1, 2), sep = "_")}

  quadspp_comb3 <- quadspp_comb2 %>% rename_with(~cov_rename("SQ", .), all_of(quad_sq_list))


  quadspp_comb4 <- if(returnNoCover == FALSE){
    filter(quadspp_comb3, missing_cover == FALSE)
    } else {quadspp_comb3}

  # select columns based on specified valueType
  req_cols <- c("Plot_Name", "Network", "ParkUnit", "ParkSubUnit", "PlotTypeCode",
                "PanelCode", "PlotCode", "PlotID", "EventID", "IsQAQC", "SampleYear",
                "SampleDate", "cycle", "SQQuadSum", "TSN", "ScientificName", "num_quads")

  sum_cols <- c("quad_avg_cov", "quad_pct_freq")

  plant_cols <- c("Confidence", "IsGerminant")

  sq_cols <- c("SQ_UC", "SQ_UR", "SQ_MR", "SQ_BR", "SQ_BC", "SQ_BL", "SQ_ML", "SQ_UL")

  pct_cols <- c("Pct_Cov_UC", "Pct_Cov_UR", "Pct_Cov_MR", "Pct_Cov_BR",
                "Pct_Cov_BC", "Pct_Cov_BL", "Pct_Cov_ML", "Pct_Cov_UL")

  txt_cols <- c("Txt_Cov_UC", "Txt_Cov_UR", "Txt_Cov_MR", "Txt_Cov_BR",
                "Txt_Cov_BC", "Txt_Cov_BL", "Txt_Cov_ML", "Txt_Cov_UL")

  taxa_cols <- c("Exotic", "InvasiveNETN", "Tree", "TreeShrub", "Shrub", "Vine",
                 "Herbaceous", "Graminoid", "FernAlly")


  quadspp_final <- switch(valueType,
                          "midpoint" = quadspp_comb4[, c(req_cols, pct_cols, plant_cols,
                                                         sum_cols, taxa_cols, "QuadSppNote")],
                          "classes" = quadspp_comb4[, c(req_cols, txt_cols, plant_cols,
                                                        sum_cols, taxa_cols, "QuadSppNote")],
                          "all" = quadspp_comb4[, c(req_cols, sq_cols, pct_cols, txt_cols,
                                                    plant_cols, sum_cols, taxa_cols, "QuadSppNote")],
                          "averages" = quadspp_comb4[, c(req_cols, sum_cols, plant_cols, taxa_cols)])

  return(data.frame(quadspp_final))
  } # end of function

