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
#' @param shape Temporary argument until new view is ready
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
                            shape = c("long", "wide"), ...){
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

  if(shape == "long"){ ### DROP AFTER FINALIZED WIDE VIEW
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
                                                              CoverClassLabel == "-<1%" ~ "<1%",
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

  quadspp_comb4 <- quadspp_comb3 %>% filter(quadspp_comb3$ScientificName != "None present" &
                                            rowSums(quadspp_comb3[,c("Pct_Cov_UC", "Pct_Cov_UR", "Pct_Cov_MR", "Pct_Cov_BR",
                                              "Pct_Cov_BC", "Pct_Cov_BL", "Pct_Cov_ML", "Pct_Cov_UL")], na.rm = T) > 0)

  quadspp_final <- switch(valueType,
                         "midpoint" = quadspp_comb4[, c(req_cols, pct_cols, taxa_cols, "QuadSppNote")],
                         "classes" = quadspp_comb4[, c(req_cols, txt_cols, taxa_cols, "QuadSppNote")],
                         "all" = quadspp_comb4[, c(req_cols, pct_cols, txt_cols, taxa_cols, "QuadSppNote")],
                         "averages" = quadspp_comb4[, c(req_cols, taxa_cols)])

  } else if(shape == "wide"){

    tryCatch(quadspp <- get("NETN_QuadSpecies_wide", envir = env) %>%
               select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC,
                      TSN, ScientificName, SQQuadSum,
                      UC_SQ, UR_SQ, MR_SQ, BR_SQ, BC_SQ, BL_SQ, ML_SQ, UL_SQ,
                      UC, UR, MR, BR, BC, BL, ML, UL,
                      UC_txt, UR_txt, MR_txt, BR_txt, BC_txt, BL_txt, ML_txt, UL_txt,
                      IsGerminant, ConfidenceClassCode, IsCollected, QuadSppNote),
             error = function(e){stop("NETN_QuadSpecies_wide view not found. Please import view.")})

    taxa_wide <- force(prepTaxa())

    # subset with EventID from plot_events to make function faster
    plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                      panels = panels, locType = locType, eventType = eventType,
                                      abandoned = FALSE, output = 'short')) %>%
      select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
             EventID, StartYear, StartDate, cycle, IsQAQC)

    if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

    pe_list <- unique(plot_events$EventID)

    quad_list <- c("UC", "UR", "MR", "BR", "BC", "BL", "ML", "UL")
    quad_txt_list <- c("UC_txt", "UR_txt", "MR_txt", "BR_txt", "BC_txt", "BL_txt", "ML_txt", "UL_txt")
    quad_sq_list <- c("UC_SQ", "UR_SQ", "MR_SQ", "BR_SQ", "BC_SQ", "BL_SQ", "ML_SQ", "UL_SQ")

    quadspp_evs <- filter(quadspp, EventID %in% pe_list) %>%
                   mutate(missing_cover = ifelse(rowSums(across(all_of(quad_list)), na.rm = T) == 0, TRUE, FALSE))

    names(quadspp_evs)[names(quadspp_evs) == "ConfidenceClassCode"] <- "Confidence"

    quadspp_lj <- left_join(plot_events, quadspp_evs,
                            by = c("ParkUnit", "ParkSubUnit", "PlotCode", "PlotID",
                                   "EventID", "StartYear", "IsQAQC")) %>%
                  select(Plot_Name:IsQAQC, UC_SQ:UL_SQ, SQQuadSum) %>% unique()


    # join with taxa data, so can filter for smaller dataset early
    quadspp_tax <- left_join(quadspp_evs, taxa_wide[, c("TSN", "ScientificName", "Exotic", "InvasiveNETN")],
                             by = c("TSN", "ScientificName"))


    quadspp_sum <- quadspp_tax %>% mutate(
      across(all_of(quad_list),
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
                   TRUE ~ NA_real_)),
      across(all_of(quad_txt_list), ~str_replace(.x, "-<1%", "<1%")),
      quad_avg_cov = (rowSums(across(all_of(quad_list)), na.rm = T)/SQQuadSum),
      quad_pct_freq = ((rowSums(ifelse(across(all_of(quad_list)) > 0, 1, 0), na.rm = T)/SQQuadSum)*100)
      )

    # Cover class conversions
    # Setting up df for filling out left join after filter

    quadspp_filt <- switch(speciesType,
                           'native' = filter(quadspp_sum, Exotic == FALSE),
                           'exotic' = filter(quadspp_sum, Exotic == TRUE),
                           'invasive' = filter(quadspp_sum, InvasiveNETN == TRUE),
                           'all' = quadspp_sum) %>%
                    select(-all_of(quad_sq_list), -SQQuadSum)

    # Join the plot, visit, SQ info back after species filter
    quadspp_comb <- left_join(quadspp_lj, quadspp_filt,
                              by = c("ParkUnit", "ParkSubUnit", "PlotCode", "PlotID",
                                     "EventID", "StartYear", "IsQAQC"))

    quadspp_comb2 <- left_join(quadspp_comb,
                               taxa_wide %>% select(TSN, Tree, TreeShrub, Shrub, Vine, Herbaceous,
                                                    Graminoid, FernAlly),
                               by = c("TSN"))

    quadspp_comb2 <- quadspp_comb2 %>% mutate(
                                         ScientificName =
                                                case_when(is.na(ScientificName) & SQQuadSum > 0 ~ "None present",
                                                          is.na(ScientificName) & SQQuadSum == 0 ~ "Not Sampled",
                                                          TRUE ~ paste(ScientificName)),
                                         quad_avg_cov = ifelse(is.na(quad_avg_cov) & SQQuadSum > 0, 0, quad_avg_cov),
                                         quad_pct_freq = ifelse(is.na(quad_pct_freq) & SQQuadSum > 0, 0, quad_pct_freq),
                                         across(all_of(c(quad_list, "quad_avg_cov", "quad_pct_freq")),
                                                ~ifelse(missing_cover == TRUE, NA_real_, .x)),
                                         across(all_of(quad_txt_list), ~ifelse(missing_cover == TRUE, NA_character_, .x)))

    na_cols <- c("Exotic", "InvasiveNETN",
                 "Tree", "TreeShrub", "Shrub", "Vine", "Herbaceous",
                 "Graminoid", "FernAlly")

    quadspp_comb2[ , na_cols][is.na(quadspp_comb2[, na_cols])] <- 0

    cov_rename <- function(txt, col){paste(txt, substr(col, 1, 2), sep = "_")}

    quadspp_comb3 <- quadspp_comb2 %>% rename_with(~cov_rename("Pct_Cov", .), all_of(quad_list)) %>%
                                       rename_with(~cov_rename("Txt_Cov", .), all_of(quad_txt_list)) %>%
                                       rename_with(~cov_rename("SQ", .), all_of(quad_sq_list))

    # select columns based on specified valueType
    req_cols <- c("Plot_Name", "Network", "ParkUnit", "ParkSubUnit", "PlotTypeCode", "PanelCode",
                  "PlotCode", "PlotID", "EventID", "IsQAQC", "StartYear", "StartDate", "cycle",
                  "SQQuadSum", "TSN", "ScientificName")

    sum_cols <- c("quad_avg_cov", "quad_pct_freq")

    plant_cols <- c("Confidence", "IsGerminant")

    sq_cols <- c("SQ_UC", "SQ_UR", "SQ_MR", "SQ_BR", "SQ_BC", "SQ_BL", "SQ_ML", "SQ_UL")

    pct_cols <- c("Pct_Cov_UC", "Pct_Cov_UR", "Pct_Cov_MR", "Pct_Cov_BR",
                  "Pct_Cov_BC", "Pct_Cov_BL", "Pct_Cov_ML", "Pct_Cov_UL")

    txt_cols <- c("Txt_Cov_UC", "Txt_Cov_UR", "Txt_Cov_MR", "Txt_Cov_BR",
                  "Txt_Cov_BC", "Txt_Cov_BL", "Txt_Cov_ML", "Txt_Cov_UL")

    taxa_cols <- c("Exotic", "InvasiveNETN", "Tree", "TreeShrub", "Shrub", "Vine", "Herbaceous",
                   "Graminoid", "FernAlly")

    # Convert NAs to 0 except quads with SQ NS to NA
    # quadspp_comb3[, pct_cols][is.na(quadspp_comb3[, pct_cols]) &
    #                             quadspp_comb3$missing_cover == FALSE] <- 0
    # quadspp_comb3[, txt_cols][is.na(quadspp_comb3[, txt_cols]) &
    #                             quadspp_comb3$missing_cover == FALSE] <- "0%"


    quadspp_final <- switch(valueType,
                            "midpoint" = quadspp_comb3[, c(req_cols, pct_cols, plant_cols,
                                                           sum_cols, taxa_cols, "QuadSppNote")],
                            "classes" = quadspp_comb3[, c(req_cols, txt_cols, plant_cols,
                                                          sum_cols, taxa_cols, "QuadSppNote")],
                            "all" = quadspp_comb3[, c(req_cols, sq_cols, pct_cols, txt_cols,
                                                      plant_cols, sum_cols, taxa_cols, "QuadSppNote")],
                            "averages" = quadspp_comb3[, c(req_cols, sum_cols, taxa_cols)])

  }

  return(data.frame(quadspp_final))
  } # end of function

