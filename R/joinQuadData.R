#' @include joinLocEvent.R
#' @title joinQuadData: compiles quadrat character data
#'
#' @importFrom dplyr across arrange case_when contains full_join group_by left_join mutate rename_with select summarize ungroup
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#'
#' @description This function compiles the quadrat character data (i.e., Soil, Rock, etc.) into a wide format,
#' so that each quadrat has a column. Notes fields are not compiled in this function. For quadrat-related notes,
#' use the joinQuadNotes() function.
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
#' @param locType Allows you to only include plots that are part of the GRTS sample design or include all plots, such as deer exclosures
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
#' @param valueType Allows you to return cover class midpoints (numeric) or cover class ranges (text)
#' \describe{
#' \item{"all"}{Default. Returns columns for midpoint and cover classes for each quad}
#' \item{"midpoint"}{Default. Returns numeric cover class midpoints, with Pct prefix.}
#' \item{"classes"}{Returns the text cover class definitions, with Txt prefix.}
#' }
#'
#' @param panels Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
#' If more than one panel is selected, specify by c(1, 3), for example.
#'
#' @param ... Other arguments passed to function.
#'
#' @return Returns a dataframe with cover class midpoints for each quadrat and includes guild for each species.
#'
#' @examples
#' \dontrun{
#' importData()
#' # compile quadrat data cover class midpoints in SARA for all years
#' SARA_quads <- joinQuadData(park = 'SARA', valueType = 'midpoint')
#'
#' # compile quadrat data for cycle 3
#' native_quads <- joinQuadData(from = 2014, to = 2017)
#'}
#' @export
#'
#------------------------
# Joins quadrat character data and filters by park, year, and plot/visit type
#------------------------
joinQuadData <- function(park = 'all', from = 2006, to = 2021, QAQC = FALSE, panels = 1:4,
                         locType = c('VS', 'all'), eventType = c('complete', 'all'),
                         valueType = c('all', 'midpoint', 'classes'),  ...){

  # Match args and class
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA"))
  stopifnot(class(from) == "numeric", from >= 2006)
  stopifnot(class(to) == "numeric", to >= 2006)
  stopifnot(class(QAQC) == 'logical')
  stopifnot(panels %in% c(1, 2, 3, 4))
  locType <- match.arg(locType)
  eventType <- match.arg(eventType)
  valueType <- match.arg(valueType)


  env <- if(exists("VIEWS_NETN")){VIEWS_NETN} else {.GlobalEnv}

  # Prepare the quad data
  tryCatch(quadchar <- get("QuadCharacter_NETN", envir = env) %>%
                       select(Plot_Name, PlotID, EventID, CharacterSortOrder, CharacterCode,
                              CharacterLabel, UC_SQ, UR_SQ, MR_SQ, BR_SQ, BC_SQ, BL_SQ, ML_SQ, UL_SQ,
                              UC, UR, MR, BR, BC, BL, ML, UL,
                              UC_txt, UR_txt, MR_txt, BR_txt, BC_txt, BL_txt, ML_txt, UL_txt),
           error = function(e){stop("QuadCharacter_NETN view not found. Please import view.")}
  )

  # Need to pull in IsTrampled
  tryCatch(quadnotes <- get("QuadNotes_NETN", envir = env) %>%
                        select(Plot_Name, PlotID, EventID, QuadratCode, IsTrampled),
           error = function(e){stop("QuadNotes_NETN view not found. Please import view.")}
  )

  # subset with EventID from plot_events to make function faster
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short')) %>% #, ...)) %>%
                 select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
                        EventID, SampleYear, SampleDate, cycle, IsQAQC)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  pe_list <- unique(plot_events$EventID)

  quadchar_evs <- filter(quadchar, EventID %in% pe_list)

  # reshape trampled data to wide
  quadtramp <- filter(quadnotes, EventID %in% pe_list) %>%
               pivot_wider(names_from = QuadratCode,
                           values_from = IsTrampled,
                           names_glue = "{QuadratCode}_tramp")

  # Change cover class codes to midpoints and clean up
  quadchar_evs2 <- quadchar_evs %>%
    mutate(across(.col = c(UC, UR, MR, BR, BC, BL, ML, UL),
                  .names = "Pct_Cov_{col}",
                  ~case_when(. == 0 ~ 0,
                             . == 1 ~ 0.1,
                             . == 2 ~ 1.5,
                             . == 3 ~ 3.5,
                             . == 4 ~ 7.5,
                             . == 5 ~ 17.5,
                             . == 6 ~ 37.5,
                             . == 7 ~ 62.5,
                             . == 8 ~ 85,
                             . == 9 ~ 97.5,
                             TRUE ~ NA_real_))) %>%
    mutate(across(.col = c(UC_txt, UR_txt, MR_txt, BR_txt, BC_txt,
                           BL_txt, ML_txt, UL_txt),
                  ~ifelse(. == "-<1%", "<1%", .))) #%>%

  quadchar_evs2 <- quadchar_evs2 %>%
    rename_with(.col = contains("_txt"),
                .fn = ~paste0("Txt_Cov_", substr(.x, 1, 2)))


  quadchar_comb <- full_join(quadchar_evs2, quadtramp, by = c("Plot_Name", "PlotID", "EventID"))

  quadchar_comb$num_quads <- rowSums(!is.na(quadchar_comb[, c("UC", "UR", "MR", "BR",
                                                              "BC", "BL", "ML", "UL")]),
                                     na.rm = T)

  quadchar_comb$num_trampled <- rowSums(quadchar_comb[, c("UC_tramp", "UR_tramp", "MR_tramp", "BR_tramp",
                                                       "BC_tramp", "BL_tramp", "ML_tramp", "UL_tramp")],
                                     na.rm = T)

  quadchar_comb$quad_avg_cov <- rowSums(quadchar_comb[, c("Pct_Cov_UC", "Pct_Cov_UR", "Pct_Cov_MR",
                                                          "Pct_Cov_BR", "Pct_Cov_BC", "Pct_Cov_BL",
                                                          "Pct_Cov_ML", "Pct_Cov_UL")],
                                        na.rm = T)/
                                quadchar_comb$num_quads

  quadchar_comb$quad_pct_freq <- apply(quadchar_comb[, c("UC", "UR", "MR", "BR",
                                                         "BC", "BL", "ML", "UL")],
                                       1, function(x) sum(ifelse(x > 0, 1, 0), na.rm = T))/
                                 quadchar_comb$num_quads * 100


  quadchar_comb2 <- left_join(plot_events, quadchar_comb,
                              by = intersect(names(plot_events), names(quadchar_comb)))

  # Rename SQ columns, so SQ is first, quad is last
  cov_rename <- function(txt, col){paste(txt, substr(col, 1, 2), sep = "_")}
  quad_sq_list <- c("UC_SQ", "UR_SQ", "MR_SQ", "BR_SQ", "BC_SQ", "BL_SQ", "ML_SQ", "UL_SQ")
  quadchar_comb3 <- quadchar_comb2 %>% rename_with(~cov_rename("SQ", .), all_of(quad_sq_list))

  # select columns based on specified valueType
  req_cols <- c("Plot_Name", "Network", "ParkUnit", "ParkSubUnit", "PlotTypeCode", "PanelCode",
                "PlotCode", "PlotID", "EventID", "IsQAQC", "SampleYear", "SampleDate", "cycle",
                "CharacterLabel", "num_quads", "num_trampled", "quad_avg_cov", "quad_pct_freq")

  pct_cols <- c("Pct_Cov_UC", "Pct_Cov_UR", "Pct_Cov_MR", "Pct_Cov_BR",
                "Pct_Cov_BC", "Pct_Cov_BL", "Pct_Cov_ML", "Pct_Cov_UL")

  txt_cols <- c("Txt_Cov_UC", "Txt_Cov_UR", "Txt_Cov_MR", "Txt_Cov_BR",
                "Txt_Cov_BC", "Txt_Cov_BL", "Txt_Cov_ML", "Txt_Cov_UL")

  sq_cols <- c("SQ_UC", "SQ_UR", "SQ_MR", "SQ_BR", "SQ_BC", "SQ_BL", "SQ_ML", "SQ_UL")

  # Change "Permanently Missing in txt cover fields to "Not Sampled" where that's the case.
  # Makes the results more informative. ACAD-029-2010 is EventID 710. That stays PM
  # Don't have time to figure out the fancy way to do this right now
  quadchar_comb3$Txt_Cov_UC[quadchar_comb3$SQ_UC == 'NS' & quadchar_comb3$EventID != 710] <- "Not Sampled"
  quadchar_comb3$Txt_Cov_UR[quadchar_comb3$SQ_UR == 'NS' & quadchar_comb3$EventID != 710] <- "Not Sampled"
  quadchar_comb3$Txt_Cov_MR[quadchar_comb3$SQ_MR == 'NS' & quadchar_comb3$EventID != 710] <- "Not Sampled"
  quadchar_comb3$Txt_Cov_BR[quadchar_comb3$SQ_BR == 'NS' & quadchar_comb3$EventID != 710] <- "Not Sampled"
  quadchar_comb3$Txt_Cov_BC[quadchar_comb3$SQ_BC == 'NS' & quadchar_comb3$EventID != 710] <- "Not Sampled"
  quadchar_comb3$Txt_Cov_BL[quadchar_comb3$SQ_BL == 'NS' & quadchar_comb3$EventID != 710] <- "Not Sampled"
  quadchar_comb3$Txt_Cov_ML[quadchar_comb3$SQ_ML == 'NS' & quadchar_comb3$EventID != 710] <- "Not Sampled"
  quadchar_comb3$Txt_Cov_UL[quadchar_comb3$SQ_UL == 'NS' & quadchar_comb3$EventID != 710] <- "Not Sampled"

  quadchar_comb3$ScientificName[quadchar_comb3$num_quads == 0 & quadchar_comb3$EventID != 710] <- "Not Sampled"


  quadchar_final <- switch(valueType,
                          "midpoint" = quadchar_comb3[, c(req_cols, pct_cols)],
                          "classes" = quadchar_comb3[, c(req_cols, txt_cols)],
                          "all" = quadchar_comb3[, c(req_cols, sq_cols, pct_cols, txt_cols)])

  return(data.frame(quadchar_final))

  } # end of function

