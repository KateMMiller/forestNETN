#' @include joinLocEvent.R
#' @title joinQuadData: compiles quadrat character data
#'
#' @importFrom dplyr arrange case_when full_join group_by left_join mutate select summarize ungroup
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
#' @return Returns a dataframe with cover class midpoints for each quadrat and includes guild for each species.
#'
#' @examples
#' importData()
#' # compile quadrat data cover class midpoints in SARA for all years
#' SARA_quads <- joinQuadData(park = 'SARA', valueType = 'midpoint')
#'
#' # compile quadrat data for cycle 3
#' native_quads <- joinQuadData(from = 2014, to = 2017)
#'
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
  tryCatch(quadchr <- get("COMN_QuadCharacter", envir = env) %>%
                      select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, SQQuadCharCode,
                             IsTrampled, QuadratCode, CharacterLabel, CoverClassCode, CoverClassLabel),
           error = function(e){stop("COMN_QuadCharacter view not found. Please import view.")}
  )

  # subset with EventID from plot_events to make function faster
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short')) %>%
                 select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
                        EventID, StartYear, StartDate, cycle, IsQAQC)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  pe_list <- unique(plot_events$EventID)

  quadchr_evs <- filter(quadchr, EventID %in% pe_list)

  # prep for reshaping to wide
  quadchr_evs2 <- quadchr_evs %>%
    mutate(Pct_Cov = as.numeric(case_when(CoverClassCode == "0" ~ 0,
                                            CoverClassCode == "1" ~ 0.1,
                                            CoverClassCode == "2" ~ 1.5,
                                            CoverClassCode == "3" ~ 3.5,
                                            CoverClassCode == "4" ~ 7.5,
                                            CoverClassCode == "5" ~ 17.5,
                                            CoverClassCode == "6" ~ 37.5,
                                            CoverClassCode == "7" ~ 62.5,
                                            CoverClassCode == "8" ~ 85,
                                            CoverClassCode == "9" ~ 97.5,
                                            CoverClassCode %in% c("NC", "PM") ~ NA_real_)), # convert to NA later
           Txt_Cov = ifelse(CoverClassLabel == "-<1%", "<1%", CoverClassLabel),
           Sampled = ifelse(SQQuadCharCode == "SS", 1, 0)) %>%
    select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, QuadratCode,
           Sampled, IsTrampled, CharacterLabel, Pct_Cov, Txt_Cov)

  quad_sum <- quadchr_evs2 %>% group_by(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear,
                                        IsQAQC, CharacterLabel) %>%
                               summarize(num_quads = sum(Sampled, na.rm = T),
                                         num_trampled = sum(IsTrampled, na.rm = T),
                                         quad_avg_cov = sum(Pct_Cov, na.rm = T)/num_quads,
                                         quad_pct_freq = (sum(Pct_Cov > 0, na.rm = T)/num_quads)*100,
                                         .groups = 'drop') %>% ungroup()


  # make data wide on quad name
  quadchr_wide <- quadchr_evs2 %>%  select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear,
                                           IsQAQC, QuadratCode, CharacterLabel, Pct_Cov, Txt_Cov) %>%
                                    pivot_wider(names_from = QuadratCode,
                                               values_from = c(Pct_Cov, Txt_Cov),
                                               values_fill = list(Pct_Cov = 0, Txt_Cov = "0%"))
    # note that values_fill only fills non-existent combinations with 0 or 0%. NAs already in the data remain NA.

  quadchr_comb <- full_join(quad_sum, quadchr_wide,
                            by = intersect(names(quad_sum), names(quadchr_wide)))

  quadchr_comb2 <- left_join(plot_events, quadchr_comb,
                             by = intersect(names(plot_events), names(quadchr_comb)))

  # select columns based on specified valueType
  req_cols <- c("Plot_Name", "Network", "ParkUnit", "ParkSubUnit", "PlotTypeCode", "PanelCode",
                "PlotCode", "PlotID", "EventID", "IsQAQC", "StartYear", "StartDate", "cycle",
                "CharacterLabel", "num_quads", "num_trampled", "quad_avg_cov", "quad_pct_freq")

  pct_cols <- c("Pct_Cov_UC", "Pct_Cov_UR", "Pct_Cov_MR", "Pct_Cov_BR",
                "Pct_Cov_BC", "Pct_Cov_BL", "Pct_Cov_ML", "Pct_Cov_UL")

  txt_cols <- c("Txt_Cov_UC", "Txt_Cov_UR", "Txt_Cov_MR", "Txt_Cov_BR",
                "Txt_Cov_BC", "Txt_Cov_BL", "Txt_Cov_ML", "Txt_Cov_UL")

  # Need to reset PMs to NA after pivot_wider
  invisible(lapply(seq_along(pct_cols), function(x){
    quadchr_comb2[,pct_cols[[x]]][quadchr_comb2[,txt_cols[[x]]] == "Permanently Missing"] <- NA
  }))

  quadchr_comb2$Pct_Cov_UC[quadchr_comb2$Txt_Cov_UC == "Permanently Missing"] <- NA

  quadchr_final <- switch(valueType,
                          "midpoint" = quadchr_comb2[, c(req_cols, pct_cols)],
                          "classes" = quadchr_comb2[, c(req_cols, txt_cols)],
                          "all" = quadchr_comb2[, c(req_cols, pct_cols, txt_cols)])

  return(data.frame(quadchr_final))

  } # end of function

