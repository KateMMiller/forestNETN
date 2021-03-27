#' @include joinLocEvent.R
#' @include prepTaxa.R
#'
#' @title joinQuadSpecies: compiles quadrat species data
#'
#' @importFrom dplyr group_by filter full_join select summarize ungroup
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
#' }
#'
#' @return Returns a dataframe with cover class midpoints for each quadrat and includes guild for each species.
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
                            valueType = c('all', 'midpoint', 'classes'), ...){
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

  env <- if(exists("VIEWS_NETN")){VIEWS_NETN} else {.GlobalEnv}

  # Prepare the quadrat data
  tryCatch(quadspp <- get("NETN_QuadSpecies", envir = env) %>%
             select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, SQQuadSppCode,
                    QuadratCode, TSN, ScientificName, CoverClassCode, CoverClassLabel, IsGerminant,
                    ConfidenceClassCode, IsCollected, QuadSppNote),
           error = function(e){stop("NETN_QuadSpecies view not found. Please import view.")})

  taxa_wide <- force(prepTaxa())[, c("TSN", "ScientificName", "Exotic", "InvasiveNETN")]

  # subset with EventID from plot_events to make function faster
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short')) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
           xCoordinate, yCoordinate, EventID, StartDate, StartYear, cycle, IsQAQC)

  pe_list <- unique(plot_events$EventID)

  quadspp_evs <- filter(quadspp, EventID %in% pe_list)
  names(quadspp_evs)[names(quadspp_evs) == "ConfidenceClassCode"] <- "Confidence"

  # join with taxa data, so can filter for smaller dataset early
  quadspp_tax <- left_join(quadspp_evs, taxa_wide, by = c("TSN", "ScientificName"))

  quadspp_filt <- if(speciesType == 'native'){
    filter(quadspp_tax, Exotic == FALSE)
  } else if(speciesType == 'exotic') {
    filter(quadspp_tax, Exotic == TRUE)
  } else if(speciesType == 'invasive'){
    filter(quadspp_tax, InvasiveNETN == TRUE)
  } else if(speciesType == 'all'){
    quadspp_tax
  }

  quad_check <- quadspp_filt %>% group_by(ParkUnit, PlotCode, StartYear, IsQAQC,
                                          TSN, ScientificName, IsGerminant,
                                          QuadSppNote) %>%
                                 summarize(num_quads = sum(SQQuadSppCode == "SS", na.rm = T),
                                 .groups = 'drop') %>%
                                 filter(num_quads > 8) %>% select(-num_quads)
  if(nrow(quad_check) > 0){
    warning(paste("There are", nrow(quad_check),
                  "duplicate species records. Function dropped 2nd occurrance of duplicate species: \n"),
            paste(capture.output(data.frame(quad_check)), collapse = "\n"))
    }

  # Clean this up after data are fixed in next release... code is seriously slow
  quadspp_fix <- if(nrow(quad_check) > 0){
    quadspp_filt %>% group_by(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC,
                             SQQuadSppCode, QuadratCode, TSN, ScientificName, IsGerminant, QuadSppNote) %>%
                             slice(1)
    } else {quadspp_filt}

  quadspp_fix <- quadspp_fix[, names(quadspp_filt)]

  # Pick back up here after release is fixed
  quadspp_fix$CovClass_num <- suppressWarnings(as.numeric(quadspp_fix$CoverClassCode))
  quadspp_fix$Pct_Cov <- as.numeric(NA)
  quadspp_fix$Pct_Cov[quadspp_fix$CovClass_num == 0] <- 0
  quadspp_fix$Pct_Cov[quadspp_fix$CovClass_num == 1] <- 0.1
  quadspp_fix$Pct_Cov[quadspp_fix$CovClass_num == 2] <- 1.5
  quadspp_fix$Pct_Cov[quadspp_fix$CovClass_num == 3] <- 3.5
  quadspp_fix$Pct_Cov[quadspp_fix$CovClass_num == 4] <- 7.5
  quadspp_fix$Pct_Cov[quadspp_fix$CovClass_num == 5] <- 17.5
  quadspp_fix$Pct_Cov[quadspp_fix$CovClass_num == 6] <- 37.5
  quadspp_fix$Pct_Cov[quadspp_fix$CovClass_num == 7] <- 62.5
  quadspp_fix$Pct_Cov[quadspp_fix$CovClass_num == 8] <- 85
  quadspp_fix$Pct_Cov[quadspp_fix$CovClass_num == 9] <- 97.5
  quadspp_fix$Txt_Cov <- NA
  quadspp_fix$Txt_Cov <- ifelse(quadspp_fix$CoverClassCode == "-<1%", "<1%", quadspp_fix$CoverClassCode)
  quadspp_fix$Txt_Cov[is.na(quadspp_fix$ScientificName)] <- "Permanently Missing"
  quadspp_fix$Sampled <- ifelse(quadspp_fix$SQQuadSppCode == "SS", 1, 0)

  names(quadspp_fix)
  # Calculate average % cover and frequency
  quad_sum <- quadspp_fix %>% group_by(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear,
                                        IsQAQC, TSN, ScientificName, IsGerminant, QuadSppNote) %>%
                               summarize(num_quads = sum(Sampled, na.rm = T),
                                         quad_avg_cov = sum(Pct_Cov, na.rm = T)/num_quads,
                                         quad_pct_freq = (sum(Pct_Cov > 0, na.rm = T)/num_quads)*100,
                                         .groups = 'drop') %>%
                               ungroup()

  # Spread quadrats wide
  quadspp_wide <- quadspp_fix %>% pivot_wider(names_from = QuadratCode,
                                               values_from = c(Pct_Cov, Txt_Cov),
                                               values_fill = list(Pct_Cov = 0, Txt_Cov = "0%"))

  quadspp_comb <- full_join(quadspp_wide, quad_sum,
                            intersect(names(quadspp_wide), names(quad_sum)))

  quadspp_comb2 <- left_join(plot_events, quadspp_comb,
                             by = intersect(names(plot_events), names(quadspp_comb)))

  # select columns based on specified valueType
  req_cols <- c("Plot_Name", "Network", "ParkUnit", "ParkSubUnit", "PlotTypeCode", "PanelCode",
                "PlotCode", "PlotID", "EventID", "IsQAQC", "StartYear", "cycle",
                "TSN", "ScientificName", "Confidence", "IsGerminant", "num_quads",
                "quad_avg_cov", "quad_pct_freq")

  pct_cols <- c("Pct_Cov_UC", "Pct_Cov_UR", "Pct_Cov_MR", "Pct_Cov_BR",
                "Pct_Cov_BC", "Pct_Cov_BL", "Pct_Cov_ML", "Pct_Cov_UL")

  txt_cols <- c("Txt_Cov_UC", "Txt_Cov_UR", "Txt_Cov_MR", "Txt_Cov_BR",
                "Txt_Cov_BC", "Txt_Cov_BL", "Txt_Cov_ML", "Txt_Cov_UL")

  # Need to reset PMs to NA after pivot_wider
  # invisible(lapply(seq_along(pct_cols), function(x){
  #   quadchr_comb2[,pct_cols[[x]]][quadchr_comb2[,txt_cols[[x]]] == "Permanently Missing"] <- NA
  # }))
  #
  # quadchr_comb2$Pct_Cov_UC[quadchr_comb2$Txt_Cov_UC == "Permanently Missing"] <- NA

  quadspp_comb2$ScientificName[is.na(quadspp_comb2$ScientificName)] <- "None present"

  #++++++++++++++++++++++++++++++
  # ENDED HERE- need to add 0s to % cover and average/freq in the None present

  quadspp_final <- if(valueType == "midpoint"){
    quadspp_comb2[, c(req_cols, pct_cols, "QuadSppNote")]
  } else if(valueType == "classes"){
    quadspp_comb2[, c(req_cols, txt_cols, "QuadSppNote")]
  } else if(valueType == "all"){
    quadspp_comb2[, c(req_cols, pct_cols, txt_cols, "QuadSppNote")]
  }


  } # end of function

