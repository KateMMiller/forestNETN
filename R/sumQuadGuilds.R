#' @include joinQuadData.R
#' @title sumQuadGuilds: summarizes quadrat species data by guilds
#'
#' @importFrom dplyr all_of case_when filter first full_join group_by mutate select summarize
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider pivot_longer
#'
#' @description This function summarizes output from joinQuadData and calculates average cover and quadrat frequency for each guild.
#' Average cover is corrected for number of quadrats sampled. Guilds are tree, shrub, forb, fern, and graminoid. If herbaceous guild
#' is split, then cover of ferns does not overlap with cover of herbaceous. If herbaceous guild is not split, then cover of herbaceous
#' guild includes fern and other herbaceous (but not graminoid) species cover. Only works for complete events, but does include plots
#' where a few quadrats were not sampled. Germinants are not included in summary.
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
#'   \item{FALSE}{Default. Only returns visits that are not QAQC visits}
#'   \item{TRUE}{Returns all visits, including QAQC visits}
#' }
#'
#' @param locType Allows you to only include plots that are part of the GRTS sample design or
#' include all plots, such as deer exclosures.
#' \describe{
#' \item{"VS"}{Only include plots that are part of the Vital Signs GRTS sample design}
#' \item{"all"}{Include all plots, such as plots in deer exclosures or test plots.}}
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
#' @param splitHerb TRUE/FALSE. If TRUE (Default), splits the herbaceous group into forb and fern. If FALSE,
#' then resulting data frame will be summarized for tree, shrub, herbaceous, and graminoid guilds.
#'
#' @param ... Other arguments passed to function.
#'
#' @return Returns a data frame with average quadrat cover, percent quadrat frequency and quadrat
#' frequency count for tree, shrub/vine, herbaceous, and graminoid for each plot visit. Data are either
#' summarized for all species, native only, exotic only, or invasive only.
#'
#' @examples
#' \dontrun{
#' importData()
#'
#' # compile invasive quad data for all parks and most recent survey. Keep ferns in with herbs
#' inv_guilds <- sumQuadGuilds(speciesType = 'invasive', from = 2015, to = 2018, splitHerb = FALSE)
#'
#' # compile native quad data for more recent survey in ACAD, with ferns and forbs split in separate guilds
#' ACAD_guilds <- sumQuadGuilds(speciesType = 'native', from = 2015, to = 2018, splitHerb = TRUE)
#'}
#'
#' @export
#'
#------------------------
# Joins quadrat tables and filters by park, year, and plot/visit type
#------------------------
sumQuadGuilds <- function(park = 'all', from = 2006, to = 2021, QAQC = FALSE, panels = 1:4,
                          locType = c('VS', 'all'), speciesType = c('all', 'native', 'exotic', 'invasive'),
                          splitHerb = TRUE, ...){
    # Match args and class
    park <- match.arg(park, several.ok = TRUE,
                      c("all", "ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA"))
    stopifnot(class(from) == "numeric", from >= 2006)
    stopifnot(class(to) == "numeric", to >= 2006)
    stopifnot(class(QAQC) == 'logical')
    stopifnot(class(splitHerb) == 'logical')
    stopifnot(panels %in% c(1, 2, 3, 4))
    locType <- match.arg(locType)
    speciesType <- match.arg(speciesType)

    # Prepare the quadrat data
    quad_evs <- suppressWarnings(joinQuadSpecies(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                                locType = locType, eventType = 'complete', speciesType = speciesType,
                                valueType = 'midpoint')) %>%
                filter(!TSN %in% -9999999950) %>% # Drops "Unknown species" which can't be fit into a group
                filter(IsGerminant == 0 | is.na(IsGerminant))

    # Combine TreeShrub and Vines into shrub group. Note Vines group only includes woody spp.
    quad_evs <- quad_evs %>% mutate(Shrub = ifelse(TreeShrub == 1 | Vine == 1, 1, Shrub)) %>%
                             select(-TreeShrub, - Vine)

    quad_evs2 <- if(splitHerb == TRUE){
      quad_evs %>% mutate(group = case_when(Tree == 1 ~ "Tree",
                                            Shrub == 1 ~ "Shrub",
                                            Herbaceous == 1 & FernAlly == FALSE ~ "Herbaceous",
                                            Graminoid == 1 ~ "Graminoid",
                                            FernAlly == TRUE ~ "Fern",
                                            TRUE ~ "Unk"))
    } else if(splitHerb == FALSE){
      quad_evs %>% mutate(group = case_when(Tree == 1 ~ "Tree",
                                            Shrub == 1 ~ "Shrub",
                                            Herbaceous == 1 ~ "Herbaceous",
                                            Graminoid == 1 ~ "Graminoid",
                                            TRUE ~ "Unk"))
    }

    quad_sum <- quad_evs2 %>% group_by(Plot_Name, ParkUnit, PlotID, EventID, IsQAQC,
                                       SampleYear, SampleDate, cycle, group) %>%
                              summarize(pct_UC = sum(Pct_Cov_UC, na.rm = T),
                                        pct_UR = sum(Pct_Cov_UR, na.rm = T),
                                        pct_MR = sum(Pct_Cov_MR, na.rm = T),
                                        pct_BR = sum(Pct_Cov_BR, na.rm = T),
                                        pct_BC = sum(Pct_Cov_BC, na.rm = T),
                                        pct_BL = sum(Pct_Cov_BL, na.rm = T),
                                        pct_ML = sum(Pct_Cov_ML, na.rm = T),
                                        pct_UL = sum(Pct_Cov_UL, na.rm = T),
                                        quad_avg_cov = (pct_UC + pct_UR + pct_MR + pct_BR +
                                                        pct_BC + pct_BL + pct_ML + pct_UL)/ first(num_quads),
                                        UC = ifelse(pct_UC > 0, 1, 0),
                                        UR = ifelse(pct_UR > 0, 1, 0),
                                        MR = ifelse(pct_MR > 0, 1, 0),
                                        BR = ifelse(pct_BR > 0, 1, 0),
                                        BC = ifelse(pct_BC > 0, 1, 0),
                                        BL = ifelse(pct_BL > 0, 1, 0),
                                        ML = ifelse(pct_ML > 0, 1, 0),
                                        UL = ifelse(pct_UL > 0, 1, 0),
                                        quad_pct_freq = 100*(UC + UR + MR + BR +
                                                             BC + BL + ML + UL)/ first(num_quads),
                                        .groups = 'drop') %>%
                               select(Plot_Name:cycle, group, quad_avg_cov, quad_pct_freq)

    # Need to expand over all group levels, so each plot has all of the groups to facilitate summary stats
    groups <- if(splitHerb == TRUE){c("Tree", "Shrub", "Herbaceous", "Graminoid", "Fern")
      } else if(splitHerb == FALSE) {c("Tree", "Shrub", "Herbaceous", "Graminoid")}

    quad_full_cov <- quad_sum %>% select(-quad_pct_freq) %>%
                                  pivot_wider(names_from = group,
                                              values_from = quad_avg_cov,
                                              values_fill = 0)

    # check for and fix any missing group columns
    missing_groups <- setdiff(groups, names(quad_full_cov))
    quad_full_cov[missing_groups] <- 0

    quad_full_cov2 <- quad_full_cov %>% pivot_longer(cols = all_of(groups),
                                                     names_to = "Group",
                                                     values_to = "quad_pct_cover")

    quad_full_freq <- quad_sum %>% select(-quad_avg_cov) %>%
                                   pivot_wider(names_from = group,
                                               values_from = quad_pct_freq,
                                               values_fill = 0)
    quad_full_freq[missing_groups] <- 0

    quad_full_freq2 <- quad_full_freq %>% pivot_longer(cols = all_of(groups),
                                                       names_to = "Group",
                                                       values_to = "quad_pct_freq")

    # Combine cover and freq data
    quad_full <- full_join(quad_full_cov2, quad_full_freq2,
                           by = intersect(names(quad_full_cov2), names(quad_full_freq2)))

    quad_final <- if("Unk" %in% names(quad_full)){quad_full %>% select(-Unk)
      } else {quad_full}

    return(data.frame(quad_final))
} # end of function

