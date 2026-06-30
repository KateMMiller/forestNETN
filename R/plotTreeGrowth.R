#' @include joinLocEvent.R
#' @include joinTreeData.R
#'
#' @title plotTreeGrowth: plot individual tree growth and mortality
#'
#' @importFrom dplyr arrange case_when lag left_join mutate select
#' @importFrom tidyr fill
#' @import ggplot2
#'
#' @description This function plots individual tree growth over time for all trees that were alive on their
#' first record in the plot.
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
#' @param subunit Filter park data by subunit. Must match units within a park. Valid inputs:
#' ACAD: "Isle_au_Haut", "MDI_East", "MDI_West", "Schoodic"
#' MIMA: "Battle_Road", "North_Bridge"
#' MORR: "Jockey_Hollow", "NJ_Brigade"
#' ROVA: "ELRO" for Eleanor Roosevelt NHS; "HOFR_East" for "Home of FDR - East/Farm Road";
#'  "HOFR_W" for Home of FDR NHS; "VAMA" for Vanderbilt Mansion NHS.
#'
#' @param from Year to start analysis, ranging from 2006 to current year
#' @param to Year to stop analysis, ranging from 2006 to current year
#'
#' @param locType Allows you to only include plots that are part of the GRTS sample design or include all plots,
#' such as deer exclosures
#' \describe{
#' \item{"VS"}{Only include plots that are part of the Vital Signs GRTS sample design}
#' \item{"all"}{Include all plots, such as plots in deer exclosures or test plots.}}
#'
#' @param panels Allows you to filter individual panels from 1 to 4. Default is all 4 panels (1:4).
#' If more than one panel is selected, specify by c(1, 3), for example.
#'
#' @param species Allows you to filter on species
#'
#' @param plotName Allows you to select a specific plot to run function for. Value inputs are "PARK-###", like "ACAD-001".
#' If no plot name is specified, or multiple plots are specified, the function will facet on plotName.
#'
#'@return Returns a plot of tree growth rates over time, color coded by rate and whether a tree died.
#'
#' @examples
#' \dontrun{
#' importData()
#'
#' # make plot of red spruce for plots on Schoodic in panel 1
#' plotTreeGrowth(park = "ACAD", subunit = "Schoodic", panels = 1, species = "Picea rubens")
#' plotTreeGrowth(park = "ACAD", subunit = "Schoodic", panels = 1)
#'
#' # Eastern MDI and balsam fir
#' plotTreeGrowth(park = "ACAD", subunit = "MDI_East", species = "Abies balsamea", panels = 1)
#'
#' # Specific plots
#' plotTreeGrowth(plotName = c("ACAD-035", "ACAD-046"), species = "Abies balsamea")
#'
#' # Plot beech for MORR with vertical line where BLD was detected
#' plotTreeGrowth(park = "MORR", from = 2006, to = 2026, species = "Fagus grandifolia", panels = 4) +
#'   geom_vline(xintercept = 2019, lty = 'dashed')
#'
#' # Plot multiple species of oaks in MORR
#' plotTreeGrowth(park = "MORR", from = 2006, to = 2026, panels = 4,
#'   species = c("Quercus rubra", "Quercus velutina", "Quercus alba", "Quercus montana"))
#'
#' # Plot hemlock in ROVA
#' plotTreeGrowth(park = "ROVA", species = "Tsuga canadensis")
#'
#' # Plot ash in SARA, panel 3
#' plotTreeGrowth(park = "SARA",
#'   species = c("Fraxinus americana", "Fraxinus pennsylvanica", "Fraxinus nigra"), panels = 3)
#'
#'
#' }
#'
#' @export
#'
plotTreeGrowth <- function(park = 'all', from = 2006, to = as.numeric(format(Sys.Date(), "%Y")),
                           locType = c('VS', 'all'), panels = 1:4, subunit = NA,
                           plotName = NA_character_, species = NA_character_, subuni){

  if(!requireNamespace("ggrepel", quietly = TRUE)){
    stop("Package 'ggrepel' needed for this function to work. Please install it.", call. = FALSE)
  }

  if(!requireNamespace("cowplot", quietly = TRUE)){
    stop("Package 'cowplot' needed for this function to work. Please install it.", call. = FALSE)
  }

  # Match args and class
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA"))
  stopifnot(class(from) == "numeric", from >= 2006)
  stopifnot(class(to) == "numeric", to >= 2006)
  stopifnot(panels %in% c(1, 2, 3, 4))
  locType <- match.arg(locType)
  stopifnot(nchar(plotName) == 8 | is.na(plotName))

  # compile data
  trees_ind <- joinTreeData(park = park, from = from, to = to, QAQC = FALSE, panels = panels,
                            locType = locType, status = "active")

  live <- c("1", "AB", "AF", "AL", "AM", "AS", "RB", "RF", "RL", "RS")
  dead <- c("2", "DB", "DF", "DL", "DM", "DS")

  # filter on fxn args
  trees_ind1a <- if(any(!is.na(plotName))){filter(trees_ind, Plot_Name %in% plotName)
    } else {trees_ind}

  if(nrow(trees_ind1a) == 0){stop("Specified plotName resulted in a data frame with no records.
                                   Check plot spelling or that park, year range, etc. will return records.")}

  trees_ind1b <- if(any(!is.na(species))){filter(trees_ind1a, ScientificName %in% species)
  } else {trees_ind1a}


  if(nrow(trees_ind1b) == 0){stop("Specified species resulted in a data frame with no records.
                                  Check species spelling or that park, year range, etc. will return records.")}

  trees_ind1b$SubUnit <- substr(trees_ind1b$ParkSubUnit, 6, nchar(trees_ind1b$ParkSubUnit))

  trees_ind1c <- if(any(!is.na(subunit))){filter(trees_ind1b, SubUnit %in% subunit)
    } else {trees_ind1b}

  if(nrow(trees_ind1c) == 0){stop("Specified park subunit resulted in a data frame with no records.
                                  Check subunit spelling or that park, year range, etc. will return records.")}

  plot_evs <- joinLocEvent(park = park, from = from, to = to, QAQC = FALSE, panels = panels,
                          locType = locType) |>
              select(Plot_Name, SampleYear, cycle, IsStuntedWoodland)

  #+++ TO do +++
  # Adjust growth rate to annual for non-equal years
  # Add ability to remove windthrow
  # Add ability to only view plots with elevated mortality.

  trees_ind2 <- left_join(trees_ind1c, plot_evs, by = c("Plot_Name", "cycle", "SampleYear")) |>
    filter(IsStuntedWoodland == FALSE) |> # might turn off
    mutate(tag = sprintf("%02d", TagCode),
           tree_id = paste0(Plot_Name, "-", tag),
           status = ifelse(TreeStatusCode %in% live, "live",
                           ifelse(TreeStatusCode %in% dead, "dead",
                                  "unk")),
           subunit = sub("ACAD_", "", ParkSubUnit)) |>
    select(Plot_Name, tree_id, subunit, PanelCode, status, TreeStatusCode,
           SampleYear, cycle, ScientificName, DBHcm) |>
    arrange(Plot_Name, tree_id, SampleYear)  |>
    mutate(dbh_fill = DBHcm) |>
    tidyr::fill(dbh_fill, .direction = 'down', .by = c(Plot_Name, tree_id)) |> # fills for dead trees missing DBH
    mutate(dbh_prev = dplyr::lag(dbh_fill, 1), .by = c(Plot_Name, tree_id),
           dbh_growth = dbh_fill - dbh_prev)

  # Remove trees that started dead and after first dead record
  trees_ind3 <- trees_ind2 |>
    mutate(prev_stat = lag(status, 1),
           .by = c(Plot_Name, tree_id, subunit, PanelCode, ScientificName)) |>
    mutate(first_stat = first(status), .by = c(Plot_Name, tree_id)) |>
    filter(!prev_stat %in% 'dead') |> filter(!first_stat %in% "dead") |>
    mutate(windthrow1 = ifelse(grepl("B", TreeStatusCode), 1, 0)) |>
    # drop trees that died from windthrow
    mutate(windthrow = ifelse(sum(windthrow1 > 0), 1, 0),
           .by = c(Plot_Name, tree_id)) #|>
  #filter(windthrow == 0)

  # Select plots that have more than 1 tree die since 2006
  trees_ind4 <- trees_ind3 |>
    mutate(num_live = sum(status == "live"),
           num_dead = sum(status == 'dead'),
           elev_mort = ifelse(num_dead/num_live > 0.05, 1, 0),
           .by = c(Plot_Name, SampleYear, cycle)) |>
    mutate(elev_mort = ifelse(sum(elev_mort > 0), 1, 0),
           .by = Plot_Name) |>
    mutate(dbh_growth_prev = lead(dbh_growth, 1), .by = c(Plot_Name, tree_id)) |>
    mutate(growth_color = factor(case_when(dbh_growth_prev < 0 ~ "shrink",
                                           dbh_growth_prev == 0 ~ "no growth",
                                           dbh_growth_prev > 0 & dbh_growth_prev < 0.5 ~ "slow",
                                           dbh_growth_prev >= 0.5 & dbh_growth_prev < 1 ~  "moderate",
                                           dbh_growth_prev >= 1 ~ "fast"),
                                 levels = c("shrink", "no growth", "slow", "moderate", "fast")))


  #table(trees_ind4$elev_mort, trees_ind4$ScientificName)

  facet_plot <- if(length(unique(trees_ind4$Plot_Name)) > 0) {TRUE} else {FALSE}

  p <-
  suppressWarnings(
  ggplot(trees_ind4, aes(x = SampleYear, y = dbh_fill, group = tree_id)) +
    theme_FHM() +
    geom_line(aes(color = growth_color), linewidth = 1) +
    geom_point(aes(shape = status, size = status), stroke = 1) + #geom_smooth(span = 0.8, se = F) +
    scale_shape_manual(values = c("dead" = "X", "live" = "|"), name = 'Status at measurement') +
    scale_size_manual(values = c("dead" = 3, "live" = 2), name = 'Status at measurement') +
    scale_color_manual(values = c("#d7191c", "#fdae61", "#e6f598","#78c679", "#238443"),
                       breaks = c("shrink", "no growth", "slow", "moderate", "fast"),
                       labels = c("shrink: < 0cm", "no growth: 0 cm", "slow: 0.1 - 0.4 cm",
                                  "moderate: 0.5 - 1 cm", "fast: > 1 cm"),
                       name = 'Growth Rate per 4 years') +
    {if(facet_plot == TRUE){facet_wrap(~Plot_Name)}} +
    scale_x_continuous(breaks = c(seq(2006, 2026, 4), 2026),
                       limits = c(2005.9, 2026.1)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(y = "DBH (cm)", x = NULL)
    )

  return(p)

}
