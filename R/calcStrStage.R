#' @include joinLocEvent.R
#' @include joinStandData.R
#' @include joinTreeData.R
#'
#' @importFrom dplyr mutate summarise case_when filter group_by arrange everything
#' @importFrom magrittr %>%
#'
#' @title calcStrStage: calculate structural stage for each plot
#'
#' @description This function calculates structural stage metric from Ecological Integrity Scorecard,
#' which assigns Pole, Mature, Late Successional, or Mosaic (i.e., none of the above) to each plot based
#' on the percent of live basal area of canopy trees in pole, mature and large size classes. Plots must be
#' closed-canopy forest to be classified for this metric. Therefore plots classified as Woodlands (ACAD only)
#' or Early successional (SARA only) in the field are automartically assigned those classes in the calculation.
#' Must run importData first.
#'
#' @param park Combine data from all parks or one park at a time. Acceptable options are:
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
#' @param from Year to start analysis, ranging from 2006-2019
#' @param to Year to stop analysis, ranging from 2006-2019
#'
#' @param QAQC Allows you to remove or include QAQC events.
#' \describe{
#' \item{FALSE}{Default. Only returns visits that are not QAQC visits}
#' \item{TRUE}{Returns all visits, including QAQC visits}}
#'
#' @param locType Allows you to only include plots that are part of the GRTS sample design or include all plots, such as deer exclosures
#' \describe{
#' \item{"VS"}{Default. Only include plots that are part of the Vital Signs GRTS sample design}
#' \item{"all"}{Include all plots, such as deer exclosures and bonus plots}}
#' @param panels Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
#' If more than one panel is selected, specify by c(1,3), for example.
#'
#' #' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' }
#'
#'
#' @return returns a dataframe with structural stage and metrics used to assign stages to plots.
#'
#' @examples
#' importData() #imports using default odbc
#' stage_df <- calcStrStage(park = 'MABI', from = 2016, to = 2019)
#'
#'
#' @export
#'
#------------------------
# Join tree data
#------------------------
calcStrStage<-function(park = 'all', QAQC = FALSE, locType = 'VS', panels = 1:4,
                       from = 2006, to = 2019, output, speciesType = c('all', 'native', 'exotic'), ...){

  park.plots <- force(joinLocEvent(park = park, from = from, to = to, QAQC = QAQC,
                                 locType = locType, panels = panels, output = 'short'))

  stand_df <- force(joinStandData(park = park, from = from, to = to, QAQC = QAQC,
                                  locType = locType, panels = panels))

  tree_live <- force(joinTreeData(park = park, from = from, to = to, QAQC = QAQC, speciesType = speciesType,
                                  locType = locType, panels = panels, status = 'live'))

  canopy_trees <- c(2, 3, 4)

  # Set up tree data
  tree_stand_str <- tree_live %>% filter(Crown_Class_ID %in% canopy_trees) %>%
    mutate(pole_size =    ifelse(Unit_Code == 'ACAD', 20, 26),
           mature_size =  ifelse(Unit_Code == 'ACAD', 34.9, 45.9),
           BA_pole = ifelse(DBH < pole_size, BA_cm2, 0),
           BA_mature = ifelse(DBH >= pole_size & DBH < mature_size, BA_cm2, 0),
           BA_large = ifelse(DBH > mature_size, BA_cm2, 0))

  # Summarize to plot-level
  stand_str <- tree_stand_str %>% group_by(Event_ID, Plot_Name) %>%
    summarise(BA_tot = sum(BA_cm2),
              pctBA_pole = sum(BA_pole) / BA_tot * 100,
              pctBA_mature = sum(BA_mature) / BA_tot * 100,
              pctBA_large = sum(BA_large) / BA_tot * 100
    ) %>% ungroup()

  # Add in stand structure, so woodlands and early successional are not part of stage calculation
  stand_str2 <- merge(stand_str, stand_df[,c('Event_ID','Plot_Name','Stand_Structure')],
                      by=c('Event_ID','Plot_Name'), all.x=T)

  stand_str3 <- stand_str2 %>%
    mutate(Stage = case_when(Stand_Structure == 'Woodland (ACAD only)' ~ 'Woodland',
                             Stand_Structure == 'Early successional' ~ 'Early_successional',
                             pctBA_pole + pctBA_mature >= 67 & pctBA_pole > pctBA_mature ~ 'Pole',
                             pctBA_pole + pctBA_mature >= 67 & pctBA_pole <= pctBA_mature | pctBA_mature >= 67 ~ 'Mature',
                             pctBA_mature + pctBA_large >= 67 & pctBA_large > pctBA_mature ~ 'Late_successional',
                             TRUE ~ 'Mosaic')
    )

  stand_str4 <- merge(park.plots, stand_str3, by=c('Event_ID','Plot_Name'), all.x=T)

  stand_str_final <- stand_str4 %>% select(Location_ID, Event_ID, Plot_Name, everything()) %>%
    arrange(Plot_Name, Year)

  }
