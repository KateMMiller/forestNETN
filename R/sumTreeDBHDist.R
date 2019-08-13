#' @include joinLocEvent.R
#' @include joinTreeData.R
#' @title sumTreeDBHDist: calculates DBH distribution of trees
#'
#'
#' @importFrom dplyr select filter arrange mutate summarise group_by case_when
#' @importFrom magrittr %>%
#'
#' @description This function calculates DBH distribution by 10cm size classes. Must run importData first.
#'
#' @param status Filter by live, dead, or all. Acceptable options are:
#' \describe{
#' \item{"all"}{Includes all standing trees}
#' \item{"live"}{live trees only}
#' \item{"dead"}{dead trees only}
#' }
#'
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' }
#'
#'@param units Allows you to choose which metric to calculate: basal area or stem density
#'\describe{
#'\item{"density"}{Default. Returns stems/ha}
#'\item{"ba"}{Returns basal area in sq.m/ha}
#' }
#'
#' @param dist_m Filter trees by a distance that is less than or equal to the specified distance in meters
#' of the tree to the center of the plot. If no distance is specified, then all trees will be selected. For
#' example, to select an area of trees that is 100 square meters in area, use a distance of 5.64m.
#'
#' @return returns a dataframe with one row for each plot and either density or BA
#'
#' @examples
#' importData()
#' tree_diam_dist <-sumTreeDBHDist(park = 'MORR', speciesType = 'native', from = 2016, to = 2019, units = 'ba')
#' head(tree_diam_dist)
#'
#' @export
#'
#------------------------
# Calculates tree diameter distribution
#------------------------
sumTreeDBHDist<-function(status = c('all', 'live','dead'), speciesType = c('all', 'native','exotic'),
                         units = c('density', 'ba'), park = 'all',
                         from = 2006, to = 2019, QAQC = FALSE, locType = 'VS',
                         panels = 1:4, dist_m = NA, output, ...){

  status <- match.arg(status)
  speciesType <- match.arg(speciesType)
  units <- match.arg(units)

  park.plots <- force(joinLocEvent(park = park, from = from, to = to, QAQC = QAQC,
                                   locType = locType, panels = panels, output = 'short'))

  tree_df <- force(joinTreeData(park = park, from = from, to = to, QAQC = QAQC, speciesType = speciesType,
                   locType = locType, panels = panels, dist_m = dist_m, status = status))

  alive<-c("1", "AB", "AF", "AL" ,"AM" ,"AS", "RB", "RF", "RL", "RS")
  dead<-c("2","DB" ,"DL", "DM","DS")
  exclude<-c('EX','ES','DC','DF','XO','XP','XS')

  tree_df2<- if (status == 'live') {filter(tree_df, Status_ID %in% alive) %>% droplevels()
  } else if (status == 'dead') {filter(tree_df, Status_ID %in% dead) %>% droplevels()
  } else if (status == 'all') {filter(tree_df, !Status_ID %in% exclude) %>% droplevels()
  }

  tree_df3 <- tree_df2 %>% mutate(size_class= as.factor(case_when(between(DBH, 10, 19.9)~ 'd10_19.9',
                                                                     between(DBH, 20, 29.9)~ 'd20_29.9',
                                                                     between(DBH, 30, 39.9)~ 'd30_39.9',
                                                                     between(DBH, 40, 49.9)~ 'd40_49.9',
                                                                     between(DBH, 50, 59.9)~ 'd50_59.9',
                                                                     between(DBH, 60, 69.9)~ 'd60_69.9',
                                                                     between(DBH, 70, 79.9)~ 'd70_79.9',
                                                                     between(DBH, 80, 89.9)~ 'd80_89.9',
                                                                     between(DBH, 90, 99.9)~ 'd90_99.9',
                                                                     DBH>=100 ~ 'd100p',
                                                                     TRUE ~ 'unknown')),
                                     stem = 1,
                                     unit_conv = ifelse(Unit_Code == 'ACAD', 225, 400))

# In case there's a size class not represented


tree_dist <- tree_df3 %>% group_by(Event_ID, Plot_Name, size_class, unit_conv) %>%
                            summarise(num_stems_ha = sum(stem)*10000/first(unit_conv),
                                      BA_m2ha = sum(BA_cm2)/first(unit_conv))

tree_dist_wide <- if (units=='density') {
                tree_dist %>% select(Event_ID, Plot_Name, size_class, num_stems_ha) %>%
                              spread(size_class, num_stems_ha, fill = 0)
                } else if (units=='ba') {
                tree_dist %>% select(Event_ID, Plot_Name, size_class, BA_m2ha) %>%
                              spread(size_class, BA_m2ha, fill = 0)
                }

# next few lines find if a size class is missing, and adds it later
sizes=c('d10_19.9', 'd20_29.9', 'd30_39.9', 'd40_49.9',
        'd50_59.9', 'd60_69.9', 'd70_79.9', 'd80_89.9',
        'd90_99.9', 'd100p', 'unknown')

missing_sizes <- setdiff(sizes, names(tree_dist_wide))

tree_dist_wide[missing_sizes] <- 0

tree_dist_final <- merge(park.plots, tree_dist_wide, by=c('Event_ID', 'Plot_Name'), all.x=T) %>%
                   select(Location_ID, Event_ID, Plot_Name, Unit_Code:cycle,
                          d10_19.9, d20_29.9, d30_39.9, d40_49.9, d50_59.9, d60_69.9,
                          d70_79.9, d80_89.9, d90_99.9, d100p, unknown) %>% arrange(Plot_Name, cycle)

return(tree_dist_final)

} # end of function

