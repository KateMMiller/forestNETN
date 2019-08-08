#' @include joinLocEvent.R
#'
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#'
#' @title joinStandData: compile stand data
#'
#' @description This function combines stand data for each plot. Must run importData first.
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
#' @param from Year to start analysis, ranging from 2006-2019
#' @param to Year to stop analysis, ranging from 2006-2019
#' @param QAQC Allows you to remove or include QAQC events.
#' \describe{
#' \item{FALSE}{Default. Only returns visits that are not QAQC visits}
#' \item{TRUE}{Returns all visits, including QAQC visits}}
#' @param locType Allows you to only include plots that are part of the GRTS sample design or include all plots, such as deer exclosures
#' \describe{
#' \item{"VS"}{Default. Only include plots that are part of the Vital Signs GRTS sample design}
#' \item{"all"}{Include all plots, such as deer exclosures and bonus plots}}
#' @param panels Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
#' If more than one panel is selected, specify by c(1,3), for example.
#'
#' @return returns a dataframe with stand data attached to location and event data. Note that tree height measurements are omitted
#' from this function because they are compiled in the sumStandHeight function.
#'
#' @examples
#' importData() #imports using default odbc
#' stand_df <- joinStandData(park = 'MABI', from = 2015, to = 2019)
#'
#'
#' @export
#'
#------------------------
# Join stand table
#------------------------
joinStandData<-function(park='all', QAQC=FALSE, locType='VS', panels=1:4, from=2006, to=2019, output, ...){

  park.plots<-force(joinLocEvent(park=park, from=from,to=to,QAQC=QAQC,
                                 locType=locType, panels=panels, output='short'))
  stand2<-stand %>% select(Event_ID:Crown_Closure_ID, Deer_Browse_Line_ID,
                           Microtopography_ID:Forest_Floor_Trampled_Cover_Class_ID,
                           Stunted_Woodland,Derived_Plot_Slope)

  stand_df<-merge(park.plots, stand2, by='Event_ID', all.x=T)
  return(stand_df)
}
