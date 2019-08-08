#' @include joinLocEvent.R
#'
#' @importFrom dplyr select arrange
#' @importFrom tidyr gather spread
#' @importFrom magrittr %>%
#'
#' @title sumStandHeight: compile stand height data
#'
#' @description This function summarized tree height data for each plot. Must run importData first.
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
#' @return returns a dataframe with stand data attached to location and event data
#'
#' @examples
#' importData() #imports using default odbc
#' stand_df <- sumStandHeight(park = 'MABI', from = 2015, to = 2019)
#'
#'
#' @export
#'
#------------------------
# Summarize tree height data
#------------------------
sumStandHeight<-function(park='all', QAQC=FALSE, locType='VS', panels=1:4, from=2006, to=2019, output, ...){
  park.plots<-force(joinLocEvent(park=park, from=from,to=to,QAQC=QAQC,
                                 locType=locType, panels=panels, output='short'))
  stand2<-stand %>% select(Event_ID, Height_Tree_1_Codom:Height_Tree_3_Inter)

  stand_df<-merge(park.plots, stand2, by='Event_ID', all.x=T)
  stand_long <- stand_df %>% select(Location_ID, Event_ID, Unit_Code, Plot_Name, Plot_Number, Year,
                                    Height_Tree_1_Codom, Height_Tree_2_Codom,
                                    Height_Tree_3_Codom, Height_Tree_1_Inter,
                                    Height_Tree_2_Inter, Height_Tree_3_Inter) %>%
    gather('tree_number', 'height', -Location_ID, -Event_ID,
           -Unit_Code, -Plot_Name, -Plot_Number, -Year) %>%
    arrange(Plot_Name)

  stand_long2<-na.omit(stand_long)
  stand_long2<-stand_long2 %>% mutate(CrownType= ifelse(grepl("Codom", tree_number), "Avg_Codom_HT",'Avg_Inter_HT'))

  stand_sum <- stand_long2 %>% group_by(Location_ID, Event_ID, Unit_Code, Plot_Name, Plot_Number, Year, CrownType) %>%
    summarise(avg_height = round(mean(height, na.rm=T),2)) %>% spread(CrownType, avg_height, fill=NA) %>%
    arrange(Plot_Name, Year)

  return(stand_sum)
}

