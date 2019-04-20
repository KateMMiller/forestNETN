#' @title treeMapPrep: prepares tree data for treeMap (internal function)
#
#' @importFrom dplyr select filter arrange mutate summarise group_by
#' @importFrom magrittr %>%
#'
#' @description This function converts tree distance and azimuth values to coordinates and plots the coordinates of live or
#' dead trees, then simplifies status codes for the map. Works best with data.frame derived
#' from joinLocEvent and joinTreeData. Requires a data.frame with the following fields:
#'  Plot_Name, Tree_Number_NETN, Distance, Azimuth, Orientation, Status_ID, and DBH. This function is used in treeMap().
#'
#' @return Returns a map of trees on a given plot
#'
#' @export
#'
#------------------------
# Plots tree map by status and size
#------------------------
treeMapPrep<-function(df){

  get.coords<-function(df){
    az= ifelse(df$Azimuth-df$Orientation<0,360-df$Orientation+df$Azimuth,df$Azimuth-df$Orientation)
    df<-df %>% mutate(x=Distance*sin(az*(pi/180)), y=Distance*cos(az*(pi/180)))
  }

df<-get.coords(df) # add x,y coordinates to data

exclude<-c('DC','DF','XO','XP','XS', '0', 'ES','EX','NA')
df<-df %>% filter(!Status_ID %in% exclude) %>%
  mutate(Status_ID=as.factor(ifelse(Status_ID %in% c('1','AM','AS','RS'), 'AS',
                                    ifelse(Status_ID %in% c('2','DS','DM'), 'DS',
                                           ifelse(Status_ID == 'RF','AF',
                                                  ifelse(Status_ID == 'RL','AL',
                                                         paste(Status_ID))))))) %>%
  droplevels()

df$Orientation[df$Orientation==360]<-0
return(df)
}
