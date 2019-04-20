#' @include joinLocEvent.R
#' @title joinTreeData: compiles tree data
#'
#'
#' @importFrom dplyr select filter arrange mutate summarise group_by
#' @importFrom magrittr %>%
#'
#' @description This function combines location and event-level Tree data. Must run importData first.
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
#' @param dist_m Filter trees by a distance that is less than or equal to the specified distance in meters
#' of the tree to the center of the plot. If no distance is specified, then all trees will be selected. For
#' example, to select an area of trees that is 100 square meters in area, use a distance of 5.64m.
#'
#' @return returns a dataframe with plot-level and visit-level tree data
#'
#' @examples
#' importData()
#' # compile tree data for live trees only in most recent survey in all parks
#' live_trees <- joinTreeData(status = 'live', from = 2015, to = 2018)
#'
#' # compile ACAD trees within 100m^2 circle in most recent survey
#' ACAD_100m <- joinTreeData(park = 'ACAD', from = 2015, to = 2018, dist_m = 5.64)
#'
#' # compile dead trees in MABI in most recent survey
#' MABI_dead <- joinTreeData(park = 'MABI', from = 2015, to = 2018, status = 'dead')
#'
#' # compile exotic trees in MIMA in all years
#' MIMA_exotic <- joinTreeData(park = 'MIMA', from = 2015, to = 2018, speciesType = 'exotic')
#'
#' @export
#'
#------------------------
# Joins tbl_Trees and tbl_Tree_Data tables and filters by park, year, and plot/visit type
#------------------------
joinTreeData<-function(status=c('all', 'live','dead'), speciesType=c('all', 'native','exotic'), park='all',
                       from=2006, to=2018, QAQC=FALSE, locType='VS', panels=1:4, dist_m=NA, output, ...){

  status<-match.arg(status)
  speciesType<-match.arg(speciesType)

  treeTSN<-merge(trees[,c("Tree_ID","Location_ID","TSN","Tree_Number_NETN", "Distance","Azimuth")],
                 plants[,c('TSN','Latin_Name','Common','Exotic')], by="TSN", all.x=T)
  tree2<-merge(treeTSN,treedata,by="Tree_ID", all.x=T,all.y=T)
  tree2<-tree2 %>% select(Tree_ID:HWA_Status,Event_ID,-Location_ID)
  tree2$BA_cm2<-round(pi*((tree2$DBH/2)^2),4)# basal area (cm^2)

  alive<-c("1", "AB", "AF", "AL" ,"AM" ,"AS", "RB", "RF", "RL", "RS")
  dead<-c("2","DB" ,"DF" ,"DL", "DM","DS")

  tree3<- if (status=='live') {filter(tree2,Status_ID %in% alive)
  } else if (status=='dead') {filter(tree2,Status_ID %in% dead)
  } else if (status=='all') {(tree2)
  }

  tree4<- if (speciesType=='native'){filter(tree3,Exotic==FALSE)
  } else if (speciesType=='exotic'){filter(tree3,Exotic==TRUE)
  } else if (speciesType=='all'){(tree3)
  }

  tree5<-if (!is.na(dist_m)){filter(tree4,Distance<=dist_m)
  } else {tree4}


  park.plots2<-force(joinLocEvent(park=park, from=from, to=to, QAQC=QAQC, locType=locType,panels=panels,output='short'))

  tree6<-merge(park.plots2, tree5, by='Event_ID', all.x=T)
  tree6<-droplevels(tree6)

  return(data.frame(tree6))
} # end of function

