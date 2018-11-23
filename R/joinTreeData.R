#' @include joinLocEvent.R
#' @title joinTreeData
#'
#' @description This function combines location and event-level Tree data. Must run importData first.
#'
#' @param status Filter by live, dead, or all. Acceptable options are:
#' \describe{
#' \item{"all"}{Includes all standing trees}
#' \item{"live"}{live trees only}
#' \item{"dead"}{dead trees only}
#' }
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' }
#' @return returns a dataframe with plot-level and visit-level tree data
#'
#' @export
#'

#------------------------
# Joins tbl_Trees and tbl_Tree_Data tables and filters by park, year, and plot/visit type
#------------------------
joinTreeData<-function(status='all',speciesType='all',park='all',from=2006,to=2018,QAQC=FALSE,locType='VS',output){
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
  } else if (status!="live"|status!='dead'|status!='all') {stop("status must either be 'live','dead', or 'all'")}

  tree4<- if (speciesType=='native'){filter(tree3,Exotic==FALSE)
  } else if (speciesType=='exotic'){filter(tree3,Exotic==TRUE)
  } else if (speciesType=='all'){(tree3)
  } else if (speciesType!='native'|speciesType!='exotic'|speciesType!='all'){
    stop("speciesType must be either 'native','exotic', or 'all'")}

  park.plots2<-force(joinLocEvent(park=park, from=from,to=to,QAQC=QAQC,locType=locType, output='short'))

  tree5<-merge(park.plots2, tree4, by='Event_ID', all.x=T)
  tree5<-droplevels(tree5)

  return(data.frame(tree5))
} # end of function

