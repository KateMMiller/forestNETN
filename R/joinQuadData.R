#' @include joinLocEvent.R
#' @title joinQuadData
#'
#' @description This function combines quadrat species data with species names and calculates average cover and quadrat frequency for each species. Average cover is corrected for number of quadrats sampled.
#'
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' }
#'
#' @return Returns a dataframe with average quadrat cover and frequency for each species and guild for each species.
#'
#' @export
#'
#------------------------
# Joins quadrat tables and filters by park, year, and plot/visit type
#------------------------
joinQuadData<-function(speciesType='all', park='all',from=2006, to=2018, QAQC=FALSE, locType='VS', output){
  # Prepare the quadrat data
  # calculate correct number of quadrats to divide by for average.
  quadsamp$numHerbPlots<-apply(quadsamp[,c(15:22)], 1,sum)
  park.plots<-force(joinLocEvent(park=park, from=from,to=to,QAQC=QAQC,locType=locType, output='short'))
  quads1<-merge(park.plots, quadsamp[,c("Event_ID","numHerbPlots")], by="Event_ID", all.x=T)
  plants<-plants %>% mutate(Shrub=ifelse(Shrub==1|Vine==1,1,0),
    Tree=ifelse(Latin_Name=="Rhamnus cathartica",0,Tree))

  #------Quadrat Cover
  quadspp<-merge(quads[,c("Event_ID","TSN","qUC_Cover_Class_ID","qUL_Cover_Class_ID","qML_Cover_Class_ID",
    "qBL_Cover_Class_ID","qBC_Cover_Class_ID","qBR_Cover_Class_ID","qMR_Cover_Class_ID","qUR_Cover_Class_ID")],
    plants[,c("TSN","Latin_Name","Tree","Shrub","Vine","Herbaceous","Graminoid","Exotic","Indicator_Invasive_NETN")], by="TSN",all.x=T)
  quads2<-merge(quads1,quadspp,by="Event_ID",all.x=T)

  # Convert coverclasses to midpoints for all 8 quadrats
  quads2[,14:21][quads2[,14:21]==1]<-0.1
  quads2[,14:21][quads2[,14:21]==2]<-1.5
  quads2[,14:21][quads2[,14:21]==3]<-3.5
  quads2[,14:21][quads2[,14:21]==4]<-7.5
  quads2[,14:21][quads2[,14:21]==5]<-17.5
  quads2[,14:21][quads2[,14:21]==6]<-37.5
  quads2[,14:21][quads2[,14:21]==7]<-62.5
  quads2[,14:21][quads2[,14:21]==8]<-85
  quads2[,14:21][quads2[,14:21]==9]<-97.5

  # average cover by species, so I can later summarize by guild
  quads3<-quads2 %>% group_by(Event_ID,TSN,Latin_Name) %>%
    mutate(avecov=(qUC_Cover_Class_ID+qUR_Cover_Class_ID+qMR_Cover_Class_ID+qBR_Cover_Class_ID+
        qBC_Cover_Class_ID+qBL_Cover_Class_ID+qML_Cover_Class_ID+qUL_Cover_Class_ID)/numHerbPlots) %>%
    dplyr::select(Event_ID,TSN,Latin_Name,Exotic,Tree,Shrub,Herbaceous,Graminoid,Indicator_Invasive_NETN,avecov) %>%
    mutate(Tree=ifelse(Latin_Name=="Rhamnus cathartica",0,Tree))

  # calculate frequency
  quads2[,14:21][!is.na(quads2[,14:21]) & quads2[,14:21]>0]<-1

  quads4<-quads2 %>% group_by(Event_ID,TSN) %>%
    mutate(quadfreq=(qUC_Cover_Class_ID+qUR_Cover_Class_ID+qMR_Cover_Class_ID+qBR_Cover_Class_ID+
        qBC_Cover_Class_ID+qBL_Cover_Class_ID+qML_Cover_Class_ID+qUL_Cover_Class_ID), numHerbPlots=first(numHerbPlots)) %>%
    dplyr::select(Event_ID,TSN,quadfreq) %>% left_join(quads3,.,by=c("Event_ID","TSN"))

  quads5<-if (speciesType=='native'){filter(quads4,Exotic==FALSE)
  } else if (speciesType=='exotic'){filter(quads4,Exotic==TRUE)
  } else if (speciesType=='all'){(quads4)
  } else if (speciesType!='native'|speciesType!='exotic'|speciesType!='all'){
    stop("speciesType must be either 'native','exotic', or 'all'")}

  quads6<-merge(quads1,quads5,by='Event_ID',all.x=T)
  quads6[,21:22][is.na(quads6[,21:22])]<-0
  return(data.frame(quads6))
} # end of function

