#' @include joinLocEvent.R
#' @title joinQuadData
#'
#' @description This function combines quadrat species data with species names and allows you to filter on species types, park, years, and visit type. Note that the Shrub guild also includes woody vine species.
#'
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' \item{"invasive"}{Returns species on the Indicator Invasive List}
#' }
#'
#' @return Returns a dataframe with cover class midpoints for each quadrat and includes guild for each species.
#'
#' @export
#'
#------------------------
# Joins quadrat tables and filters by park, year, and plot/visit type
#------------------------
joinQuadData<-function(speciesType='all', park='all',from=2006, to=2018, QAQC=FALSE, locType='VS', output){
  # Prepare the quadrat data
  quadsamp$numHerbPlots<-apply(quadsamp[,c(15:22)], 1,sum)
  park.plots<-force(joinLocEvent(park=park, from=from,to=to,QAQC=QAQC,locType=locType, output='short'))

  quads1<-merge(park.plots, quadsamp[,c("Event_ID","numHerbPlots")], by="Event_ID", all.x=T)

  plants<-plants %>% mutate(Shrub=ifelse(Shrub==1|Vine==1,1,0),
    Tree=ifelse(Latin_Name=="Rhamnus cathartica",0,Tree))

  quadspp<-merge(quads[,c("Event_ID","TSN","qUC_Cover_Class_ID","qUL_Cover_Class_ID","qML_Cover_Class_ID",
    "qBL_Cover_Class_ID","qBC_Cover_Class_ID","qBR_Cover_Class_ID","qMR_Cover_Class_ID","qUR_Cover_Class_ID")],
    plants[,c("TSN","Latin_Name","Tree","Shrub","Vine","Herbaceous","Graminoid","Exotic","Indicator_Invasive_NETN")],
    by="TSN",all.x=T)
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

  old.names<-names(quads2[,14:21])
  new.names<-c('UC','UL','ML','BL','BC','BR','MR','UR')
  quads2<-quads2 %>% rename_at(vars(old.names),~new.names)

  quads3<-if (speciesType=='native'){filter(quads2,Exotic==FALSE)
  } else if (speciesType=='exotic'){filter(quads2,Exotic==TRUE)
  } else if (speciesType=='invasive'){filter(quads2,Indicator_Invasive_NETN==TRUE)
  } else if (speciesType=='all'){(quads2)
  } else if (speciesType!='native'|speciesType!='exotic'|speciesType!='invasive'|speciesType!='all'){
    stop("speciesType must be either 'native','exotic', 'invasive', or 'all'")}

  quads4<-merge(quads1,quads3[,c(1,13:29)],by='Event_ID',all.x=T)
  quads4[,c(14:21, 23:29)][is.na(quads4[,c(14:21, 23:29)])]<-0
  return(data.frame(quads4))

} # end of function

