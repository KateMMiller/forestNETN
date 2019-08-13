#' @include joinLocEvent.R
#' @title joinQuadData: compiles quadrat species data
#'
#' @importFrom dplyr select filter arrange mutate summarise group_by rename_at
#' @importFrom magrittr %>%
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
#' @examples
#' importData()
#' # compile quadrat data for invasive species in SARA for all years
#' SARA_quads <- joinQuadData(park = 'SARA', speciesType = 'invasive')
#'
#' # compile native species only for all parks in most recent survey
#' native_quads <- joinQuadData(speciesType = 'native', from = 2015, to = 2018)
#'
#' @export
#'
#------------------------
# Joins quadrat tables and filters by park, year, and plot/visit type
#------------------------
joinQuadData<-function(speciesType=c('all', 'native','exotic', 'invasive'), park='all',from=2006, to=2018,
                       QAQC=FALSE, locType='VS', panels=1:4, output, ...){
  speciesType<-match.arg(speciesType)
  # Prepare the quadrat data
  quadsamp$numHerbPlots<-apply(quadsamp[,c(15:22)], 1,sum)
  park.plots<-force(joinLocEvent(park=park, from=from,to=to,QAQC=QAQC,locType=locType, panels=panels,output='short'))

  quads1<-merge(park.plots, quadsamp[,c("Event_ID","numHerbPlots")], by="Event_ID", all.x=T)
  plants<-plants %>% mutate(Tree=ifelse(Latin_Name=="Rhamnus cathartica",0,Tree))

  quadspp<-merge(quads[,c("Event_ID","TSN","Germinant","qUC_Cover_Class_ID","qUL_Cover_Class_ID",
    "qML_Cover_Class_ID", "qBL_Cover_Class_ID","qBC_Cover_Class_ID","qBR_Cover_Class_ID",
    "qMR_Cover_Class_ID","qUR_Cover_Class_ID")],
    plants[,c("TSN","Latin_Name","Tree","Shrub","Vine","Herbaceous","Graminoid","Fern_Ally",
      "Exotic","Indicator_Invasive_NETN")],
    by="TSN",all.x=T)
  quads2<-merge(quads1,quadspp,by="Event_ID",all.x=T) #%>% filter(Germinant==0) %>% select(-Germinant)

#names(quads2)

  # Convert coverclasses to midpoints for all 8 quadrats
  quads2[,15:22][quads2[,15:22]==1]<-0.1
  quads2[,15:22][quads2[,15:22]==2]<-1.5
  quads2[,15:22][quads2[,15:22]==3]<-3.5
  quads2[,15:22][quads2[,15:22]==4]<-7.5
  quads2[,15:22][quads2[,15:22]==5]<-17.5
  quads2[,15:22][quads2[,15:22]==6]<-37.5
  quads2[,15:22][quads2[,15:22]==7]<-62.5
  quads2[,15:22][quads2[,15:22]==8]<-85
  quads2[,15:22][quads2[,15:22]==9]<-97.5

  old.names<-names(quads2[,15:22])
  new.names<-c('UC','UL','ML','BL','BC','BR','MR','UR')
  quads2<-quads2 %>% rename_at(vars(old.names),~new.names)
  quads2[,c(15:22)][is.na(quads2[,c(15:22)])]<-0

  quads3<-quads2 %>% mutate(avg.cover=(UC+UL+ML+BL+BC+BR+MR+UR)/numHerbPlots)
  quads3[,c(15:22)][quads3[,c(15:22)]>0]<-1
  quads3<-quads3 %>% mutate(avg.freq=(UC+UL+ML+BL+BC+BR+MR+UR)/numHerbPlots)

  quads4<-if (speciesType=='native'){filter(quads3,Exotic==FALSE)
  } else if (speciesType=='exotic'){filter(quads3,Exotic==TRUE)
  } else if (speciesType=='invasive'){filter(quads3,Indicator_Invasive_NETN==TRUE)
  } else if (speciesType=='all'){(quads3)
  }

  quads5<-merge(quads1,quads4[,c(1,13:33)],by='Event_ID',all.x=T)
  quads5[,c(15:22, 24:33)][is.na(quads5[,c(15:22, 24:33)])]<-0
  quads5<-quads5 %>% mutate(germ.cover=ifelse(Germinant==1,avg.cover,0), germ.freq=ifelse(Germinant==1,avg.freq,0),
                            avg.cover=ifelse(Germinant==0,avg.cover,0), avg.freq=ifelse(Germinant==0,avg.freq,0))

  quads5.nongerm<-quads5 %>% filter(Germinant==0) %>% select(-(germ.cover:germ.freq)) %>% droplevels()
  quads5.germ<-quads5 %>% filter(Germinant==1) %>% select(-(avg.cover:avg.freq)) %>% droplevels()

  quads6<-merge(quads1,quads5.nongerm[,c(1,13,15:22,32,33)], by="Event_ID",all.x=T)
  quads7<-merge(quads1,quads5.germ[,c(1,13:22,32,33)], by=c("Event_ID"), all.x=T,all.y=T)
  quads8<-merge(quads6,quads7,by=c("Event_ID","Location_ID","Unit_Code","Plot_Name",
    "Plot_Number","X_Coord","Y_Coord","Panel","Year","Event_QAQC","cycle", "TSN"), all.x=T,all.y=T)

  quads8[,c(14:35)][is.na(quads8[,c(14:35)])]<-0

  quads9<-quads8 %>% mutate(numHerbPlots=ifelse(numHerbPlots.x>0,numHerbPlots.x,numHerbPlots.y),
                            UC=ifelse((UC.x+UC.y)>0,1,0), UR=ifelse((UR.x+UR.y)>0,1,0),
                            MR=ifelse((MR.x+MR.y)>0,1,0), BR=ifelse((BR.x+BR.y)>0,1,0),
                            BC=ifelse((BC.x+BC.y)>0,1,0), BL=ifelse((BL.x+BL.y)>0,1,0),
                            ML=ifelse((ML.x+ML.y)>0,1,0), UL=ifelse((UL.x+UL.y)>0,1,0)) %>%
    select(-(UC.x:UR.x),-(UC.y:UR.y),-numHerbPlots.x,-numHerbPlots.y,-Germinant)

  quads10<-merge(quads9,plants[,c("TSN","Latin_Name","Tree","Shrub","Vine","Herbaceous","Graminoid","Fern_Ally",
    "Exotic","Indicator_Invasive_NETN")], by="TSN",all.x=T)

  quads10<-quads10 %>% mutate(Latin_Name= ifelse(is.na(Latin_Name), paste0('No species'), paste0(Latin_Name)))

  quads.final<-quads10 %>% select(Location_ID,Event_ID:cycle,numHerbPlots,UC:UL,TSN,Latin_Name,Tree:Indicator_Invasive_NETN,avg.cover:germ.freq)
  return(data.frame(quads.final))

  } # end of function

