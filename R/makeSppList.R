#' @include joinLocEvent.R
#' @include joinTreeData.R
#' @include joinQuadData.R
#' @include joinRegenData.R
#' @include joinMicroShrubData.R
#' @title makeSppList: creates a species list for each plot
#'
#' @importFrom dplyr select filter arrange mutate summarise group_by
#' @importFrom magrittr %>%
#'
#' @description This function creates a plot-level species list from live trees, microplots, quadrats, and additional species lists.
#'
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' \item{"invasive"}{Returns species on the Indicator Invasive List}
#' }
#'
#' @return Returns a dataframe with species list for each plot.
#'
#' @export
#'
#------------------------
# Joins quadrat tables and filters by park, year, and plot/visit type
#------------------------
makeSppList<-function(speciesType=c('all', 'native','exotic', 'invasive'), park='all',from=2007, to=2018,
  QAQC=FALSE, locType='VS', panels=1:4,eventType='complete',output, ...){
  speciesType<-match.arg(speciesType)

  park.plots<-force(joinLocEvent(park=park,from=from,to=to,QAQC=QAQC,eventType='complete',locType=locType,panels=panels,rejected=F,output='short'))
  plants<-plants %>% mutate(Shrub=ifelse(Shrub+Vine>0,1,0))

  #trees1<-joinTreeData(status='live',output='short')
  trees1<-force(joinTreeData(park=park, from=from,to=to,QAQC=QAQC,locType=locType,
    output='short', status='live'))
  trees1<-trees1 %>% filter(!is.na(Tree_ID)) %>% droplevels() #removes events without live trees (SARA-012.2018,ACAD-029.2010)

  #trees1<-na.omit(trees1, cols='Tree_ID') #removes SARA-012, which doesn't have live trees
  trees2<-trees1 %>% mutate(Common= ifelse(is.na(Common), paste0(Latin_Name), paste0(Common))) %>%
                              group_by(Event_ID,TSN,Latin_Name,Common) %>% summarise(tree.stems=length(DBH>10),
    tree.BAcm2=sum(BA_cm2)) %>% ungroup()
  trees3<-trees2 %>% select(Event_ID,TSN,tree.stems,tree.BAcm2)

  #regen1<-joinRegenData(output='short')
  regen1<-force(joinRegenData(park=park, from=from,to=to,QAQC=QAQC,locType=locType,
    output='short'))
  regen2<-regen1 %>% select(Event_ID,TSN,seed.den,sap.den,stock)

  #quads1<-joinQuadData(output='short')
  quads1<-force(joinQuadData(park=park, from=from,to=to,QAQC=QAQC,locType=locType,
    output='short'))
  quads2<-quads1 %>% select(Event_ID,TSN,avg.cover,avg.freq, germ.cover,germ.freq)

  #shrub1<-joinMicroShrubData(output='short')
  shrub1<-force(joinMicroShrubData(park=park, from=from,to=to,QAQC=QAQC,locType=locType,
    output='short'))
  shrub2<-shrub1 %>% select(Event_ID,TSN,present.old,cover)

  addspp2<-addspp %>% filter(TSN!=0) %>% select(Event_ID,TSN) %>% mutate(addspp=1)

  comb1<-merge(trees3,regen2,by=c("Event_ID","TSN"),all.x=T,all.y=T)
  comb2<-merge(comb1,quads2,by=c("Event_ID","TSN"),all.x=T,all.y=T)
  comb3<-merge(comb2,shrub2,by=c("Event_ID","TSN"),all.x=T,all.y=T)
  comb4<-merge(comb3,addspp2,by=c("Event_ID","TSN"),all.x=T,all.y=T)
  comb5<-comb4 %>% filter(TSN!=-9999999951)

  comb6<-merge(comb5,plants[,c("TSN","Latin_Name","Common","Exotic",
    "Indicator_Invasive_NETN","Tree","Shrub","Herbaceous",
    "Graminoid","Fern_Ally")],by="TSN",all.x=T)

  comb6<-comb6 %>% mutate(Common= ifelse(is.na(Common), paste0(Latin_Name), paste0(Common)))

  comb7<-if (speciesType=='native'){filter(comb6,Exotic==FALSE)
  } else if (speciesType=='exotic'){filter(comb6,Exotic==TRUE)
  } else if (speciesType=='invasive'){filter(comb6,Indicator_Invasive_NETN==TRUE)
  } else if (speciesType=='all'){comb6
  }

  comb8<-merge(park.plots,comb7,by="Event_ID",all.x=T,all.y=F)

  colnames(comb8)<-c("Event_ID","Location_ID","Unit_Code","Plot_Name","Plot_Number","X_Coord","Y_Coord","Panel",
    "Year","Event_QAQC","cycle","TSN","tree.stems","tree.BAcm2","seed.den","sap.den","stocking.index","avg.quad.cover",
    "avg.quad.freq",'avg.germ.cover','avg.germ.freq',"shrub.present.old","shrub.cover","addspp.present",
    "Latin_Name","Common","Exotic","Indicator_Invasive_NETN","Tree","Shrub","Herbaceous","Graminoid","Fern_Ally")

  comb8[,c(13:21,24)][is.na(comb8[,c(13:21,24)])]<-0
  comb9<-comb8 %>% mutate(shrub.cover=ifelse(Year>2009 & is.na(shrub.cover),0,shrub.cover),
    shrub.present.old=ifelse(Year<=2009 & is.na(shrub.present.old),0,shrub.present.old))

  return(data.frame(comb9))
} # end of function
