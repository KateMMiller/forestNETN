#' @include joinLocEvent.R
#' @title joinCWDData: compile coarse woody debris data.
#'
#' @description This function combines calculates CWD volume for each plot. Must run importData first.
#'
#'#' @param units Calculates CWD Volume based on different units.
#' \describe{
#' \item{"ha"}{Default. Returns CWD volume as cubic m/hectare}
#' \item{"acres"}{Returns CWD volume as cubic ft/ acre}
#'}
#'
#' @return returns a dataframe with CWD volume for each plot, one with cubic m/ha and cubic ft/acre
#'
#' @export
#'
#------------------------
# Join CWD table and filters by park, year, and plot/visit type
#------------------------
joinCWDData<-function(units=c('ha','acres'), park='all',from=2006, to=2018, QAQC=FALSE, locType='VS', panels=1:4, output){
  units<-match.arg(units)
  # Prepare the CWD data
  park.plots<-force(joinLocEvent(park=park, from=from,to=to,QAQC=QAQC,locType=locType, panels=panels,output='short'))
  cwd1<-merge(park.plots,cwd,by='Event_ID', all.x=T,all.y=F)
  cwd2<-merge(cwd1[,c("Event_ID","TSN","Diameter","Decay_Class_ID","Hollow","Transect","Distance","Wood_Type")],
              plants[,c('TSN','Latin_Name','Common')], by='TSN',all.x=T)
  cwd.std<-merge(cwd2,stand[,c("Event_ID","Slope_UP","Slope_BR","Slope_BL")],by="Event_ID",all.x=T,all.y=F)

  cwd.std2<-cwd.std %>% mutate(slope=case_when(Transect=='BL'~ Slope_BL, Transect=='BR'~ Slope_BR, Transect=='UP'~ Slope_UP),
                               pct.slope=ifelse(is.na(slope),0,tan(slope*pi/180)*100),
                               hdist=((((pct.slope/100)^2)+1)^0.5)*((pi^2)/(8*15)),
                               diam=Diameter^2)
  cwd3<-cwd.std2 %>% group_by(Event_ID,Transect, hdist, Latin_Name, Decay_Class_ID) %>% summarise(diam=sum(diam)) # Add species and decay class
  cwd4<-cwd3 %>% group_by(Event_ID, Latin_Name, Decay_Class_ID) %>% summarise(CWD_Vol=ifelse(is.na(sum(diam)),0,sum(hdist*diam)/3))
  cwd5<-merge(park.plots,cwd4,by="Event_ID", all.x=T)

  cwd6<-if (units=='acres'){
    cwd5 %>% mutate(CWD_Vol=CWD_Vol*35.314667/2.4710538)
    # 35.314667 is the # cubic feet in a cubic meter. 2.4710538 is # acres in 1 hectare.)
  } else if (units=='ha'){return(cwd5)
  }

  cwd7<-merge(park.plots,cwd6[,c("Event_ID","CWD_Vol", "Latin_Name","Decay_Class_ID")],by="Event_ID",all.x=T)
  cwd7[,"CWD_Vol"][is.na(cwd7[,"CWD_Vol"])]<-0

  return(data.frame(cwd7))
} # end of function
