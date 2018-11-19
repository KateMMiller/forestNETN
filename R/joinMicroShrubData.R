#' @include joinLocEvent.R
#' @title joinMicroShrubData
#'
#' @description This function combines shrub stem counts (cycle 1) and percent cover (cycle 2+) data from microplots. Must run importData first.
#'
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' }
#' @return returns a dataframe with shrub data collected in microplots
#'
#' @export
#'
#------------------------
# Joins microplot tables and filters by park, year, and plot/visit type
#------------------------
joinMicroShrubData<-function(speciesType='all', park='all',from=2007, to=2018, QAQC=FALSE, locType='VS', output){
  park.plots<-force(joinLocEvent(park=park, from=from,to=to,QAQC=QAQC,locType=locType, rejected=F,output='short'))

  # Prepare the sapling data
  shrub1<-merge(micro,shrub[,c("Microplot_Characterization_Data_ID","TSN", "Num_Stems","Cover_Class_ID")],
    by="Microplot_Characterization_Data_ID", all.x=T, all.y=T)
  shrub2<-merge(park.plots,shrub1,by='Event_ID',all.x=T)
  shrub3<-merge(shrub2,plants[,c("TSN","Latin_Name","Common",'Exotic')],by="TSN",all.x=T)

  # For data with # stems or DRC change anything >0 to 1 for Present.old. For % Cover, change cover classes to midpoint
  shrub3<-shrub3 %>% mutate(present.old=ifelse(Year<=2009 & (Cover_Class_ID>0 | Num_Stems> 0 ), 1,NA))
  # Cycle 1 changed from stem counts to % cover in 2010.
  # Due to these changes, we're only going to use cover data for cycle 2 (2010) and after, but will record species as
  # present.old if they were recorded from 2006 to 2009.
  shrub3<-shrub3 %>% mutate(cover=
      case_when(Cover_Class_ID == 1 ~ 0.1,
        Cover_Class_ID == 2 ~ 3,
        Cover_Class_ID == 3 ~ 7.5,
        Cover_Class_ID == 4 ~ 17.5,
        Cover_Class_ID == 5 ~ 37.5,
        Cover_Class_ID == 6 ~ 62.5,
        Cover_Class_ID == 7 ~ 85,
        Cover_Class_ID == 8 ~ 97.5,
        Cover_Class_ID == 0 ~ 0))
  shrub3<-shrub3 %>% mutate(cover=ifelse(TSN==-9999999951 & Year >2009,0,cover)) # Where 'no species recorded' was entered
  # and after % cover being used, record 0.

  shrub4<-shrub3 %>% group_by(Event_ID,TSN,Latin_Name,Common,Exotic) %>% summarise(present.old=ifelse(sum(present.old)>0,1,NA),
    cover=sum(cover)/3)

  shrub5<- if (speciesType=='native'){filter(shrub4,Exotic==FALSE)
  } else if (speciesType=='exotic'){filter(shrub4,Exotic==TRUE)
  } else if (speciesType=='all'){(shrub4)
  } else if (speciesType!='native'|speciesType!='exotic'|speciesType!='all'){
    stop("speciesType must be either 'native','exotic', or 'all'")}

  shrub6<-merge(park.plots,shrub5[,c("Event_ID","TSN","Latin_Name","Common","Exotic","present.old","cover")],by="Event_ID",all.x=T)

  return(data.frame(shrub6))
} # end of function


