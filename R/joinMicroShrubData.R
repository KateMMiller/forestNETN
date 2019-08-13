#' @include joinLocEvent.R
#' @title joinMicroShrubData: compiles shrub data collected in microplots
#'
#' @importFrom dplyr select filter arrange mutate summarise group_by
#' @importFrom magrittr %>%
#'
#' @description This function combines shrub stem counts (cycle 1) and percent cover (cycle 2+) data from microplots. Must run importData first.
#'
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' }
#'
#' @param numMicros Allows you to select 1, 2, or 3 microplots of data to summarize
#'
#' @return returns a dataframe with shrub data collected in microplots
#'
#' @examples
#' importData()
#' # native shrubs in MORR all years
#' native_shrubs <- joinMicroShrubData(park ='MORR', speciesType = 'native')
#'
#' # all parks with exotic shrubs in most recent survey
#' exotic_shrubs <- joinMicroShrubData(from = 2015, to = 2018, speciesType = 'exotic')
#'
#' @export
#'
#------------------------
# Joins microplot tables and filters by park, year, and plot/visit type
#------------------------
joinMicroShrubData<-function(speciesType = c('all', 'native','exotic'), numMicros = 3, park='all',
  from = 2007, to = 2018, QAQC = FALSE, locType = 'VS', panels = 1:4, output, ...){

  speciesType<-match.arg(speciesType)

  park.plots<-force(joinLocEvent(park = park, from = from,to = to, QAQC = QAQC,locType = locType,
                                 rejected = F, panels = panels, output = 'short'))

  # Prepare the sapling data
  shrub1<-merge(micro,shrub[,c("Microplot_Characterization_Data_ID","TSN", "Num_Stems","Cover_Class_ID")],
    by="Microplot_Characterization_Data_ID", all.x=T, all.y=T)
  shrub2<-merge(park.plots,shrub1,by='Event_ID',all.x=T)
  shrub3<-merge(shrub2,plants[,c("TSN","Latin_Name","Common",'Exotic')],by="TSN",all.x=T)
  shrub3<-shrub3 %>% mutate(Latin_Name= ifelse((Plot_Name=="SAGA-008" & Year==2010),
                                               paste0('MissingData'), paste0(Latin_Name)),
                            Common= ifelse(is.na(Common), paste0(Latin_Name), paste0(Common)),
                            present.old=ifelse(Year<=2009 & (Cover_Class_ID>0 | Num_Stems> 0 ), 1,NA))

  # For data with # stems or DRC change anything >0 to 1 for Present.old. For % Cover, change cover classes to midpoint
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

  shrub4<- if (numMicros==1) {filter(shrub3, Microplot_Name=='UR') %>% droplevels()
    } else if (numMicros==2) {filter(shrub3, Microplot_Name %in% c("UR","B")) %>% droplevels()
    } else if (numMicros==3) {shrub3}

  shrub5<-shrub4 %>% group_by(Event_ID,TSN,Latin_Name,Common,Exotic,Year) %>%
    summarise(present.old=ifelse(sum(present.old)>0,1,NA),
    cover=sum(cover))

  shrub6<-shrub5 %>% group_by(Event_ID,TSN,Latin_Name,Common,Exotic) %>%
    summarise(micro=ifelse(Year==2006,1,numMicros), present.old=present.old,
              cover=cover/micro)

  shrub7<- if (speciesType=='native'){filter(shrub6,Exotic==FALSE)
  } else if (speciesType=='exotic'){filter(shrub6,Exotic==TRUE)
  } else if (speciesType=='all'){(shrub6)
  }

  shrub8<-merge(park.plots,shrub7[,c("Event_ID","TSN","Latin_Name","Common","Exotic","present.old","cover")],by="Event_ID",all.x=T)
  shrub8<-shrub8 %>% arrange(Plot_Name, Year, Latin_Name)

  return(data.frame(shrub8))
} # end of function


