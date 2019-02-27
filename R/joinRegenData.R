#' @include joinLocEvent.R
#' @title joinRegenData
#'
#' @description This function combines seedling and sapling data, and calculates stocking index. Must run importData first.
#'
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' }
#' @param canopyForm Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Returns all species, including low canopy species.}
#' \item{"canopy"}{Default. Returns canopy-forming species only}
#'}
#' @param units Calculates seedling and sapling densities based on different units.
#' \describe{
#' \item{"micro"}{Default. Returns seedling and sapling densities per microplot.}
#' \item{"ha"}{Returns seedling and sapling densities per hectare}
#' \item{"acres"}{Returns densities per acre}
#'}
#'
#' @return returns a dataframe with seedling and sapling densities, and stocking index
#'
#' @export
#'
# consider making it so that individual species can be filtered?
#------------------------
# Joins microplot tables and filters by park, year, and plot/visit type
#------------------------
joinRegenData<-function(speciesType=c('all', 'native','exotic'), canopyForm=c('canopy','all'),
  units=c('micro','ha','acres'), park='all',from=2006, to=2018, QAQC=FALSE, locType='VS', output){
  speciesType<-match.arg(speciesType)
  canopyForm<-match.arg(canopyForm)
  units<-match.arg(units)

# Prepare the seedling data
  seeds1<-merge(sdlg,micro, by="Microplot_Characterization_Data_ID",all.x=T)
  seeds1$Num_Seedlings_15_30cm[is.na(seeds1$Num_Seedlings_15_30cm)]<-0
  seeds1$Num_Seedlings_30_100cm[is.na(seeds1$Num_Seedlings_30_100cm)]<-0
  seeds1$Num_Seedlings_100_150cm[is.na(seeds1$Num_Seedlings_100_150cm)]<-0
  seeds1$Num_Seedlings_Above_150cm[is.na(seeds1$Num_Seedlings_Above_150cm)]<-0

  seeds2<-seeds1 %>% group_by(Event_ID,TSN) %>% summarise(seed15.30=sum(Num_Seedlings_15_30cm),
    seed30.100=sum(Num_Seedlings_30_100cm),seed100.150=sum(Num_Seedlings_100_150cm),
    seed150p=sum(Num_Seedlings_Above_150cm))

# Prepare the sapling data
  saps1<-merge(micro,saps,by="Microplot_Characterization_Data_ID", all.y=T)
  saps1<-saps1 %>% mutate(sap=ifelse(Count>0 & !is.na(Count),Count,ifelse(DBH>0 & !is.na(DBH),1,0)))
  saps2<-saps1 %>% group_by(Event_ID,TSN) %>% summarise(sap.stems=sum(sap, na.rm=T),avg.sap.dbh=mean(DBH, na.rm=T))

# Combine seedling and sapling data
  park.plots<-force(joinLocEvent(park=park, from=from,to=to,QAQC=QAQC,locType=locType, output='short'))
  regen1<-merge(park.plots,seeds2,by='Event_ID', all.x=T,all.y=F)
  regen2<-merge(regen1,saps2,by=c("Event_ID","TSN"),all.x=T,all.y=F)
  regen3<-merge(regen2,plants[,c('TSN','Latin_Name','Common','Exotic','Canopy_Exclusion')], by='TSN',all.x=T)

  regen4<-if(canopyForm=='canopy'){filter(regen3, Canopy_Exclusion!=1)
  } else if(canopyForm=='all'){(regen3)
  }

  regen5<- if (speciesType=='native'){filter(regen4,Exotic==FALSE)
  } else if (speciesType=='exotic'){filter(regen4,Exotic==TRUE)
  } else if (speciesType=='all'){(regen4)
  }

  regen5[,12:17][is.na(regen5[,12:17])]<-0
  regen5<-regen5 %>% mutate(micro=ifelse(Year==2006,1,3),
    stock=((1*seed15.30)+(2*seed30.100)+(20*seed100.150)+(50*seed150p)+(50*sap.stems))/micro,
    seed.den=(seed15.30+seed30.100+seed100.150+seed150p)/micro,
    sap.den=sap.stems/micro,
    regen.den=(seed.den+sap.den))

  regen6<-if (units=='ha'){
    regen5 %>%
      mutate(seed15.30=(seed15.30*10000)/(pi*4),
        seed30.100=(seed30.100*10000)/(pi*4),
        seed100.150=(seed100.150*10000)/(pi*4),
        seed150p=(seed150p*10000)/(pi*4),
        seed.den=(seed.den*10000)/(pi*4),
        sap.den=(sap.den*10000)/(pi*4),
        regen.den=(regen.den*10000)/(pi*4))
  } else if (units=='acres'){
    regen5 %>%
      mutate(seed15.30=(seed15.30*4046.856)/(pi*4),
        seed30.100=(seed30.100*4046.856)/(pi*4),
        seed100.150=(seed100.150*4046.856)/(pi*4),
        seed150p=(seed150p*4046.856)/(pi*4),
        seed.den=(seed.den*4046.856)/(pi*4),
        sap.den=(sap.den*4046.856)/(pi*4),
        regen.den=(regen.den*4046.856)/(pi*4))
  } else if (units=='micro'){regen5
  }

  regen7<-regen6 %>% select(Event_ID,TSN,Latin_Name,Common,Exotic,Canopy_Exclusion,seed15.30,
    seed30.100,seed100.150, seed150p,seed.den,sap.den,regen.den,stock) %>% droplevels()

  regen8<-merge(park.plots,regen7,by="Event_ID",all.x=T)
  regen8[,16:24][is.na(regen8[,16:24])]<-0

  return(data.frame(regen8))
} # end of function

# Need to account for SAGA 008 being NA and ACAD 029 as being NA because missing data
