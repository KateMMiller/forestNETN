#' @include joinLocEvent.R
#' @title joinRegenData: compiles seedling and sapling data
#'
#' @importFrom dplyr select filter arrange mutate summarise group_by ungroup
#' @importFrom magrittr %>%
#' @importFrom stringr str_sub str_pad
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
#' @param numMicros Allows you to select 1, 2, or 3 microplots of data to summarize
#'
#' @return returns a dataframe with seedling and sapling densities, and stocking index
#'
#' @examples
#' importCSV('./forest_csvs/')
#' # compile seedling and sapling data for all parks and all species in most recent cycle
#' regen_data <- joinRegenData(canopyForm = 'all', from = 2015, to = 2018)
#'
#' # compile regen data for only canopy-forming (default) and native species in SAGA for all years
#' SAGA_regen <- joinRegenData(park = 'SAGA', speciesType = 'native')
#'
#' # compile only 1 microplot of data for ACAD native canopy-forming species for all but first year
#' ACAD_regen_m1 <- joinRegenData(park = 'ACAD', speciesType = 'native', numMicros = 1, from = 2007)
#'
#' @export
#'
#------------------------
# Joins microplot tables and filters by park, year, and plot/visit type
#------------------------
joinRegenData<-function(speciesType=c('all', 'native','exotic'), canopyForm=c('canopy','all'), numMicros=3,
  units=c('micro','ha','acres'), park='all',from=2006, to=2018, QAQC=FALSE, locType='VS', panels=1:4, output, ...){

  speciesType<-match.arg(speciesType)
  canopyForm<-match.arg(canopyForm)
  units<-match.arg(units)

# Prepare the seedling data
  seeds1<-merge(sdlg,micro, by="Microplot_Characterization_Data_ID",all.x=T)
  seeds1$Num_Seedlings_15_30cm[is.na(seeds1$Num_Seedlings_15_30cm)]<-0
  seeds1$Num_Seedlings_30_100cm[is.na(seeds1$Num_Seedlings_30_100cm)]<-0
  seeds1$Num_Seedlings_100_150cm[is.na(seeds1$Num_Seedlings_100_150cm)]<-0
  seeds1$Num_Seedlings_Above_150cm[is.na(seeds1$Num_Seedlings_Above_150cm)]<-0

  seeds2<-seeds1 %>% group_by(Event_ID,TSN,Microplot_Name) %>% summarise(seed15.30=sum(Num_Seedlings_15_30cm),
    seed30.100=sum(Num_Seedlings_30_100cm),seed100.150=sum(Num_Seedlings_100_150cm),
    seed150p=sum(Num_Seedlings_Above_150cm))

# Prepare the sapling data
  saps1<-merge(micro,saps,by="Microplot_Characterization_Data_ID", all.y=T)
  saps1<-saps1 %>% mutate(sap=ifelse(Count>0 & !is.na(Count),Count,ifelse(DBH>0 & !is.na(DBH),1,0)))
  saps2<-saps1 %>% group_by(Event_ID,TSN, Microplot_Name) %>%
    summarise(sap.stems=sum(sap, na.rm=T),avg.sap.dbh=mean(DBH, na.rm=T))

# Combine seedling and sapling data
  park.plots<-force(joinLocEvent(park=park, from=from,to=to,QAQC=QAQC,locType=locType,panels=panels,output='short'))
  regen1<-merge(park.plots,seeds2,by='Event_ID', all.x=T,all.y=F)
  regen2<-merge(regen1,saps2,by=c("Event_ID","TSN", "Microplot_Name"),all.x=T,all.y=T)
  regen3<-merge(regen2,plants[,c('TSN','Latin_Name','Common','Exotic','Canopy_Exclusion')], by='TSN',all.x=T)

  regen4<- if (numMicros==1) {filter(regen3, Microplot_Name=='UR') %>% droplevels() # randomly determined this
  } else if (numMicros==2) {filter(regen3, Microplot_Name %in% c('UR','B')) %>% droplevels() #randomly determined this
  } else if (numMicros==3) {regen3}


  regen5<-if(canopyForm=='canopy'){filter(regen4, Canopy_Exclusion!=1)
  } else if(canopyForm=='all'){(regen4)
  }

  regen6<- if (speciesType=='native'){filter(regen5,Exotic==FALSE)
  } else if (speciesType=='exotic'){filter(regen5,Exotic==TRUE)
  } else if (speciesType=='all'){(regen5)
  }

  regen6[,14:18][is.na(regen6[,14:18])]<-0
  regen6<-regen6 %>% mutate(Latin_Name= ifelse((Plot_Name=="SAGA-008" & Year==2010),
                                               paste0('MissingData'), paste0(Latin_Name)),
                            Common= ifelse(is.na(Common), paste0(Latin_Name), paste0(Common)))
  # Summarise data at plot level. We lose the Microplot name, but average over # microplots selected in next step
  regen7<-regen6 %>% group_by(Event_ID, TSN, Latin_Name, Common, Exotic, Canopy_Exclusion, Year, cycle) %>%
    summarise(seed15.30=sum(seed15.30, na.rm=T),seed30.100=sum(seed30.100, na.rm=T),seed100.150=sum(seed100.150, na.rm=T),
              seed150p=sum(seed150p, na.rm=T), sap.stems=sum(sap.stems, na.rm=T), avg.sap.dbh=mean(avg.sap.dbh, na.rm=T)) %>%
    ungroup()

  regen8<-regen7 %>% mutate(micro=ifelse(Year==2006, 1, numMicros),
           stock=((1*seed15.30)+(2*seed30.100)+(20*seed100.150)+(50*seed150p)+(50*sap.stems))/micro,
           seed15.30=seed15.30/micro,
           seed30.100=seed30.100/micro,
           seed100.150=seed100.150/micro,
           seed150p=seed150p/micro,
           seed.den=(seed15.30+seed30.100+seed100.150+seed150p),
           sap.den=sap.stems/micro, regen.den=(seed.den+sap.den))

  regen9<-if (units=='ha'){
    regen8 %>%
      mutate(seed15.30=(seed15.30*10000)/(pi*4),
        seed30.100=(seed30.100*10000)/(pi*4),
        seed100.150=(seed100.150*10000)/(pi*4),
        seed150p=(seed150p*10000)/(pi*4),
        seed.den=(seed.den*10000)/(pi*4),
        sap.den=(sap.den*10000)/(pi*4),
        regen.den=(regen.den*10000)/(pi*4))
  } else if (units=='acres'){
    regen8 %>%
      mutate(seed15.30=(seed15.30*4046.856)/(pi*4),
        seed30.100=(seed30.100*4046.856)/(pi*4),
        seed100.150=(seed100.150*4046.856)/(pi*4),
        seed150p=(seed150p*4046.856)/(pi*4),
        seed.den=(seed.den*4046.856)/(pi*4),
        sap.den=(sap.den*4046.856)/(pi*4),
        regen.den=(regen.den*4046.856)/(pi*4))
  } else if (units=='micro'){regen8
  }

  regen10<-regen9 %>% select(Event_ID,TSN,Latin_Name,Common,Exotic,Canopy_Exclusion,seed15.30,
    seed30.100,seed100.150, seed150p,seed.den,sap.den,regen.den,stock) %>% droplevels()

  regen11<-merge(park.plots,regen10,by="Event_ID",all.x=T)
  regen11[,17:24][is.na(regen11[,17:24])]<-0
  regen11[,13:14][is.na(regen11[,13:14])]<-'no species recorded'
  regen11<-regen11 %>% arrange(Plot_Name,Latin_Name)
  return(data.frame(regen11))
} # end of function

# Need to account for SAGA 008 being NA and ACAD 029 as being NA because missing data
