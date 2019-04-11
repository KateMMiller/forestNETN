#' @include joinQuadData.R
#' @title sumQuadGuilds: summarizes quadrat species data by guilds
#'
#' @description This function summarizes output from joinQuadData and calculates average cover and quadrat frequency for each guild.
#' Average cover is corrected for number of quadrats sampled. Guilds are tree, shrub, forb, fern, and graminoid. If herbaceous guild
#' is split, then cover of ferns does not overlap with cover of herbaceous. If herbaceous guild is not split, then cover of herbaceous
#' guild includes fern and other herbaceous (but not graminoid) species cover.
#'
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Returns all species.}
#' \item{"native"}{Default. Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' \item{"invasive"}{Returns species on the Indicator Invasive List}
#' }
#'
#'@param splitHerb TRUE/FALSE. If TRUE (default), allows you to split the herbaceous group into forb and fern. If FALSE,
#' then resulting data frame will be summarised for tree, shrub, herbaceous, and graminoid guilds.
#' @return Returns a dataframe with average quadrat cover, percent quadrat frequency and quadrat frequency count for tree,shrub/vine,herbaceous,and graminoid. Data are sither summarized for all species, native only, exotic only, or invasive only.
#'
#' @export
#'
#------------------------
# Joins quadrat tables and filters by park, year, and plot/visit type
#------------------------
sumQuadGuilds<-function(speciesType=c('native','exotic','all', 'invasive'), park='all',from=2006, to=2018,splitHerb=TRUE,
  QAQC=FALSE, locType='VS', panels=1:4, output, ...){

  if(!requireNamespace("tidyr", quietly = TRUE)){
    stop("Package 'tidyr' needed for this function to work. Please install it.", call. = FALSE)
  }

  speciesType<-match.arg(speciesType)
  # Prepare the quadrat data
  park.plots<-force(joinLocEvent(park=park,from=from,to=to,QAQC=QAQC,locType=locType, panels=panels,output='short'))
  quads1<-force(joinQuadData(park=park, from=from,to=to,QAQC=QAQC,locType=locType,speciesType=speciesType,
    output='short'))
  quads1<-quads1 %>% mutate(Tree=ifelse(Tree+Shrub>1,0,Tree),Shrub=ifelse(Tree+Shrub>1,1,Shrub)) %>%
    filter(Latin_Name!='No species recorded' & Latin_Name!="Unknown species") %>% droplevels()

  quads2<-if (speciesType=='native'){filter(quads1,Exotic==FALSE)
  } else if (speciesType=='exotic'){filter(quads1,Exotic==TRUE)
  } else if (speciesType=='invasive'){filter(quads1,Indicator_Invasive_NETN==TRUE)
  } else if (speciesType=='all'){(quads1)
  }

  # gather to get every combination of plot visit and guild. Does not include germinants
  quads3<-quads2 %>% group_by(Event_ID,Tree,Shrub,Herbaceous,Graminoid, Fern_Ally) %>% summarise(avg.cover=sum(avg.cover),
    UC=ifelse(sum(UC)>0,1,0),UR=ifelse(sum(UR)>0,1,0),MR=ifelse(sum(MR)>0,1,0),BR=ifelse(sum(BR)>0,1,0),
    BC=ifelse(sum(BC)>0,1,0),BL=ifelse(sum(BL)>0,1,0),ML=ifelse(sum(ML)>0,1,0),UL=ifelse(sum(UL)>0,1,0),
    avg.freq=(UC+UR+MR+BR+BC+BL+ML+UL)/first(numHerbPlots)) %>%
      mutate(guild= if(splitHerb==TRUE) {
               case_when(Tree == 1 ~ 'Tree',
                            Shrub == 1 ~ 'Shrub',
                            Herbaceous == 1 & Fern_Ally!= 1 ~ 'Herbaceous',
                            Fern_Ally == 1 ~ 'Fern',
                            Graminoid == 1 ~ 'Graminoid')
      } else {
               case_when(Tree == 1 ~ 'Tree',
                          Shrub == 1 ~ 'Shrub',
                          Herbaceous == 1  ~ 'Herbaceous',
                           Graminoid == 1 ~ 'Graminoid')
              }) %>% ungroup() %>% select(Event_ID,guild,avg.cover,avg.freq)

  quads3$guild<-as.factor(quads3$guild)

  park.plots2<-park.plots %>% mutate(Graminoid=1,Herbaceous=1,Fern=1,Shrub=1,Tree=1) %>%
    tidyr::gather(key=guild,value=pres,Graminoid:Tree) %>% select(-pres)
  # makes a matrix with every plot visit and every combination of guild

  quads.comb1<-merge(park.plots2,quads3,by=c("Event_ID","guild"),all.x=T)
  quads.comb1[,13:14][is.na(quads.comb1[,13:14])]<-0
  quads.comb2<-if(splitHerb==TRUE) {quads.comb1
               } else if (splitHerb==FALSE) {filter(quads.comb1, guild!='Fern') %>% droplevels()}

  quads.comb3<-quads.comb2 %>% select(Location_ID,Event_ID,Unit_Code:cycle,guild,avg.cover:avg.freq) %>%
    arrange(Plot_Name,Year,guild)

  return(data.frame(quads.comb3))
} # end of function

