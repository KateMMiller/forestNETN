#' @include joinQuadData.R
#' @title sumQuadGuilds
#'
#' @description This function summarizes output from joinQuadData and calculates average cover and quadrat frequency for each guild. Average cover is corrected for number of quadrats sampled.
#'
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' \item{"invasive"}{Returns species on the Indicator Invasive List}
#' }
#'
#' @return Returns a dataframe with average quadrat cover, percent quadrat frequency and quadrat frequency count for tree,shrub/vine,herbaceous,and graminoid.
#'
#' @export
#'
#------------------------
# Joins quadrat tables and filters by park, year, and plot/visit type
#------------------------
sumQuadGuilds<-function(speciesType='all', park='all',from=2006, to=2018, QAQC=FALSE, locType='VS', output,...){
  # Prepare the quadrat data
  park.plots<-force(joinLocEvent(park=park,from=from,to=to,QAQC=QAQC,locType=locType,output='short'))
  quads1<-force(joinQuadData(park=park, from=from,to=to,QAQC=QAQC,locType=locType,speciesType=speciesType,
    output='short'))
  quads1<-quads1 %>% mutate(Shrub=ifelse(Shrub+Vine>0,1,0)) %>% select(-Vine)

  # gather to put quadrats all in one column
  quads2<-quads1 %>% select(Event_ID:Event_QAQC,numHerbPlots,UC:UR,Tree:Indicator_Invasive_NETN) %>%
    gather(key='quad',value='pctcov',UC:UR)

  # replace 1/0 for guilds with percent cover values where they're present
  quads3<-quads2 %>% mutate(Tree=ifelse(Tree==1,pctcov,0),Shrub=ifelse(Shrub==1,pctcov,0),
    Herbaceous=ifelse(Herbaceous==1,pctcov,0), Graminoid=ifelse(Graminoid==1,pctcov,0))

  # gather to put all guild in one column. Have to remove 0s
  quads4<-quads3 %>% select(Event_ID:Graminoid,quad) %>% gather(key=guild,value=cover,Tree:Graminoid) %>%
    filter(cover>0)

  # summarise cover by quadrat and guild. eg if two shrubs are present in the same quad, their cover is summed
  quads5<-quads4 %>% select(Event_ID,Plot_Name,Year,numHerbPlots:cover) %>%
    group_by(Event_ID,Plot_Name,Year,quad,guild) %>%
    summarise(cover=sum(cover),numHerbPlots=first(numHerbPlots))

  # calculate average quadrat cover and frequency
  #quads6<-quads5 %>% group_by(Event_ID,Plot_Name,Year,guild) %>%
  #  summarize(avecov=sum(cover)/first(numHerbPlots),quadfreq=length(cover>0),
  #  avefreq=length(cover>0)/first(numHerbPlots))

  # wrangle data so that the end result is one guild column with tree, shrub, herb, gram and one field for each
  # percent cover, percent frequency and quadrat frequency count.
  # percent cover
  quads.pctcov<-quads5 %>% select(Event_ID:guild,cover) %>% spread(guild,cover,fill=0) %>% arrange(Plot_Name,Year)
  quads.pctcov2<-merge(park.plots,quads.pctcov[,c("Event_ID","Graminoid","Herbaceous","Shrub","Tree")],
    by=c("Event_ID"), all.x=T)
  quads.pctcov2[,12:15][is.na(quads.pctcov2[,12:15])]<-0
  quads.pctcov3<-quads.pctcov2 %>% gather(guild,cover,Graminoid:Tree)
  # quadrat frequency
  quads.freq<-quads6 %>% select(Event_ID:guild,quadfreq) %>% spread(guild,quadfreq,fill=0) %>% arrange(Plot_Name,Year)
  quads.freq2<-merge(park.plots,quads.freq[,c("Event_ID","Graminoid","Herbaceous","Shrub","Tree")],
    by=c("Event_ID"), all.x=T)
  quads.freq2[,12:15][is.na(quads.freq2[,12:15])]<-0
  quads.freq3<-quads.freq2 %>% gather(guild,quadfreq,Graminoid:Tree)
  head(quads.freq3)
  # quadrat percent frequency
  quads.pctfreq<-quads6 %>% select(Event_ID:guild,avefreq) %>% spread(guild,avefreq,fill=0) %>% arrange(Plot_Name,Year)
  quads.pctfreq2<-merge(park.plots,quads.pctfreq[,c("Event_ID","Graminoid","Herbaceous","Shrub","Tree")],
    by=c("Event_ID"), all.x=T)
  quads.pctfreq2[,12:15][is.na(quads.pctfreq2[,12:15])]<-0
  quads.pctfreq3<-quads.pctfreq2 %>% gather(guild,avefreq,Graminoid:Tree)

  #combine to get final dataframe
  quad.comb1<-merge(quads.pctcov3,quads.freq3[,c("Event_ID","guild","quadfreq")],
    by=c("Event_ID","guild"), all.x=T,all.y=T)
  quad.comb2<-merge(quad.comb1,quads.pctfreq3[,c("Event_ID","guild","avefreq")],
    by=c("Event_ID","guild"), all.x=T,all.y=T)
  quads.final<-quad.comb2 %>% select(Location_ID,Event_ID,Unit_Code:cycle,guild,avecov:avefreq)

  head(quads.final)
  return(data.frame(quads.final))
} # end of function

