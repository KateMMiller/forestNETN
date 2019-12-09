#' @include joinLocEvent.R
#' @include joinSoilLabData.R
#'
#' @importFrom dplyr select filter arrange mutate summarise group_by case_when summarise_at rename left_join
#' @importFrom magrittr %>%
#' @importFrom tidyr spread
#'
#' @title joinSoilSampleData: compile corrected soil sample data.
#'
#' @description This function verifies whether O and A horizons were named corrected based on % Total Carbon using
#' the joinSoilLabData() function, then compiles sample depth by horizon. Must run importData first.
#' Note that Earthworms are summarized in joinStandData(). Note that data starts at 2007 because 2006 methods were pretty different.
#'
#' @return returns a dataframe containing each plot and visit with soil sample data.Tot_Samp_cm is the total depth of O and A sampled.
#' Litter is not included in total depth calculation. Note soil chemistry data are typically a year behind plot data,
#' so correcteed soil horizon depths will also be a year behind. Plots that weren't sampled during a given cycle are
#' not returned. Horizon depths are averaged across samples of the same horizon type.
#'
#' @examples
#' importData() #imports using default odbc
#'# join horizon dpeth data for most recent cycle in ACAD.
#' soil_ACAD_O <- joinSoilSampleData(park = 'ACAD', from = 2015, to = 2018)
#'
#' @export
#'
#------------------------
# Join soil lab data
#------------------------
joinSoilSampleData <- function(park = 'all', from = 2007, to = 2018, QAQC = FALSE,
                               locType = 'VS', panels = 1:4, output, layers = 'all', ...){

  layers<-match.arg(layers)

  park.plots <- force(joinLocEvent(park = park, from = from, to = to, QAQC = QAQC,
                                   locType = locType, panels = panels, output = 'short'))

  park.plots <- park.plots %>% filter(Plot_Name!='SARA-015') %>% droplevels() #superfund site- we don't sample soils there

  soil1 <- merge(park.plots,
                 soildata[,c('Soil_Data_ID','Event_ID','Sample_Type','Horizon_Type','Archived','Notes')],
                 by='Event_ID', all.x=T, all.y=F)

  # drop visits that weren't sampled for soils and make sure 2006 is removed
  soil2 <- soil1 %>% filter(!grepl('Soil not scheduled for sampling this year', Notes)) %>%
    filter(Year > 2006) %>% select(-Notes) %>% droplevels()


  # join next level of soil data containing horizon depths
  soil3 <- merge(soil2, soilsamp[,c('Soil_Sample_Data_ID','Soil_Data_ID','Sample_Number','Litter_Depth',
                                    'FF_Depth','A_Horizon_Depth', "Total_Excavation_Depth")],
                 by= 'Soil_Data_ID', all.x=T, all.y=F)

  # summarize horizon depths by event
  soil4 <- soil3 %>% group_by(Event_ID, Soil_Data_ID, Unit_Code, Plot_Name, Plot_Number, Year, cycle) %>%
    summarise(Litter_cm = mean(Litter_Depth, na.rm = T)) %>%  arrange(Plot_Name, Year) %>% ungroup() %>%
    mutate(Horizon = 'Litter') %>% rename(Hor_Depth = Litter_cm)

  # join soil chem data to have correct horizons based on %TC check
  soil_chem <- force(joinSoilLabData(park = park, from = from, to = to, layers = 'all',
                                     QAQC = QAQC, locType = locType, panels = panels, output = 'short'))

  soil_chem2 <- soil_chem %>% select(Event_ID:cycle, Horizon, Hor_Depth)

  # bind depths for litter and O and A layers
  soil_comb <- rbind(soil4, soil_chem2)

  soil_comb2 <- soil_comb %>% spread(Horizon, Hor_Depth, fill = 0) %>%
    rename(A_Hor_cm = A, O_Hor_cm = O, Litter_cm = Litter) %>%
    select(Event_ID:cycle, Litter_cm, O_Hor_cm, A_Hor_cm) %>% mutate(Tot_Samp_cm = O_Hor_cm + A_Hor_cm)

  soil_comb_final <- soil_comb2 %>% left_join(., park.plots[,c('Location_ID','Event_ID', 'X_Coord','Y_Coord','Panel','Event_QAQC')], by='Event_ID') %>%
    select(Location_ID, Event_ID, Soil_Data_ID, Unit_Code, Plot_Name, Plot_Number, X_Coord, Y_Coord, Panel, Year, Event_QAQC,
           cycle, Litter_cm, O_Hor_cm, A_Hor_cm, Tot_Samp_cm) %>% arrange(Plot_Name, cycle)

  return(soil_comb_final)
}
