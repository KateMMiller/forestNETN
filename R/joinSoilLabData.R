#' @include joinLocEvent.R
#'
#' @importFrom dplyr select filter arrange mutate summarise group_by case_when summarise_at rename
#' @importFrom magrittr %>%
#'
#' @title joinSoilLabData: compile soil chemistry data by horizon.
#'
#' @description This function verifies whether O and A horizons were named corrected based on % Total Carbon.
#' For duplicate horizons on a plot, chemistry variables are corrected using weighted averages, with sample depth
#' as the weight. Must run importData first. Note that Earthworms are summarized in joinStandData().
#' Note that data starts at 2007 because 2006 methods were pretty different.
#'
#' @return returns a dataframe containing each plot and visit with soil chemistry data for each horizon on a plot
#' Plots that weren't sampled during a given cycle are not returned. Horizon depths are averaged across samples.
#'
#' @examples
#' importData() #imports using default odbc
#'# join only O horizon data for most recent cycle in ACAD. Note soil chemistry data are typically a year behind plot data.
#' soil_ACAD_O <- joinSoilLabData(park = 'ACAD', from = 2015, to = 2018, layers = 'O')
#'
#'# join all park data from all layers and all years
#' soil_df_all <- joinSoilLabData(from = 2007, to = 2018, layers = 'all')
#'
#' @export
#'
#------------------------
# Join soil lab data
#------------------------
joinSoilLabData <- function(park = 'all', from = 2007, to = 2018, layers = c('all', 'O', 'A'),
                            QAQC = FALSE, locType = 'VS', panels = 1:4, output, ...){

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
    summarise(Litter_cm = mean(Litter_Depth, na.rm = T), O_Hor_cm = mean(FF_Depth, na.rm=T),
              A_Hor_cm = mean(A_Horizon_Depth, na.rm = T), Total_Depth = mean(Total_Excavation_Depth,na.rm=T)) %>%
                arrange(Plot_Name, Year) %>% ungroup() %>%
    mutate(Total_Depth=ifelse(is.na(Total_Depth) | Total_Depth == 0, O_Hor_cm+A_Hor_cm, Total_Depth))
  # leave soil4 for later

  soillab2<-soillab %>% select(Soil_Data_ID, Layer, soil.pH:Ca, K.Analysis, Mg, P.Analysis, Al.Analysis,
                               Fe.Analysis, Mn, Na.Analysis, Zn.Analysis, acidity.Analysis, ECEC)

  # Set up chemistry lab data
  soil_plots <- soil4 %>% select(Event_ID, Soil_Data_ID, Unit_Code, Plot_Name, Plot_Number, Year, cycle) %>% unique()
  soil_comb <- merge(soil_plots, soillab2, by='Soil_Data_ID',all.x=T, all.y=F)
  colnames(soil_comb) <- c( "Soil_Data_ID", "Event_ID", "Unit_Code", "Plot_Name", "Plot_Number", "Year", "cycle", "Layer",
                          "soilpH", "LOI", "TN", "TC", "Ca", "K", "Mg", "P", "Al", "Fe", "Mn", "Na",
                          "Zn", "acidity", "ECEC")
  soil_comb <- soil_comb %>% mutate(Ca_Al = (Ca/40.078)/(Al/26.981),
                                    C_N   = TC/TN,
                                    Ca.meq = Ca/((40.08/2)*10),
                                    K.meq = K/(39.1*10),
                                    Mg.meq = Mg/((24.31/2)*10),
                                    Na.meq = Na/((22.29)*10),
                                    Al.meq = Al/((26.98/3)*10),
                                    Fe.meq = Fe/((55.85/2)*10),
                                    Mn.meq = Mn/((54.94/2)*10),
                                    Zn.meq = Zn/((65.39/2)*10),
                                    BaseSat = ((Ca.meq + K.meq + Mg.meq + Na.meq)/ECEC)*100,
                                    CaSat = ((Ca.meq)/ECEC)*100,
                                    AlSat = ((Al.meq)/ECEC)*100)

  # combine lab data with sample data for QC
  soil_comb2 <- merge(soil_comb, soil4[,c('Event_ID','Soil_Data_ID','O_Hor_cm','A_Hor_cm','Total_Depth')],
                      by=c('Event_ID','Soil_Data_ID'), all.x=T, all.y=T)

  names(soil_comb2)
  hor_names<-c("O_Hor_cm","A_Hor_cm","Total_Depth")
  soil_comb2[,hor_names][is.na(soil_comb2[,hor_names])]<-0

  exc_layers <- c('10cm - EXC', 'A - EXC', 'N/A - EXC', 'O - EXC', '20 cm')

  soil_qc <- soil_comb2 %>% filter(!(O_Hor_cm>10)) %>% filter(!(A_Hor_cm>10)) %>% filter(!(Total_Depth>20)) %>%
    filter(!(Layer %in% exc_layers)) %>% filter(!(is.na(TC))) %>% droplevels()

  soil_qc2 <- soil_qc %>% mutate(layer_orig = Layer, layer_new = ifelse(TC>=20, "O", "A"),
                                 Hor_Depth = case_when(Plot_Name == 'MORR-012' & Year == 2007 ~ A_Hor_cm,
                                                layer_orig == 'O' & O_Hor_cm > 0  ~ O_Hor_cm,
                                                layer_orig == 'O' & O_Hor_cm == 0 & A_Hor_cm > 0 ~ Total_Depth - A_Hor_cm,
                                                layer_orig == 'O' & O_Hor_cm == 0 & A_Hor_cm == 0 ~ Total_Depth,
                                                layer_orig == 'A' & A_Hor_cm > 0 ~ A_Hor_cm,
                                                layer_orig == 'A' & A_Hor_cm == 0 & O_Hor_cm > 0 ~ Total_Depth - O_Hor_cm,
                                                layer_orig == 'A' & A_Hor_cm == 0 & O_Hor_cm == 0 ~ Total_Depth,

                                                !(layer_orig %in% c('O','A')) ~ Total_Depth))

  # Ready for weighted averages
 soil_qc3 <- soil_qc2 %>% group_by(Plot_Name, Year, layer_new) %>% mutate(num_samps = n()) %>% ungroup() %>%
   select(Event_ID:cycle, layer_new, Hor_Depth, soilpH:AlSat)

 soilvars <- names(soil_qc3[,10:37])

 soil_qc4 <- soil_qc3 %>% group_by(Event_ID, Soil_Data_ID, Unit_Code, Plot_Name, Plot_Number, Year, cycle, Year, layer_new) %>%
      summarise_at(vars(soilvars), list(~weighted.mean(., Hor_Depth))) %>% rename(Horizon=layer_new)

 soil_qc4b <- soil_qc3 %>% group_by(Event_ID, Soil_Data_ID, layer_new) %>%
   summarise(Hor_Depth = sum(Hor_Depth)) %>% rename(Horizon=layer_new)

 soil_qc5 <- merge(soil_qc4, soil_qc4b, by=c('Event_ID','Soil_Data_ID','Horizon'), all.x=T, all.y=T) %>%
   select(Event_ID, Soil_Data_ID, Unit_Code:cycle, Horizon, Hor_Depth, everything())

 soil_qc_final <- if(layers == 'A'){
   soil_qc5 %>% filter(Horizon == 'A') %>% droplevels()
 } else if (layers == 'O'){
   soil_qc5 %>% filter(Horizon == 'O') %>% droplevels()
 } else if (layers == 'all'){soil_qc5}

 return(soil_qc_final)
 }
