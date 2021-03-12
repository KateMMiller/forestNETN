#' @include joinLocEvent.R
#'
#' @importFrom dplyr case_when mutate
#' @importFrom magrittr %>%
#' @importFrom stringr str_pad
#' @importFrom tidyr spread
#'
#' @title joinStandData: compile stand data
#'
#' @description This function combines stand-level data for each plot, including cover by strata,
#' earthworms presence/absence, plot slope, canopy cover, etc. Must run importData first.
#'
#' @param park Combine data from all parks or one park at a time. Acceptable options are:
#' \describe{
#' \item{"all"}{Includes all parks in the network}
#' \item{"ACAD"}{Acadia NP only}
#' \item{"MABI"}{Marsh-Billings-Rockefeller NHP only}
#' \item{"MIMA"}{Minute Man NHP only}
#' \item{"MORR"}{Morristown NHP only}
#' \item{"ROVA"}{Roosevelt-Vanderbilt NHS only}
#' \item{"SAGA"}{Saint-Gaudens NHS only}
#' \item{"SARA"}{Saratoga NHP only}
#' \item{"WEFA"}{Weir Farm NHS only}}
#'
#' @param from Year to start analysis
#' @param to Year to stop analysis
#'
#' @param QAQC Allows you to remove or include QAQC events.
#' \describe{
#' \item{FALSE}{Default. Only returns visits that are not QAQC visits}
#' \item{TRUE}{Returns all visits, including QAQC visits}}
#'
#' @param locType Allows you to only include plots that are part of the GRTS sample design or include all plots, such as deer exclosures
#' \describe{
#' \item{"VS"}{Default. Only include plots that are part of the Vital Signs GRTS sample design}
#' \item{"all"}{Include all plots, such as deer exclosures and bonus plots}}
#'
#' @param panels Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
#' If more than one panel is selected, specify by c(1,3), for example.
#'
#' @return returns a dataframe with stand data attached to location and event data. Field names starting with "Pct" are midpoints
#' between cover class ranges (e.g., 62.5 is the midpoint for 50-75%).
#'
#' @examples
#' importData() #imports using default odbc
#' stand_df <- joinStandData(park = 'MABI', from = 2015, to = 2019)
#'
#'
#' @export
#'
#------------------------
# Join stand data
#------------------------
joinStandData<-function(park = 'all', QAQC = FALSE, locType = 'VS', panels = 1:4,
                        from = 2006, to = 2021, output = 'short', ...){

    # Match args and class
    park <- match.arg(park, several.ok = TRUE,
                      c("all", "ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA"))
    stopifnot(class(from) == "numeric", from >= 2006)
    stopifnot(class(to) == "numeric", to >= 2006)
    locType <- match.arg(locType)
    stopifnot(class(QAQC) == 'logical')
    stopifnot(panels %in% c(1, 2, 3, 4))
    output <- match.arg(output, c("short", "verbose"))

    env <- if(exists("VIEWS_NETN")){VIEWS_NETN} else {.GlobalEnv}

    # import the Stand Data views
    tryCatch(standinfo <- subset(get("NETN_StandInfoPhotos", envir = env),
                                 select = c(PlotID, EventID, ParkUnit, PlotCode, IsQAQC, StartYear, CrownClosureCode, CrownClosureLabel,
                                            StandStructureCode, StandStructureSummary, DeerBrowseLabel, WaterPlotCode,
                                            WaterPlotLabel, MicrotopographyCode, EarthwormCode, WeatherLabel, PhotoNotes)),
             error = function(e){stop("NETN_StandInfoPhotos view not found. Please import view.")}
    )

    tryCatch(pstrata <- subset(get("COMN_StandPlantCoverStrata", envir = env),
                               select = c(PlotID, EventID, ParkUnit, PlotCode, IsQAQC, StartYear, StrataCode,
                                          CoverClassLabel)),
             error = function(e){stop("COMN_StandPlantCoverStrata view not found. Please import view.")}
    )

    tryCatch(ffloor <- subset(get("COMN_StandForestFloor", envir = env),
                              select = c(PlotID, EventID, ParkUnit, PlotCode, IsQAQC, StartYear,
                                         ForestFloorCode, CoverClassLabel)),
             error = function(e){stop("COMN_StandForestFloor view not found. Please import view.")}
    )

    tryCatch(treeheights <- subset(get("COMN_StandTreeHeights", envir = env),
                                   select = c(PlotID, EventID, ParkUnit, PlotCode, IsQAQC, StartYear,
                                              CrownClassCode, CrownClassLabel, TagCode, Height)),
             error = function(e){stop("COMN_StandTreeHeights view not found. Please import view.")}
    )

    tryCatch(slopes <- subset(get("COMN_StandSlopes", envir = env),
                              select = c(PlotID, EventID, ParkUnit, PlotCode, IsQAQC, StartYear, PlotSlope)),
             error = function(e){stop("COMN_StandSlopes view not found. Please import view.")}
    )

    # standinfo comes in as 1 row per visit, so don't need to reshape, but need to fix cover midpoints and rename
    # to match previous package names
    standinfo$Plot_Name <- paste(standinfo$ParkUnit,
                                 stringr::str_pad(standinfo$PlotCode, 3, side = 'left', "0"), sep = "-")

    standinfo <- standinfo %>% mutate(Stand_Structure = ifelse(StandStructureCode == "PM",
                                                                 paste0("Permanently missing"),
                                                                 StandStructureSummary),
                                      Stand_Structure_Code = ifelse(StandStructureCode == "PM",
                                                                    NA, StandStructureCode),
                                      Pct_Crown_Closure = case_when(CrownClosureLabel == "<10%" ~ 5,
                                                                    CrownClosureLabel == "10-25%" ~ 17.5,
                                                                    CrownClosureLabel == "25-50%" ~ 37.5,
                                                                    CrownClosureLabel == "50-75%" ~ 62.5,
                                                                    CrownClosureLabel == "75-100%" ~ 87.5,
                                                                    CrownClosureLabel == "Permanently Missing" ~ NA_real_,
                                                                    TRUE ~ NA_real_),
                                      Deer_Browse_Index = case_when(DeerBrowseLabel == "Very Low" ~ 1,
                                                                    DeerBrowseLabel == "Low Impact" ~ 2,
                                                                    DeerBrowseLabel == "Moderate" ~ 3,
                                                                    DeerBrowseLabel == "High" ~ 4,
                                                                    DeerBrowseLabel %in% c("Very High", "Present") ~ 5,
                                                                    DeerBrowseLabel %in% c(
                                                                      "Absent", "Not Collected", "Permanently Missing") ~ NA_real_,
                                                                    TRUE ~ NA_real_),
                                      Earthworms = ifelse(EarthwormCode == "PM", NA, EarthwormCode),
                                      Microtopography = ifelse(MicrotopographyCode == "PM", NA, MicrotopographyCode)) %>%
                               select(PlotID:StartYear, Plot_Name, Stand_Structure, Stand_Structure_Code,
                                      Pct_Crown_Closure, CrownClosureLabel, Deer_Browse_Index, Microtopography,
                                      Earthworms, WaterPlotCode, WaterPlotLabel, WeatherLabel, PhotoNotes)

    old_scol <- c("CrownClosureLabel", "WaterPlotCode", "WaterPlotLabel", "WeatherLabel")
    new_scol <- c("Txt_Crown_Closure", "Water_on_Plot_Code", "Water_on_Plot", "Weather_Photo")

    names(standinfo)[match(old_scol, names(standinfo))] <- new_scol # change col names to match prev. analyses


    # reshape pstrata and ffloor so 1 row per event and add midpoint cover fields
    # pstrata- had to reshape in 2 steps, so PMs didn't get converted to NA in teh txt fields

    pstrata_pct_wide <- pstrata %>% mutate(Pct = case_when(CoverClassLabel == "0%" ~ 0,
                                                           CoverClassLabel == "1-5%" ~ 3,
                                                           CoverClassLabel == "5-25%" ~ 15,
                                                           CoverClassLabel == "25-50%" ~ 37.5,
                                                           CoverClassLabel == "50-75%" ~ 62.5,
                                                           CoverClassLabel == "75-95%" ~ 85,
                                                           CoverClassLabel == "95-100%" ~ 97.5,
                                                           CoverClassLabel == "Permanently Missing" ~ NA_real_,
                                                           TRUE ~ NA_real_)) %>%
                                select(-CoverClassLabel) %>%
                                pivot_wider(names_from = StrataCode, values_from = Pct,
                                            values_fill = NA) %>%
                                select(PlotID:StartYear, G, MU, HU)

    old_pcol <- c("G", "MU", "HU")
    new_pcolp <- c("Pct_Understory_Low", "Pct_Understory_Mid", "Pct_Understory_High")
    names(pstrata_pct_wide)[match(old_pcol, names(pstrata_pct_wide))] <- new_pcolp # change col names to match prev. analyses

    pstrata_txt_wide <- pstrata %>% select(PlotID:StartYear, StrataCode, CoverClassLabel) %>%
                                    pivot_wider(names_from = StrataCode, values_from = CoverClassLabel) %>%
                                    select(PlotID:HU)

    new_pcolt <- c("Txt_Understory_Low", "Txt_Understory_Mid", "Txt_Understory_High")
    names(pstrata_txt_wide)[match(old_pcol, names(pstrata_txt_wide))] <- new_pcolt # change col names to match prev. analyses

    pstrata_wide <- merge(pstrata_pct_wide, pstrata_txt_wide,
                          by = intersect(names(pstrata_pct_wide), names(pstrata_txt_wide)),
                          all.x = TRUE, all.y = TRUE)

    pstrata_wide <- pstrata_wide[,c("PlotID", "EventID", "ParkUnit", "PlotCode", "IsQAQC", "StartYear",
                                    "Pct_Understory_Low", "Txt_Understory_Low",
                                    "Pct_Understory_Mid", "Txt_Understory_Mid",
                                    "Pct_Understory_High", "Txt_Understory_High")]

    # forest floor
    ffloor_pct_wide <- ffloor %>% mutate(Pct = case_when(CoverClassLabel == "0%" ~ 0,
                                                         CoverClassLabel == "1-5%" ~ 3,
                                                         CoverClassLabel == "5-25%" ~ 15,
                                                         CoverClassLabel == "25-50%" ~ 37.5,
                                                         CoverClassLabel == "50-75%" ~ 62.5,
                                                         CoverClassLabel == "75-95%" ~ 85,
                                                         CoverClassLabel == "95-100%" ~ 97.5,
                                                         CoverClassLabel %in% c("Not Collected",
                                                                                "Permanently Missing") ~ NA_real_,
                                                         TRUE ~ NA_real_)) %>%
                                  select(-CoverClassLabel) %>%
                                  pivot_wider(names_from = ForestFloorCode, values_from = Pct,
                                              values_fill = NA) %>%
                                  select(PlotID:StartYear, BS, R, W, `T`, NV, L)

    old_fcol <- c("BS", "R", "W", "T", "NV", "L")
    new_fcolp <- c("Pct_Bare_Soil", "Pct_Rock", "Pct_Water", "Pct_Trampled", "Pct_Bryophyte", "Pct_Lichen")
    names(ffloor_pct_wide)[match(old_fcol, names(ffloor_pct_wide))] <- new_fcolp # change col names to match prev. analyses

    ffloor_txt_wide <- ffloor %>% pivot_wider(names_from = ForestFloorCode, values_from = CoverClassLabel) %>%
                                  select(PlotID:StartYear, BS, R, W, `T`, NV, L)

    new_fcolt <- c("Txt_Bare_Soil", "Txt_Rock", "Txt_Water", "Txt_Trampled", "Txt_Bryophyte", "Txt_Lichen")
    names(ffloor_txt_wide)[match(old_fcol, names(ffloor_txt_wide))] <- new_fcolt # change col names to match prev. analyses

    ffloor_wide <- merge(ffloor_pct_wide, ffloor_txt_wide,
                         by = intersect(names(ffloor_pct_wide), names(ffloor_txt_wide)),
                         all.x = TRUE, all.y = TRUE)

    head(ffloor_wide)

    ffloor_wide <- ffloor_wide[, c("PlotID", "EventID", "ParkUnit", "PlotCode", "IsQAQC", "StartYear",
                                   "Pct_Bare_Soil", "Txt_Bare_Soil", "Pct_Bryophyte", "Txt_Bryophyte",
                                   "Pct_Lichen", "Txt_Lichen", "Pct_Rock", "Txt_Rock", "Pct_Trampled",
                                   "Txt_Trampled", "Pct_Water", "Txt_Water")]

    merge_cols <- Reduce(intersect, list(names(standinfo), names(pstrata_wide), names(ffloor_wide)), names(slopes))

    stand_comb <- Reduce(function(x, y) merge (x = x, y = y, by = merge_cols, all.x = T, all.y = T),
                         list(standinfo, slopes, pstrata_wide, ffloor_wide)) %>% unique()

    #stand_comb[which(duplicated(stand_comb$EventID)),] #no dups

#++++++ ENDED HERE +++++ Tree heights are all that are left
# Consider making a new function that pulls in the stand heights and leaves individuals

  stand_long <- stand_df3 %>% select(Event_ID, Plot_Name, Height_Tree_1_Codom, Height_Tree_2_Codom,
                                    Height_Tree_3_Codom, Height_Tree_1_Inter,
                                    Height_Tree_2_Inter, Height_Tree_3_Inter) %>%
    gather('tree_number', 'height', -Event_ID, -Plot_Name) %>%
    arrange(Plot_Name)

  stand_long2<-na.omit(stand_long)
  stand_long2<-stand_long2 %>% mutate(CrownType= ifelse(grepl("Codom", tree_number), "Avg_Codom_HT",'Avg_Inter_HT'))

  stand_sum <- stand_long2 %>% group_by(Event_ID,Plot_Name, CrownType) %>%
    summarise(avg_height = round(mean(height, na.rm=T),2)) %>%
    spread(CrownType, avg_height, fill=NA) %>%
    arrange(Plot_Name)

  stand_comb<- merge(stand_df3, stand_sum, by=c("Event_ID","Plot_Name"), all.x=T)
names(stand_comb)

stand_df4<-stand_comb %>% select(Location_ID, Event_ID, Unit_Code, Plot_Name,Plot_Number:cycle, Stand_Structure_ID, Stand_Structure,
                                 Crown_Closure_ID, Pct_Crown_Closure, Avg_Codom_HT, Avg_Inter_HT,
                                 Deer_Browse_Line_ID, Microtopography_ID,
                                 Groundstory_Cover_Class_ID:Derived_Plot_Slope) %>% arrange(Plot_Name,cycle)

 names(stand_df4)[names(stand_df4)=='Groundstory_Cover_Class_ID']<-"Pct_Understory_Low"
 names(stand_df4)[names(stand_df4)=='Mid_Understory_Cover_Class_ID']<-"Pct_Understory_Mid"
 names(stand_df4)[names(stand_df4)=='High_Understory_Cover_Class_ID']<-"Pct_Understory_High"
 names(stand_df4)[names(stand_df4)=='Lichen_Cover_Class_ID']<-"Pct_Lichen_Cover"
 names(stand_df4)[names(stand_df4)=='Non_Vascular_Cover_Class_ID']<-"Pct_Bryophyte_Cover"
 names(stand_df4)[names(stand_df4)=='Forest_Floor_Bare_Soil_Cover_Class_ID']<-"Pct_Bare_Soil_Cover"
 names(stand_df4)[names(stand_df4)=='Forest_Floor_Rock_Cover_Class_ID']<-"Pct_Rock_Cover"
 names(stand_df4)[names(stand_df4)=='Forest_Floor_Water_Cover_Class_ID']<-"Pct_Surface_Water_Cover"
 names(stand_df4)[names(stand_df4)=='Forest_Floor_Trampled_Cover_Class_ID']<-"Pct_Trampled_Cover"
 names(stand_df4)[names(stand_df4)=='Derived_Plot_Slope']<-"Plot_Slope_Deg"

 worms<-soildata %>% select(Event_ID, Earthworms) %>% unique()

 #----
 plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                   panels = panels, locType = locType, eventType = "complete",
                                   abandoned = FALSE, output = 'short')) %>%
   select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
          xCoordinate, yCoordinate, EventID, StartDate, StartYear, cycle, IsQAQC)

 #----
 stand_df5<- merge(stand_df4, worms, by='Event_ID', all.x=T, all.y=F)

   return(stand_df5)
}
