#' @include joinLocEvent.R
#' @include joinAdditionalSpecies.R
#' @include joinCWDData.R
#' @include joinMicroSaplings.R
#' @include joinMicroSeedlings.R
#' @include joinMicroShrubData.R
#' @include joinQuadSpecies.R
#' @include joinTreeData.R
#' @include joinTreeVineSpecies.R
#' @include prepTaxa.R
#'
#' @title exportNPSForVeg
#'
#' @importFrom dplyr arrange filter first group_by last left_join mutate select summarize
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @description This function exports NETN forest data that are formatted to match flat
#' files that can be imported into the NPSForVeg R package. Abandoned plots, QAQC visits,
#' partial visits (e.g., ACAD-029-2010), and non-VS plots are not included in the export.
#'
#' @param export Logical. If TRUE, saves formatted csvs to specified path.
#'
#' @param path Quoted path to save files to. If not specified, will save to working directory.
#'
#' @param zip Logical. If TRUE, exports zip file of csvs with timestamp of date generated.
#' If FALSE (Default), exports individual csvs.
#'
#' @return NPSForVeg flatfiles
#'
#' @examples
#' \dontrun{
#' # RUN FIRST
#' library(forestNETN)
#' importData()
#'
#' exportNPSForVeg(export = T, path = "C:/NETN/R_Dev/data", zip = T)
#'
#' }
#'
#' @export
#'

exportNPSForVeg <- function(export = T, path = NA, zip = F){

  #---- Error handling ----
  stopifnot(class(export) %in% "logical")
  stopifnot(class(zip) %in% "logical")

  # Error handling for path
  if(export == TRUE){
    if(is.na(path)){path <- getwd()
      print(paste0("No path specified. Output saved to working directory: ", getwd()), quote = FALSE)
    } else if(!dir.exists(path)){
      stop("Specified directory does not exist.")
    } else{print(paste0("Output saving to ", path), quote = FALSE)}

    if(!grepl("/$", path)){path <- paste0(path, "/")} # add / to end of filepath if doesn't exist

    # Normalize path for zip
    pathn <- normalizePath(path)
    }

  #-- Compile CSVs for NPSForVeg --
  plot_evs1 <- joinLocEvent(output = 'verbose', eventType = "complete", QAQC = F)

  #---- Plots ----
  # Pull in alternative plot labels
  alturl <-
    "https://raw.githubusercontent.com/KateMMiller/forestSummaries/main/tbl_Alternative_Plot_Labels.csv"
  altlabs <- read.csv(alturl, quote = "", row.names = NULL)

  plot_evs <- left_join(plot_evs1,
                        altlabs |> select(Plot_Name, Unit, Fire, Eco_System), by = "Plot_Name") |>
    mutate(fire1947 = ifelse(Fire == "1947", "burned1947", "unburned"),
           Unit_Group = ifelse(ParkUnit %in% "ACAD", fire1947,
                          ifelse(ParkUnit %in% "MABI", Eco_System,
                                 ParkSubUnit)))
  #nrow(plot_evs) #1614 plot x event combos

  plot_count <- plot_evs |> group_by(Plot_Name) |>
    summarize(Event_Earliest = format(as.Date(first(SampleDate), format = "%Y-%m-%d"), "%Y%m%d"),
              Event_Latest = format(as.Date(last(SampleDate), format = "%Y-%m-%d"), "%Y%m%d"),
              Event_Count = sum(!is.na(SampleYear)))

  plots <- left_join(plot_count, plot_evs, by = "Plot_Name") |>
    mutate(Frame = "NETN",
           Location_Status = ifelse(IsAbandoned == FALSE, "Active", "Inactive")) |>
    select(Plot_Name, Unit_Code = ParkUnit, Unit_Group,
           Subunit_Code = ParkSubUnit, Panel = PanelCode, Frame,
           GRTS_Order = GRTS, Location_Status, Location_Notes = PlotNotes,
           Event_Count, Event_Earliest, Event_Latest,
           UTM_ZONE_NAD83_X = xCoordinate, UTM_ZONE_NAD83_Y = yCoordinate,
           Latitude = Lat, Longitude = Long, UTM_Zone = ZoneCode,
           Aspect = Aspect, Physiographic_Class = PhysiographySummary,
           StuntedWoodland = IsStuntedWoodland) |> unique()

  #---- Events ----
  events1 <- plot_evs |>
    mutate(Event_Date = format(as.Date(SampleDate, format = "%Y-%m-%d"), "%m/%d/%Y"),
           Event_Date_Txt = format(as.Date(SampleDate, format = "%Y-%m-%d"), "%Y%m%d"),
           Frame = "NETN",
           excludeEvent = 0) |> # ACAD-029-2010 already removed, so all = 0
    select(Event_ID = EventID, Plot_Name, Event_Date, Event_Date_Txt, Unit_Code = ParkUnit,
           Unit_Group, SubUnitCode = ParkSubUnit, ProtocolName = ProtocolPublishYear,
           Panel = PanelCode, Frame, Cycle = cycle, excludeEvent, SampleYear, IsQAQC, EventNotes)

  numquads <- joinQuadSpecies() |> select(Plot_Name, SampleYear, IsQAQC, num_quads) |> unique()

  nummicros <- joinMicroSaplings() |> filter(!SQSaplingCode %in% "NS") |>
    select(Plot_Name, SampleYear, IsQAQC, MicroplotCode, SQSaplingCode) |> unique() |>
    group_by(Plot_Name, SampleYear, IsQAQC) |>
    summarize(num_micros = sum(!is.na(MicroplotCode)), .groups = 'drop')

  stand <- joinStandData() |>
    select(Plot_Name, Stand_Structure_ID = Stand_Structure_Code,
           Crown_Closure_ID = CrownClosureCode,
           Plot_Slope_Degree = PlotSlope,
           SampleYear, IsQAQC)

  # Need cover codes instead of text/midpoints
  stand_und <- VIEWS_NETN$StandPlantCoverStrata_NETN |>
    mutate(Strata1 = gsub("-understory", "", StrataLabel),
           Strata = gsub("Ground", "Low", Strata1)) |>
    select(Plot_Name, SampleYear, IsQAQC, Strata, CoverClassCode) |>
    pivot_wider(names_from = "Strata", values_from = "CoverClassCode",
                names_prefix = "Groundstory_Cover_Class_") |> unique()

  stand_ff <- VIEWS_NETN$StandForestFloor_NETN |>
    mutate(label = gsub(" ", "_", ForestFloorLabel)) |>
    select(Plot_Name, SampleYear, IsQAQC, label, CoverClassCode) |>
    pivot_wider(names_from = label, values_from = CoverClassCode) |>
    select(Plot_Name, SampleYear, IsQAQC,
           Forest_Floor_Bare_Soil_Cover_Class_ID = Bare_Soil,
           Forest_Floor_Rock_Cover_Class_ID = Rock,
           Forest_Floor_Trampled_Cover_Class_ID = Trampled) |> unique()

  stand_dbi <- VIEWS_NETN$StandInfoPhotos_NETN |>
    select(Plot_Name, SampleYear, IsQAQC, Deer_Browse_Line_ID = DeerBrowseCode)

  ev_comb1 <- left_join(events1, numquads, by = c("Plot_Name", "SampleYear", "IsQAQC"))
  ev_comb2 <- left_join(ev_comb1, nummicros, by = c("Plot_Name", "SampleYear", "IsQAQC"))
  ev_comb3 <- left_join(ev_comb2, stand_und, by = c("Plot_Name", "SampleYear", "IsQAQC"))
  ev_comb4 <- left_join(ev_comb3, stand_ff, by = c("Plot_Name", "SampleYear", "IsQAQC"))
  ev_comb5 <- left_join(ev_comb4, stand, by = c("Plot_Name", "SampleYear", "IsQAQC"))
  ev_comb6 <- left_join(ev_comb5, stand_dbi, by = c("Plot_Name", "SampleYear", "IsQAQC"))

  events <- ev_comb6 |> select(Event_ID, Plot_Name, Event_Date, Event_Date_Txt,
                               Event_Year = SampleYear, Unit_Code, Unit_Group, Subunit_Code = SubUnitCode,
                               ProtocolName, Panel, Frame, Cycle, numHerbPlots = num_quads,
                               numSapPlots = num_micros, numSeedPlots = num_micros,
                               excludeEvent, Stand_Structure_ID, Crown_Closure_ID,
                               Deer_Browse_Line_ID, Groundstory_Cover_Class_Low, Groundstory_Cover_Class_Mid,
                               Groundstory_Cover_Class_High, Forest_Floor_Bare_Soil_Cover_Class_ID,
                               Forest_Floor_Rock_Cover_Class_ID, Forest_Floor_Trampled_Cover_Class_ID,
                               Plot_Slope_Degree)
  #---- MetaData ----
  meta <- data.frame(ParkCode = c("ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA"),
                     ShortName = c("Acadia", "Marsh-Billings-Rockefeller",
                                   "Minute Man", "Morristown", "Roosevelt-Vanderbilt",
                                   "Saint-Gaudens", "Saratoga", "Weir Farm"),
                     LongName = c("Acadia National Park", "Marsh-Billings-Rockefeller National Historical Park",
                                  "Minute Man National Historical Park", "Morristown National Historical Park",
                                  "Roosevelt-Vanderbilt National Historic Sites",
                                  "Saint-Gaudens National Historical Park", "Saratoga National Historical Park",
                                  "Weir Farm National Historical Park"),
                     Network = rep("NETN", 8),
                     TPlotNum = rep(1, 8),
                     SapPlotNum = rep(3, 8),
                     SeedPlotNum = rep(3, 8),
                     SeedPlotSize = rep(12.57, 8),
                     ShrubPlotNum = rep(3, 8),
                     ShrubPlotSize = rep(12.57, 8),
                     ShSeedPlotNum = NA_real_,
                     ShSeedPlotSize = NA_real_,
                     VPlotNum = c(225, rep(400, 7)),
                     VPlotSize = rep(1, 8),
                     HPlotNum = rep(8, 8),
                     HPlotSize = rep(1, 8))

  #---- Cycles ----
  # REMOVED CYCLES FROM NETN/MIDN NPSForVeg package

  #---- CommonNames ----
  plants1 <- prepTaxa() |>
    mutate(Woody = ifelse(Tree + TreeShrub + Shrub + Vine > 0, TRUE, FALSE),
           Targeted_Herb = ifelse(DeerIndicatorHerb + InvasiveNETN > 0, TRUE, FALSE),
           Tree = ifelse(Tree + TreeShrub > 0, TRUE, FALSE), # inclusive for shrubs >10cm dbh
           Shrub = ifelse(Shrub + TreeShrub > 0, TRUE, FALSE)) |> # inclusive for shrubs >10cm dbh
    select(Latin_Name = ScientificName, NCRN_Common = CommonName, Common = CommonName,
           Family, Genus, Species, TSN, Woody, Herbaceous, Targeted_Herb, Tree, Shrub,
           Vine, Exotic, Graminoid, Fern_Ally = FernAlly)

  plants_code <- VIEWS_NETN$Taxa_NETN |> select(TSN, TaxonCode)
  plants <- left_join(plants1, plants_code, by = "TSN")

  #---- Trees ----
  table(trees$TreeStatusCode)

  live <- c("1", "AB", "AF", "AL", "AM", "AS", "RB", "RF", "RL", "RS")
  dead <- c("2", "DB", "DC", "DF", "DL", "DM", "DS")

  trees1 <- joinTreeData(status = 'active', output = 'verbose') |>
    mutate(Date = format(as.Date(SampleDate, format = "%Y-%m-%d"), "%Y%m%d"),
           TaxonCode = NA_real_,
           SumLiveBasalArea_cm2 = ifelse(TreeStatusCode %in% live, BA_cm2, 0),
           Equiv_Live_DBH_cm = ifelse(TreeStatusCode %in% live, DBHcm, 0),
           SumDeadBasalArea_cm2 = ifelse(TreeStatusCode %in% dead, BA_cm2, 0),
           Equiv_Dead_DBH_cm = ifelse(TreeStatusCode %in% dead, DBHcm, 0))

  tree_stat <- data.frame(stat = c("1", "AB", "AF", "AL", "AM", "AS",
                                   "2", "DB", "DC", "DF", "DL", "DM", "DS",
                                   "RB", "RF", "RL", "RS"),
                          label = c("Alive", "Alive Broken", "Alive Fallen",
                                    "Alive Leaning", "Alive Missed", "Alive Standing",
                                    "Dead", "Dead Broken", "Dead Cut", "Dead Fallen",
                                    "Dead Leaning", "Dead Missed", "Dead Standing",
                                    "Recruit Broken", "Recruit Fallen", "Recruit Leaning",
                                    "Recruit Standing"))

  trcond <- joinTreeConditions(status = 'active') |>
    mutate(CondAD = ifelse(AD == 1, paste0("Advanced Decay"), NA_character_),
           CondDBT = ifelse(DBT == 1, paste0("Large Dead Branches"), NA_character_),
           Condition1 = paste(CondAD, CondDBT, sep = ", "),
           Condition = gsub("NA,| NA|, NA", "", Condition1)) |>
    select(Plot_Name, SampleYear, IsQAQC, TagCode, Condition)

  tree_comb <- left_join(trees1, tree_stat, by = c("TreeStatusCode" = "stat"))
  tree_comb2 <- left_join(tree_comb, trcond, by = c("Plot_Name", "SampleYear", "IsQAQC", "TagCode"))
  tree_comb3 <- left_join(tree_comb2, plots |> select(Plot_Name, Unit_Group), by = "Plot_Name")

  trees <- tree_comb3 |>
    select(Plot_Name, Unit_Code = ParkUnit, Unit_Group, Subunit_Code = ParkSubUnit,
           Cycle = cycle, Panel = PanelCode, Sample_Year = SampleYear, Date,
           Tag = TagCode, TSN, TaxonCode, Latin_Name = ScientificName, Stems = num_stems,
           SumLiveBasalArea_cm2, Equiv_Live_DBH_cm,
           SumDeadBasalArea_cm2, Equiv_Dead_DBH_cm,
           Status = label,
           Crown_Class = CrownClassCode,
           Crown_Description = CrownClassLabel,
           DBH_Status = IsDBHUnusual,
           DecayClass = DecayClassCode)

  #---- Saplings ----
  saps1 <- joinMicroSaplings() |>
    mutate(#BA_cm2 = round(pi*((DBHcm/2)^2),4),
           Status = "Alive",
           Habit = "Tree",
           Browsed = NA_character_,
           Browsable = NA_character_,
           Date = format(SampleDate, "%Y%m%d"),
           Tag = NA,
           TaxonCode = NA) |>
    filter(!SQSaplingCode %in% "NS")

  saps1$rep = ifelse(saps1$Count <= 1, 1, saps1$Count)
  #sum(saps1$Count) #7550 live saplings; sum(saps1$rep) #9869 sapling records, including 0s
  saps_long <- saps1[rep(1:nrow(saps1), saps1$rep),]

  saplings <- left_join(saps_long, plots |> select(Plot_Name, Unit_Group), by = "Plot_Name") |>
    mutate(Count = ifelse(Count == 0, 0, 1),
           StemsDead = 0,
           SumLiveBasalArea_cm2 = round(pi*((DBHcm/2)^2),4),
           SumDeadBasalArea_cm2 = 0,
           Equiv_Dead_DBH_cm = 0,
           Browsed = NA_character_,
           Browsable = NA_character_) |>
    select(Plot_Name, Unit_Code = ParkUnit, Unit_Group, Subunit_Code = ParkSubUnit,
           Cycle = cycle, Panel = PanelCode, Frame = Network, Sample_Year = SampleYear,
           Date, Tag, TSN, TaxonCode, Latin_Name = ScientificName, StemsLive = Count,
           StemsDead, SumLiveBasalArea_cm2, SumDeadBasalArea_cm2,
           Equiv_Live_DBH_cm = DBHcm, Equiv_Dead_DBH_cm, Status, Habit, Browsed, Browsable)

  #---- Seedlings ----
  seeds <- joinMicroSeedlings()

  #---- Herbs ----
  herbs <- joinQuadSpecies()

  #---- Vines ----
  Vines <- joinTreeVineSpecies()

  #---- CWD ----
  cwd <- joinCWDData()


  }
