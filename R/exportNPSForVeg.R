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
#' Note the every year after 2024, the cycles code will need to be updated.
#'
#' @param keep Logical. If TRUE (default), assigns NPSForVeg objects to global environment.
#' If FALSE, does not return output, which is useful when export = T.
#'
#' @param export Logical. If TRUE (default), saves formatted csvs to specified path.
#'
#' @param path Quoted path to save files to. If not specified, will save to working directory.
#'
#' @param zip Logical. If TRUE, exports zip file of csvs with timestamp of date generated.
#' If FALSE (default), exports individual csvs.
#'
#' @return NPSForVeg flatfiles
#'
#' @examples
#' \dontrun{
#' # RUN FIRST
#' library(forestNETN)
#' importData()
#' filepath <- "C:/NETN/R_Dev/data/NPSForVeg/NETN"
#' exportNPSForVeg(export = T, path = filepath, keep = T)
#' exportNPSForVeg(export = T, path = filepath, keep = F)
#' exportNPSForVeg(export = T, path = filepath, keep = F, zip = T)
#'
#' }
#'
#' @export
#'

exportNPSForVeg <- function(export = T, path = NA, zip = F, keep = T){

  #---- Error handling ----
  stopifnot(class(export) %in% "logical")
  stopifnot(class(zip) %in% "logical")
  if(keep == FALSE & export == FALSE){stop("Must either specify keep = T or export = T for function to return anything.")}

  # Check that suggested package required for this function are installed
  if(!requireNamespace("zip", quietly = TRUE) & zip == TRUE){
    stop("Package 'zip' needed to export to zip file. Please install it.", call. = FALSE)
  }

  env <- if(exists("VIEWS_NETN")){VIEWS_NETN} else {.GlobalEnv}

  tryCatch(test <- get("Events_NETN", envir = env),
           error = function(e){stop("NETN Forest Views not found. Please import views first.")})

  # Error handling for path
  if(export == TRUE){
    if(is.na(path)){path <- getwd()
      print(paste0("No path specified. Output saved to working directory: ", getwd()), quote = FALSE)
    } else if(!dir.exists(path)){
      stop("Specified directory does not exist.")
    } else{print(paste0("Output saving to ", path), quote = FALSE)}

    if(!grepl("/$", path)){path <- paste0(path, "/")} # add / to end of filepath if doesn't exist
  }
  if(zip == TRUE){
    # Normalize path for zip
    pathn <- normalizePath(path)
    if(!grepl("/$", pathn)){pathn <- paste0(pathn, "\\")}
    }

  if(export == FALSE){print("Compiling NPSForVeg data", quote = F)}

  maxpb = ifelse(export == FALSE, 10, 20)
  pb <- txtProgressBar(min = 0, max = maxpb, style = 3)
  x <- 1
  setTxtProgressBar(pb, x)

  #-- Compile CSVs for NPSForVeg --
  plot_evs1 <- joinLocEvent(output = 'verbose', eventType = "complete", QAQC = F)

  #---- Plots ----
  # Pull in alternative plot labels.
  # If this url fails, access via:
      # "Z:/PROJECTS/MONITORING/Forest_Health/5_Data/Database/EI Scorecard/tbl_Alternate_Plot_Labels.csv"
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
    mutate(Frame = ParkUnit,
           Location_Status = ifelse(IsAbandoned == FALSE, "Active", "Inactive")) |>
    select(Plot_Name, Unit_Code = ParkUnit, Unit_Group,
           Subunit_Code = ParkSubUnit, Panel = PanelCode, Frame,
           GRTS_Order = GRTS, Location_Status, Location_Notes = PlotNotes,
           Event_Count, Event_Earliest, Event_Latest,
           UTM_ZONE_NAD83_X = xCoordinate, UTM_ZONE_NAD83_Y = yCoordinate,
           Latitude = Lat, Longitude = Long, UTM_Zone = ZoneCode,
           Aspect = Aspect, Physiographic_Class = PhysiographySummary,
           StuntedWoodland = IsStuntedWoodland) |> unique()
  x <- x + 1
  setTxtProgressBar(pb, x)

  #---- Events ----
  events1 <- plot_evs |>
    mutate(Event_Date = format(as.Date(SampleDate, format = "%Y-%m-%d"), "%m/%d/%Y"),
           Event_Date_Txt = format(as.Date(SampleDate, format = "%Y-%m-%d"), "%Y%m%d"),
           Frame = ParkUnit,
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
                               Plot_Slope_Degree) |>
    arrange(Plot_Name, Event_Year)
  x <- x + 1
  setTxtProgressBar(pb, x)

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
                     TPlotSize = c(225, rep(400, 7)),
                     SapPlotNum = rep(3, 8),
                     SapPlotSize = rep(12.57, 8),
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
  x <- x + 1
  setTxtProgressBar(pb, x)

  #---- Cycles ----
  # Cycles by park
  ACAD_cycles <- data.frame(
    Cycle = c(1, 2, 3, 4, 5),
    Name = c("Cycle 1", "Cycle 2", "Cycle 3", "Cycle 4", "Latest Data"),
    YearStart = c(2006, 2010, 2014, 2018, 2021),
    YearEnd =   c(2009, 2013, 2017, 2021, 2024),
    PanelStart = c(1, 1, 1, 1, 4))

  #MABI MIMA SAGA SARA
  NHP13_cycles <- data.frame(
    Cycle = c(1, 2, 3, 4, 5),
    Name = c("Cycle 1", "Cycle 2", "Cycle 3", "Cycle 4", "Latest Data"),
    YearStart = c(2006, 2010, 2014, 2018, 2022),
    YearEnd =   c(2009, 2013, 2017, 2022, 2024),
    PanelStart = c(1, 1, 1, 1, 3))

  #MORR ROVA WEFA
  NHP24_cycles <- data.frame(
    Cycle = c(1, 2, 3, 4, 5),
    Name = c("Cycle 1", "Cycle 2", "Cycle 3", "Cycle 4", "Latest Data"),
    YearStart = c(2006, 2010, 2014, 2018, 2022),
    YearEnd =   c(2009, 2013, 2017, 2022, 2024),
    PanelStart = c(1, 1, 1, 1, 4))

  cycles <- rbind(
    data.frame(Unit_Code = rep("ACAD", 5), ACAD_cycles),
    data.frame(Unit_Code = rep("MABI", 5), NHP13_cycles),
    data.frame(Unit_Code = rep("MIMA", 5), NHP13_cycles),
    data.frame(Unit_Code = rep("MORR", 5), NHP24_cycles),
    data.frame(Unit_Code = rep("ROVA", 5), NHP24_cycles),
    data.frame(Unit_Code = rep("SAGA", 5), NHP13_cycles),
    data.frame(Unit_Code = rep("SARA", 5), NHP13_cycles),
    data.frame(Unit_Code = rep("WEFA", 5), NHP24_cycles))

  #---- CommonNames ----
  plants1 <- prepTaxa() |>
    mutate(Woody = ifelse(Tree + TreeShrub + Shrub + Vine > 0, TRUE, FALSE),
           Targeted_Herb = ifelse(DeerIndicatorHerb + InvasiveNETN > 0, TRUE, FALSE),
           Tree = ifelse(Tree + TreeShrub > 0, TRUE, FALSE), # inclusive for shrubs >10cm dbh
           Shrub = ifelse(Shrub + TreeShrub > 0, TRUE, FALSE)) |> # inclusive for shrubs >10cm dbh
    select(Latin_Name = ScientificName, NCRN_Common = CommonName, Common = CommonName,
           Family, Genus, Species, TSN, Woody, Herbaceous, Targeted_Herb, Tree, Shrub,
           Vine, Exotic, Graminoid, Fern_Ally = FernAlly) |>
    arrange(Latin_Name)

  plants_code <- VIEWS_NETN$Taxa_NETN |> select(TSN, TaxonCode)
  plants <- left_join(plants1, plants_code, by = "TSN")
  x <- x + 1
  setTxtProgressBar(pb, x)
  #---- Trees ----
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
           DecayClass = DecayClassCode) |>
    arrange(Plot_Name, Sample_Year, Tag)
  x <- x + 1
  setTxtProgressBar(pb, x)

  #---- Saplings ----
  saps1 <- joinMicroSaplings() |>
    mutate(#BA_cm2 = round(pi*((DBHcm/2)^2),4),
           Status = "Alive",
           Habit = "Tree",
           Browsed = NA_character_,
           Browsable = NA_character_,
           Date = format(as.Date(SampleDate, "%Y-%m-%d"), "%Y%m%d"),
           Tag = NA,
           TaxonCode = NA,
           Microplot_Number = ifelse(MicroplotCode == "UR", 45, ifelse(MicroplotCode == "B", 180, 315))) |>
    filter(!SQSaplingCode %in% c("NS", "NP"))

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
           Cycle = cycle, Panel = PanelCode, Frame = ParkUnit, Sample_Year = SampleYear,
           Date, Tag, Microplot_Number, TSN, TaxonCode, Latin_Name = ScientificName, StemsLive = Count,
           StemsDead, SumLiveBasalArea_cm2, SumDeadBasalArea_cm2,
           Equiv_Live_DBH_cm = DBHcm, Equiv_Dead_DBH_cm, Status, Habit, Browsed, Browsable) |>
    arrange(Plot_Name, Sample_Year, Microplot_Number)
  x <- x + 1
  setTxtProgressBar(pb, x)

  #---- Seedlings ----
  seeds1 <- joinMicroSeedlings() |>
    filter(!ScientificName %in% c("None present", "Not Sampled")) |>  #NPSForVeg doesn't take 0s
    mutate(Date = format(as.Date(SampleDate, "%Y-%m-%d"), "%Y%m%d"),
           Quadrat_Number = ifelse(MicroplotCode == "UR", 45, ifelse(MicroplotCode == "B", 180, 315))) |>
    pivot_longer(cols = c(Seedlings_15_30cm, Seedlings_30_100cm, Seedlings_100_150cm, Seedlings_Above_150cm),
                 names_to = "Class", values_to = "Count") |>
    select(Plot_Name, Unit_Code = ParkUnit, Subunit_Code = ParkSubUnit, Cycle = cycle,
           Panel = PanelCode, Frame = ParkUnit, Sample_Year = SampleYear, Date, Quadrat_Number,
           Latin_Name = ScientificName, TSN, Class, Count)

  seeds1$Height <- NA_real_
  seeds1$Height[seeds1$Class == "Seedlings_15_30cm"] <- 22.5
  seeds1$Height[seeds1$Class == "Seedlings_30_100cm"] <- 65.0
  seeds1$Height[seeds1$Class == "Seedlings_100_150cm"] <- 125.0
  seeds1$Height[seeds1$Class == "Seedlings_Above_150cm"] <- 200.0

  seeds_long <- seeds1[rep(1:nrow(seeds1), seeds1$Count),]

  seeds <- left_join(seeds_long, plots |> select(Plot_Name, Unit_Group), by = "Plot_Name") |>
    mutate(Browsable = NA_character_,
           Browsed = NA_character_) |>
    select(Plot_Name, Unit_Code, Unit_Group, Subunit_Code, Cycle, Panel, Frame, Sample_Year, Date,
           Quadrat_Number, Latin_Name, TSN, Height, Browsable, Browsed) |>
    arrange(Plot_Name, Sample_Year, Quadrat_Number)
  x <- x + 1
  setTxtProgressBar(pb, x)

  #---- Herbs ----
  herbs1 <- joinQuadSpecies() |>
    mutate(Date = format(as.Date(SampleDate, "%Y-%m-%d"), "%Y%m%d"))

  herbs2 <- left_join(herbs1, plots |> select(Plot_Name, Unit_Group), by = "Plot_Name") |>
    select(Plot_Name, Unit_Code = ParkUnit, Unit_Group, Subunit_Code = ParkSubUnit,
           Cycle = cycle, Panel = PanelCode, Frame = ParkUnit, Sample_Year = SampleYear, Date,
           TSN, Latin_Name = ScientificName,
           Pct_Cov_UC, Pct_Cov_UR, Pct_Cov_MR, Pct_Cov_BR, Pct_Cov_BC, Pct_Cov_BL, Pct_Cov_ML, Pct_Cov_UL,
           Exotic)

  herbs <- herbs2 |> pivot_longer(cols = Pct_Cov_UC:Pct_Cov_UL,
                                  names_to = "Quadrat_Number", values_to = "Percent_Cover") |>
    mutate(TaxonCode = NA_real_) |> filter(!is.na(Percent_Cover)) |> filter(Percent_Cover > 0) |>
    arrange(Plot_Name, Sample_Year, Latin_Name)

  herbs$Quadrat_Number <- substr(herbs$Quadrat_Number, 9, 10)
  x <- x + 1
  setTxtProgressBar(pb, x)

  #---- Vines ----
  vines1 <- joinTreeVineSpecies() |>
    mutate(Date = format(as.Date(SampleDate, "%Y-%m-%d"), "%Y%m%d"))
  vines1$Condition <- NA_character_
  vines1$Condition[vines1$VinePositionCode == "C"] <- "Vines in the crown"
  vines1$Condition[vines1$VinePositionCode == "B"] <- "Vines on the bole"
  vines1$Tag_Status = "Tree"

  vines2 <- left_join(vines1, events1 |> select(Plot_Name, SampleYear, IsQAQC, Unit_Group, Cycle),
                      by = c("Plot_Name", "SampleYear", "IsQAQC"))
  vines3 <- left_join(vines2, trees |> select(Plot_Name, Sample_Year, Tag, Status),
                      by = c("Plot_Name", "SampleYear" = "Sample_Year",
                             "TagCode" = "Tag"))

  vines <- vines3 |>
    select(Plot_Name, Unit_Code = ParkUnit,
           Unit_Group, Subunit_Code = ParkSubUnit,
           Cycle, Panel = PanelCode, Frame = ParkUnit, Sample_Year = SampleYear,
           Date, TSN, Latin_Name = ScientificName,
           Tag_Status, Host_Tag = TagCode, Host_Latin_Name = TreeScientificName, Host_Status = Status,
           Condition, Exotic) |>
    arrange(Plot_Name, Sample_Year, Host_Tag)
  x <- x + 1
  setTxtProgressBar(pb, x)

  #---- CWD ----
  cwd1 <- joinCWDData()

  cwd <- left_join(cwd1, plots |> select(Plot_Name, Unit_Group, Subunit_Code, Panel), by = "Plot_Name") |>
         mutate(Date = format(as.Date(SampleDate, "%Y-%m-%d"), "%Y%m%d")) |>
         select(Plot_Name, Unit_Code = ParkUnit, Unit_Group, Subunit_Code, Cycle = cycle,
                Panel, Frame = ParkUnit, Sample_Year = SampleYear, Date, TSN, Latin_Name = ScientificName,
                CWD_Vol, DecayClass = DecayClassCode)
  x <- x + 1
  setTxtProgressBar(pb, x)

  #---- Export Process -----
  csv_list <- list(plots, events, meta, cycles, plants, trees, saplings, seeds, vines, herbs, cwd)
  csv_names <- c("Plots", "Events", "MetaData", "Cycles", "CommonNames",
                 "Trees", "Saplings", "Seedlings", "Vines", "Herbs", "CWD")
  csv_list <- setNames(csv_list, csv_names)

  # # Create Metadata for files
  #
  # defs <- lapply(seq_along(csv_list), function(x){
  #   col_names <- names(csv_list[[csv_names[[x]]]])
  #   rbind(data.frame(
  #     flatfile = names(csv_list[x]),
  #     columns = col_names))
  # })
  #
  # defs2 <- do.call(rbind, defs)
  # length(unique(defs2$columns))
  # write.csv(defs2, paste0(path, "NPSForVeg_Defs.csv"), row.names = F)

  if(keep == TRUE){list2env(csv_list, envir = .GlobalEnv)}

  if(export == TRUE){
    if(zip == FALSE){
      invisible(lapply(seq_along(csv_names), function(x){
        setTxtProgressBar(pb, x + 10)
        csv_name <- csv_names[[x]]
        write.csv(csv_list[[x]], paste0(path, csv_name, ".csv"), row.names = F)
      }))
    } else if(zip == TRUE){
      dir.create(tmp <- tempfile())

      invisible(lapply(seq_along(csv_names), function(x){
        setTxtProgressBar(pb, x + 10)
        csv_name <- csv_names[[x]]
        write.csv(csv_list[[x]], paste0(tmp, "\\", csv_name, ".csv"),
                  row.names = FALSE)}))

      file_list <- list.files(tmp)

      zip::zipr(zipfile = paste0(pathn, "NPSForVeg_NETN_", format(Sys.Date(), "%Y%m%d"), ".zip"),
                root = tmp,
                files = file_list)
    }
  }
  close(pb)

  }
