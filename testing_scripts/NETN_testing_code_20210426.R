library(forestNETN)
library(tidyverse)

#----- Testing the import/export functions -----
#importData(instance = 'local', server = "localhost", new_env = T) # release 1.0.22 on 4/22
importData(instance = 'server', server = "INP2300VTSQL16\\IRMADEV1", name = "NETN_Forest_Migration", new_env = T) #
path = "C:/Forest_Health/exports/NETN"
#exportCSV(path, zip = TRUE)

importCSV(path = path, zip_name = "NETN_Forest_20210503.zip") #release 1.0.22 on 4/26 ##with IsGerminant added back by hand

# Function arguments
park = 'all'
from = 2006
to = 2019
QAQC = TRUE
locType = "all"
eventType = 'all'
panels = 1:4

arglist <- list(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                locType = locType, eventType = eventType)

compev_arglist <- list(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                       locType = locType)

# Checking data function
check_data <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", col1, col2)]}
  )) %>% bind_rows()
}

# For some reason R crashes if I do this after the import from old database
library(RODBC)
db <- RODBC::odbcConnect("NETNFVM") #
disturb<-sqlFetch(db,"tbl_Disturbances")
disttlu<-sqlFetch(db,"tlu_Disturbance_Codes")
disttlutc<-sqlFetch(db,"tlu_Disturbance_Threshhold_Codes")
odbcClose(db)


# import old database for comparisons
forestNETNarch::importData(type='file',
  path='D:/NETN/Monitoring_Projects/Forest_Health/Database/2021_Forest_Database/Forest_Backend_NETN_20210427_Migration.mdb')


#----- Testing joinLocEvent and migration -----
plotevs_old <- do.call(forestNETNarch::joinLocEvent, c(arglist, output = 'verbose'))
plotevs_new <- do.call(joinLocEvent, arglist)
names(plotevs_old)
names(plotevs_new)
nrow(plotevs_old) #1280
nrow(plotevs_new) #1280

pe_merge <- full_join(plotevs_new, plotevs_old, by = c("EventLegacyID" = "Event_ID", "Plot_Name" = "Plot_Name"))

check_data(pe_merge, "ParkSubUnit", "Unit_ID") #0
check_data(pe_merge,"xCoordinate", "X_Coord")#0
check_data(pe_merge,"yCoordinate", "Y_Coord")#0
#check_data(pe_merge,"StartDate", "Start_Date") # they're diff. format, so all show as different
check_data(pe_merge,"PanelCode", "Panel")#0
check_data(pe_merge,"Event_QAQC", "IsQAQC")#0
#test <- check_data(pe_merge,"ZoneCode", "UTM_Zone") # diff b/c one has N.
table(pe_merge$ZoneCode, pe_merge$UTM_Zone)#ok
check_data(pe_merge,"Orientation.x", "Orientation.y")#0
check_data(pe_merge,"cycle.x", "cycle.y")#0
check_data(pe_merge,"PlotTypeCode", "Loc_Type")#0
check_data(pe_merge,"PlotLegacyID", "Location_ID")#0
check_data(pe_merge,"Aspect.x", "Aspect.y")#0
check_data(pe_merge,"PhysiographyCode", "Physiographic_Class")#0

plot_check <- unique(pe_merge[, c("ParkUnit", "Plot_Name")])
table(plot_check$ParkUnit) # That's the correct # of plots/park
#ACAD MABI MIMA MORR ROVA SAGA SARA WEFA
#176   24   20   29   40   21   32   10
dir_dif <- check_data(pe_merge, "Directions.x", "Directions.y")
# No issues remaining

#----- Stand Views -----
stand_new <- do.call(joinStandData, arglist)
stand_old <- do.call(forestNETNarch::joinStandData, arglist)
stand_old2 <- merge(stand_old, stand[,c("Event_ID", "Deer_Browse_Line_pre09_ID")],
                    by = "Event_ID", all.x = T, all.y = T)

stand_merge <- full_join(stand_new, stand_old2, by = c("Plot_Name" = "Plot_Name", "IsQAQC" = "Event_QAQC",
                                                      "StartYear" = "Year"))
check_data(stand_merge,"Panel", "PanelCode") # Only ACAD-029-2010
check_data(stand_merge, "Stand_Structure.x", "Stand_Structure.y") #0
check_data(stand_merge, "Stand_Structure_Code", "Stand_Structure_ID") #0
check_data(stand_merge, "Pct_Crown_Closure.x", "Pct_Crown_Closure.y") #0
check_data(stand_merge, "Pct_Understory_Low.x", "Pct_Understory_Low.y") #0
check_data(stand_merge, "Pct_Understory_Mid.x", "Pct_Understory_Mid.y") #0
check_data(stand_merge, "Pct_Understory_High.x", "Pct_Understory_High.y") #0
check_data(stand_merge, "Pct_Bare_Soil", "Pct_Bare_Soil_Cover") #0
check_data(stand_merge, "Pct_Rock", "Pct_Rock_Cover")#0
check_data(stand_merge, "Pct_Lichen", "Pct_Lichen_Cover")#0
check_data(stand_merge, "Pct_Bryophyte", "Pct_Bryophyte_Cover")#0
check_data(stand_merge, "Pct_Water", "Pct_Surface_Water_Cover")#0
check_data(stand_merge, "Pct_Bare_Soil", "Pct_Bare_Soil_Cover")#0
check_data(stand_merge, "Pct_Trampled", "Pct_Trampled_Cover")#0
check_data(stand_merge, "IsStuntedWoodland", "Stunted_Woodland") # Only ACAD-029-2010
check_data(stand_merge, "Earthworms.x", "Earthworms.y")#0
check_data(stand_merge, "Microtopography", "Microtopography_ID")#0

check_data(stand_merge, "Deer_Browse_Index", "Deer_Browse_Line_ID")
table(stand_merge$Deer_Browse_Label, stand_merge$StartYear, useNA = 'always') # All are 5s that are recorded pre 2010 as DBL pres
table(stand_merge$Deer_Browse_Label, stand_merge$Deer_Browse_Line_pre09_ID) # Either NC or Very High. OK
table(stand_merge$Deer_Browse_Label, stand_merge$Deer_Browse_Line_ID) # Labels match codes. OK

stand_merge %>% mutate(slp_dif = abs(PlotSlope - Plot_Slope_Deg)) %>%
                             select(Plot_Name, StartYear, IsQAQC, EventID, PlotSlope, Plot_Slope_Deg, slp_dif) %>%
                             filter(slp_dif > 0.1) # All QAQC events where slopes were entered. Don't care.
#check_data(stand_merge, "PlotSlope", "Plot_Slope_Deg") # Lots due to rounding, but fine

stand_merge %>% mutate(slp_dif = abs(PlotSlope - Plot_Slope_Deg)) %>%
  select(Plot_Name, StartYear, IsQAQC, EventID, PlotSlope, Plot_Slope_Deg, slp_dif) %>%
  filter(slp_dif > 0.5) # Only differences is a QAQC event, which we don't care about

#----- Stand Disturbance -----
stdist_new <- do.call(joinStandDisturbance, arglist)

#--- Tends to crash if run after old DB import, so moved code to beginning
# library(RODBC)
# db <- RODBC::odbcConnect("NETNFVM") #
# disturb<-sqlFetch(db,"tbl_Disturbances")
# disttlu<-sqlFetch(db,"tlu_Disturbance_Codes")
# disttlutc<-sqlFetch(db,"tlu_Disturbance_Threshhold_Codes")
# odbcClose(db)

st_dist_o <- merge(plotevs_old[, c("Plot_Name", "Event_ID", "Event_QAQC", "Year")],
                   disturb[, c(2:6)], by = "Event_ID", all.x = T, all.y = T)

dist_merge <- merge(stdist_new, st_dist_o,
                    by.x = c("Plot_Name", "StartYear", "IsQAQC"),#, "Disturbance.Code"),
                    by.y = c("Plot_Name", "Year", "Event_QAQC"),#, "Disturbance_Code"),
                    all.x = T, all.y = T)

dist_merge[which(!complete.cases(dist_merge$DisturbanceCode)),] # SARA.915. OK
check_data(dist_merge, "Disturbance_Threshold", "ThresholdCode") %>% arrange(Plot_Name) #18 All dups sorted wrong or SARA-915 (NA)
check_data(dist_merge, "Disturbance_Notes", "DisturbanceNote") #11 All dups sorted wrong, or SARA-915 (NA)
check_data(dist_merge, "DisturbanceCode", "Disturbance_Code") %>% arrange(Plot_Name) # All dups that sorted wrong or SARA-915 (NA)
#+++++ No remaining disturbance issues

#----- Tree Height -----
#ht_dif <-
  stand_merge %>% mutate(cod_dif = abs(Avg_Height_Codom - Avg_Codom_HT),
                         int_dif = abs(Avg_Height_Inter - Avg_Inter_HT)) %>%
                  select(Plot_Name, StartYear, IsQAQC, Avg_Height_Codom, Avg_Codom_HT,
                                 cod_dif, Avg_Height_Inter, Avg_Inter_HT, int_dif) %>%
                  filter(cod_dif > 0.5 | int_dif > 0.5)
#0 Tree height issues resolved

tree_ht_old <- stand %>% select(Event_ID, Stand_Structure_ID, Stunted_Woodland, Tree_1_Number_Codom:Height_Tree_3_Inter) %>%
  filter(Stand_Structure_ID != 5) %>% filter(Stunted_Woodland == 0)

tree_ht2 <- merge(plotevs_old[,c("Event_ID", "Plot_Name", "Year", "Event_QAQC")],
                  tree_ht_old, by = "Event_ID", all.x = FALSE, all.y = TRUE) %>% filter(!is.na(Plot_Name))

tree_ht_w1 <- tree_ht2 %>% select(Event_ID:Tree_3_Number_Inter) %>%
  pivot_longer(cols = c(Tree_1_Number_Codom:Tree_3_Number_Inter),
               names_to = "Samp",
               values_to = "Tree_Number") %>%
  mutate(Samp_Num = case_when(str_detect(Samp, "_1_") ~ 1L,
                              str_detect(Samp, "_2_") ~ 2L,
                              str_detect(Samp, "_3_") ~ 3L),
         Crown = ifelse(str_detect(Samp, "Codom"), "Codom", "Inter")
  )

tree_ht_w2 <- tree_ht2 %>% select(Event_ID:Event_QAQC, Height_Tree_1_Codom:Height_Tree_3_Inter) %>%
  pivot_longer(cols = c(Height_Tree_1_Codom:Height_Tree_3_Inter),
               names_to = "Samp_ht",
               values_to = "Height_m") %>%
  mutate(Samp_Num = case_when(str_detect(Samp_ht, "_1_") ~ 1L,
                              str_detect(Samp_ht, "_2_") ~ 2L,
                              str_detect(Samp_ht, "_3_") ~ 3L),
         Crown = ifelse(str_detect(Samp_ht, "Codom"), "Codom", "Inter")
  )

tree_ht3 <- merge(tree_ht_w1, tree_ht_w2, by = c("Event_ID", "Plot_Name", "Year", "Event_QAQC", "Samp_Num", "Crown"),
                  all.x = T, all.y = T) %>% select(-Samp, -Samp_ht) %>% filter(!is.na(Height_m))

tree_ht_old_sum <- tree_ht3 %>% group_by(Plot_Name, Year, Event_QAQC, Crown) %>%
                                summarize(total_ht_old = sum(Height_m, na.rm =T),
                                          num_trees_old = sum(!is.na(Height_m)),
                                          .groups = 'drop')

tree_ht_vw <- get("COMN_StandTreeHeights", envir = VIEWS_NETN)#[, -c(18:24, 27)]
tree_ht_vw$Plot_Name <- paste(tree_ht_vw$ParkUnit, sprintf("%03d", tree_ht_vw$PlotCode), sep = "-")
tree_ht_vw$Event_QAQC <- ifelse(tree_ht_vw$IsQAQC == 0, FALSE, TRUE)

tree_ht_vw_sum <- tree_ht_vw %>% group_by(Plot_Name, StartYear, Event_QAQC, EventID, CrownClassLabel) %>%
  summarize(total_ht_new = sum(Height),
            num_trees_new = sum(!is.na(Height)),
            .groups = 'drop') %>%
  mutate(Crown = ifelse(CrownClassLabel == "Intermediate", "Inter", "Codom"))

tree_height_comps <- merge(tree_ht_vw_sum,
                           tree_ht_old_sum,
                           by.x = c("Plot_Name", "StartYear", "Event_QAQC", "Crown"),
                           by.y = c("Plot_Name", "Year", "Event_QAQC", "Crown"),
                           all.x = T, all.y = T) %>%
  mutate(ht_diff = abs(total_ht_new - total_ht_old),
            n_tree_diff = num_trees_new - num_trees_old)

tree_height_comps %>% filter(ht_diff > 0.1) # Only 4 plots with ht diff > 0.1. All appear to be rounding related. OK.
tree_height_comps %>% filter(n_tree_diff == -1) # No missing tree heights
#+++++ No remaining stand height issues +++++

#----- CWD -----
cwd_old <- do.call(forestNETNarch::joinCWDData, arglist) %>% mutate(ScientificName = ifelse(Latin_Name == "No species recorded",
                                                                                            "None present", Latin_Name))
cwd_new <- do.call(joinCWDData, arglist)

cwd_merge <- merge(cwd_new, cwd_old, by.x = c("Plot_Name", "StartYear", "IsQAQC", "ScientificName", "DecayClassCode"),
                                     by.y = c("Plot_Name", "Year", "Event_QAQC", "ScientificName", "Decay_Class_ID"),
                   all = TRUE)

cwd_merge %>% mutate(cwd_dif = abs(CWD_Vol.x - CWD_Vol.y)) %>% filter(cwd_dif > 0.05 & IsQAQC == FALSE) %>%
              select(Plot_Name:ScientificName, DecayClassCode, CWD_Vol.x, CWD_Vol.y, cwd_dif)
#9 All are small differences due to rounding
#+++++ No remaining CWD issues +++++

#----- Tree Data
tree_old <- do.call(forestNETNarch::joinTreeData, c(arglist, list(speciesType = 'all', status = 'all'))) %>%
            mutate(TagCode = as.numeric(Tree_Number_NETN))
tree_new <- do.call(joinTreeData, c(arglist, list(speciesType = 'all', status = 'all')))

table(tree_new$HWACode, tree_new$ScientificName, useNA = 'always') # Only values for TSUCAN. Good!
non_tsucan <- tree_new %>% filter(ScientificName != "Tsuga canadensis")
table(non_tsucan$HWACode, non_tsucan$StartYear, useNA = 'always') # All NA. Good!

table(tree_new$BBDCode, tree_new$ScientificName, useNA = 'always') # Only values for FAGGRA. Good!
non_faggra <- tree_new %>% filter(ScientificName != "Fagus grandifolia")
table(non_faggra$BBDCode, non_faggra$StartYear, useNA = 'always')# All NA. Good!

tree_merge <- full_join(tree_new, tree_old, by = c("Plot_Name" = "Plot_Name",
                                                     "StartYear" = "Year",
                                                     "IsQAQC" = "Event_QAQC",
                                                     "TagCode" = "TagCode"),
                        suffix = c("_new", "_old")
                        )

check_trees <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", "TagCode", "IsQAQC", "ScientificName",
              "Status_ID", "TreeStatusCode",col1, col2)]}
  )) %>% bind_rows()
}

check_trees(tree_merge, "TSN_new", "TSN_old") #SARA-015-2012 trees that weren't located or EX after plot reset. OK.
#check_trees(tree_merge, "Distance_new", "Distance_old") # too many rounding differences. Need dif. check.
check_trees(tree_merge, "Azimuth_new", "Azimuth_old") #SARA-015-2012 trees that were NL or EX after plot reset. OK.
check_trees(tree_merge, "Fork_new", "Fork_old") #0

tree_merge %>% mutate(dist_diff = abs(Distance_new - Distance_old)) %>%
  filter(dist_diff > 0.1)  #0

tree_merge %>% mutate(dbh_diff = abs(DBH - DBHcm)) %>%
  filter(dbh_diff > 0) %>%
  select(Plot_Name, StartYear, IsQAQC, TagCode, ScientificName, DBHcm, DBH, dbh_diff)
# Only big difference is bad data in ACAD-019-2006 Tag 30 in old db not migrating to new.
# Remaining differences are cleaner rounding in new db. Good.

check_trees(tree_merge, "TreeStatusCode", "Status_ID")
# differences are ACAD-029-2010 getting PMs correctly
# bad ACAD-019-2006 Tag 30 not migrating.
# dropped SARA-015-2012 trees that were NL or EX after plot reset. All good.
crown_check <- check_trees(tree_merge, "Crown_Class_ID", "CrownClassCode")
table(crown_check$Status_ID, crown_check$Crown_Class_ID, useNA = 'always')
# Differences are because dead and EX trees were correctly scrubbed of their crown class.
tree_simp <- tree_merge %>% select(Plot_Name, StartYear, IsQAQC, TagCode, ScientificName,
                                   TreeStatusCode, Status_ID, CrownClassCode, Crown_Class_ID)
table(tree_simp$CrownClassCode, tree_simp$Crown_Class_ID, useNA = 'always')
# Only rogue tree (4/3) in table is ACAD-019 Tag 30 not migrating correctly. OK.

check_trees(tree_merge, "Decay_Class_ID", "DecayClassCode")
# No issues. Decays that are diff are DF and AS, which correctly shouldn't have a decay class

table(tree_merge$IsDBHVerified, tree_merge$DBH_Verified, useNA = 'always') # Align with diagonal. OK

table(tree_merge$Pct_Tot_Foliage_Cond, tree_merge$Total_Foliage_Condition, tree_merge$StartYear, useNA = 'always')
# 4/26: 7 records in 2011 and 1 in 2012 that have a 0 % Foliage still

tree_simp <- tree_merge %>% select(Plot_Name, StartYear, IsQAQC, TagCode, Pct_Tot_Foliage_Cond, Total_Foliage_Condition)
fol_view <- get("COMN_TreesFoliageCond", envir = VIEWS_NETN) #%>% select(ParkUnit, PlotCode, IsQAQC, StartYear, TagCode,
                                                              #        FoliageConditionLabel, TotalFoliageConditionLabel)
names(tree_view)#
table(tree_view$FoliageConditionLabel, tree_view$TotalFoliageConditionLabel, useNA = 'always')
# There's actually a dead tree with Leaf Loss: ACAD-039-2007 Tag 9. I deleted it in 20210503 migration

tree_merge %>% filter(Pct_Tot_Foliage_Cond == 0) %>% arrange(Plot_Name, StartYear) %>%
      select(Plot_Name, StartYear, IsQAQC, TagCode, ScientificName, TreeStatusCode, CrownClassCode,
             Pct_Tot_Foliage_Cond, Total_Foliage_Condition)
      # These trees appear to have had foliage conditions recorded, but the Total Foliage was recorded as 0%.
      # 6 were fixed b/c only 1 condition was reported, so applied its percent. 2 were deleted and should migrate as PM.
# 5/3 0 records. Issues resolved
write.csv(fol_check, "./testing_scripts/tot_fol_still_0.csv", row.names = F)
# +++ Tree data finished checking.

#----- Tree Foliage Conditions -----
fol_vw <- VIEWS_NETN$COMN_TreesFoliageCond

table(fol_vw$TotalFoliageConditionCode, fol_vw$StartYear, useNA = 'always')
# 8 records in 2011 and 1 in 2012 where Total Foliage was recorded as 0, but there were conditions recorded.
# Already fixed in previous section. Should migrate 2 PM and the rest with non-0 values in next migration.
# 5/3 Issue resolved

table(fol_vw$PercentLeafAreaLabel, fol_vw$StartYear, useNA = 'always') # LA NC for 2006-2015; 14 PM from 2016 & 2017 correct
nrow(fol_vw %>% filter(PercentLeafAreaLabel == "Not Collected" )) #18556 additional rows for NC
table(fol_vw$PercentLeafAreaLabel, fol_vw$FoliageConditionCode, useNA = 'always') # NA correctly applied to L, NO, S, W
table(fol_vw$TotalFoliageConditionLabel, fol_vw$FoliageConditionCode, useNA = 'always')
  # 4/22 14 0 TotFol when foliage conditions exist. Not clear why this is happening. NO to NotApp is correct.
  # 4/26 9 TotFol with 0% and foliage conditions pres. All but 3 were fixed b/c only 1 condition recorded.
      # Rest should be PM in next migration.
  # 5/3 No 0s remaining
pm_fol <- fol_vw %>% filter(TotalFoliageConditionCode == "PM")
pm_fol$Plot_Name <- paste(pm_fol$ParkUnit, sprintf("%03d", pm_fol$PlotCode), sep = "-")
write.csv(pm_fol, "./testing_scripts/PM_totfoliage.csv")

#no_folcond_with_totfol_0p <-
  fol_vw %>% filter(FoliageConditionCode == "NO" & TotalFoliageConditionCode %in% c(1:4)) %>%
                             select(ParkUnit, PlotCode, StartYear, IsQAQC, TagCode, ScientificName, TreeStatusCode,
                                    FoliageConditionCode, TotalFoliageConditionCode)
# 287 missing foliage conditions were converted to NO instead of PM (see no_folcond_with_totfol_0p)
# Instead totfol should be kept as is and foliage conditions should come in as PM.
# Remains an issue. See line 192 in legacy migration data questions spreadsheet.
# No issues remain

table(fol_vw$PercentLeavesLabel, fol_vw$FoliageConditionCode, useNA = 'always')# 1 L with 0 and 4 N with 0. NO to Not Applicable correct.
# no issues remain
fol_vw %>% filter(PercentLeavesLabel == "0%" & FoliageConditionCode %in% c("L", "N")) %>% arrange(ParkUnit, PlotCode, StartYear, TagCode) %>%
  select(ParkUnit, PlotCode, StartYear, IsQAQC, TagCode, ScientificName, TreeStatusCode,
         PercentLeavesLabel, FoliageConditionCode, TotalFoliageConditionCode) #0

table(fol_vw$FoliageConditionCode, fol_vw$PercentLeavesLabel)
table(fol_vw$TotalFoliageConditionLabel, fol_vw$PercentLeavesLabel, fol_vw$StartYear)
# 2012 1 0%; 2011 7 0%. Should be fixed in next migration (see previous section)
# 5/3 no issues remaining

fol_vw %>% filter(TotalFoliageConditionLabel == "0%") %>%
  select(ParkUnit, PlotCode, StartYear, IsQAQC, TagCode, ScientificName, TreeStatusCode,
         FoliageConditionCode, TotalFoliageConditionLabel) %>%
  arrange(ParkUnit, PlotCode, StartYear, TagCode)
# 9 records with 0% TotFolConLabel that should be > 0 b/c conditions recorded. Fixed all but 3, which should migrate as PM.
# 5/3 0 issues remain

compev_arglist <- list(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                       locType = locType)

fol_new <- do.call(joinTreeFoliageCond, compev_arglist)
fol_old1 <- xrfolcond %>% mutate(Cond = case_when(Foliage_Condition_ID == 1 ~ "C",
                                                 Foliage_Condition_ID == 2 ~ "N",
                                                 Foliage_Condition_ID == 3 ~ "H",
                                                 Foliage_Condition_ID == 4 ~ "S",
                                                 Foliage_Condition_ID == 5 ~ "W",
                                                 Foliage_Condition_ID == 6 ~ "L",
                                                 Foliage_Condition_ID == 7 ~ "O",
                                                 Foliage_Condition_ID == 8 ~ "NO",
                                                 TRUE ~ NA_character_),
                                Pct_Leaves_Aff = case_when(Foliage_Condition_Percent == 0 ~ 0,
                                                           Foliage_Condition_Percent == 1 ~ 5.5,
                                                           Foliage_Condition_Percent == 2 ~ 30,
                                                           Foliage_Condition_Percent == 3 ~ 70,
                                                           Foliage_Condition_Percent == 4 ~ 95,
                                                           TRUE ~ NA_real_),
                                Pct_Leaf_Area = case_when(Leaf_Area_Percent == 0 ~ 0,
                                                          Leaf_Area_Percent == 1 ~ 5.5,
                                                          Leaf_Area_Percent == 2 ~ 30,
                                                          Leaf_Area_Percent == 3 ~ 70,
                                                          Leaf_Area_Percent == 4 ~ 95,
                                                          TRUE ~ NA_real_)
                                )

tree_evs_old <- left_join(plotevs_old, trees %>% select(Tree_ID:Tree_Notes),
                          by = intersect(names(plotevs_old), names(trees %>% select(Tree_ID:Tree_Notes)))) %>%
                left_join(., treedata %>% select(Tree_Data_ID:Notes),
                          by = intersect(names(.), names(treedata %>% select(Tree_Data_ID:Notes)))) %>%
                select(Plot_Name, Year, Event_QAQC, Event_ID, Tree_ID, Tree_Number_NETN, Total_Foliage_Condition,
                       Tree_Data_ID, Status_ID) %>% filter(Status_ID %in% c("1", "AB", "AF", "AL", "AM", "AS",
                                                                            "RB", "RF", "RL", "RS"))

fol_old <- left_join(tree_evs_old, fol_old1, by = "Tree_Data_ID") %>%
           select(Plot_Name, Year, Event_QAQC, Tree_Number_NETN, Status_ID, Cond, Pct_Leaves_Aff, Pct_Leaf_Area) %>%
           arrange(Plot_Name, Year, Event_QAQC, Tree_Number_NETN) %>%
           pivot_wider(names_from = Cond,
                       values_from = c(Pct_Leaves_Aff, Pct_Leaf_Area),
                       values_fill = NA_real_) %>%
           mutate(TagCode = as.numeric(Tree_Number_NETN)) %>%
           select(-Pct_Leaves_Aff_NA, -Pct_Leaf_Area_NA, -Pct_Leaf_Area_W, -Pct_Leaf_Area_O, -Pct_Leaf_Area_S)

nrow(fol_old) #23188
nrow(fol_new) #23189

fol_merge <- full_join(fol_new, fol_old,
                   by = c("Plot_Name" = "Plot_Name", "StartYear" = "Year", "IsQAQC" = "Event_QAQC", "TagCode" = "TagCode"),
                   suffix = c("_new", "_old"))

head(fol_merge)
check_conds <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", "TagCode", "Status_ID", "IsQAQC", col1, col2)]}
  )) %>% bind_rows()
}

names(fol_merge)
# 0s in _new because they were filled in function
lvs_C <- check_conds(fol_merge, "Pct_Leaves_Aff_C_new", "Pct_Leaves_Aff_C_old") # all diffs are NAs converted to 0s. I think they should still be NA.
lvs_H <- check_conds(fol_merge, "Pct_Leaves_Aff_H_new", "Pct_Leaves_Aff_H_old") # all diffs are NAs converted to 0s. I think they should still be NA.
lvs_L <- check_conds(fol_merge, "Pct_Leaves_Aff_L_new", "Pct_Leaves_Aff_L_old") # all diffs are NAs converted to 0s. I think they should still be NA.
lvs_N <- check_conds(fol_merge, "Pct_Leaves_Aff_N_new", "Pct_Leaves_Aff_N_old") # all diffs are NAs converted to 0s. I think they should still be NA.
lvs_S <- check_conds(fol_merge, "Pct_Leaves_Aff_S_new", "Pct_Leaves_Aff_S_old") # all diffs are NAs converted to 0s. I think they should still be NA.
lvs_W <- check_conds(fol_merge, "Pct_Leaves_Aff_W_new", "Pct_Leaves_Aff_W_old") # all diffs are NAs converted to 0s. I think they should still be NA.
lvs_O <- check_conds(fol_merge, "Pct_Leaves_Aff_O_new", "Pct_Leaves_Aff_O_old") # all diffs are NAs converted to 0s. I think they should still be NA.

la_C <- check_conds(fol_merge, "Pct_Leaf_Area_C_new", "Pct_Leaf_Area_C_old") # all diffs are NAs converted to 0s. I think they should still be NA.
la_H <- check_conds(fol_merge, "Pct_Leaf_Area_H_new", "Pct_Leaf_Area_H_old") # all diffs are NAs converted to 0s. I think they should still be NA.
la_N <- check_conds(fol_merge, "Pct_Leaf_Area_N_new", "Pct_Leaf_Area_N_old") # all diffs are NAs converted to 0s. I think they should still be NA.

# +++++ The remaining issue is when Total_Foliage_Condition > 0 but no foliage conditions.
  # Foliage conditions are getting converted to NO instead of PM in this case.
  # 5/3 no issues remain

#----- Tree Conditions
# NETN old db condition counts
#H	    AD	BBD	BWA	CAVL CAVS	 CW	   DBT	EAB	 EB	  EHS 	G	GM	HWA	ID	NO	  OTH	RPS	SB	VIN	VOB
#15682	927	569	162	362	 349	3880	2412	2	   932	163	193	1	 206	30	1854	136	2	  2	  422	35
table(VIEWS_NETN$COMN_TreesConditions$TreeConditionCode, useNA = 'always')
# Not quite the same. New DB has fewer when they differ
# H      AD   BBD   BWA  CAVL  CAVS    CW   DBT   EAB    EB   EHS     G    GM   HWA    ID    NO   OTH   RPS    SB  VINE  <NA>
# 15561 925   569   162   362   349  3869  2402     2   929   163   193     1   206    30  1851   131     2     2   455  3149

con_vw <- VIEWS_NETN$COMN_TreesConditions
table(VIEWS_NETN$COMN_TreesConditions$TreeConditionCode,
      VIEWS_NETN$COMN_TreesConditions$StartYear, useNA = 'always')
table(con_vw$TreeStatusCode)
live <- c("1", "AB", "AF", "AL", "AM", "AS", "RB", "RF", "RL", "RS")
dead <- c("2", "DB", "DL", "DM", "DS")
con_vw$status_simp <- ifelse(con_vw$TreeStatusCode %in% live, "live",
                             ifelse(con_vw$TreeStatusCode %in% dead, "dead",
                                    "inactive"))
table(con_vw$status_simp, con_vw$TreeConditionCode, con_vw$StartYear, useNA = "always")
# Still missing NC and PMs. Otherwise RPS issue resolved
# 5/3 resolved

dead_trees_with_conds <-
  con_vw %>%
  filter(TreeStatusCode %in% c("2", "DB", "DL", "DM", "DS")) #%>%
  #filter(!TreeConditionCode %in% c("NO", "CAVS", "CAVL")) #%>%
                         #filter(!is.na(TreeConditionCode)) # 0 records

table(dead_trees_with_conds$TreeConditionCode, dead_trees_with_conds$StartYear, useNA = 'always')
# Still need NC for NAs <=2011 and PM for >2011 (1 record in 2013)

cond_new <- do.call(joinTreeConditions, c(compev_arglist, list(status = 'active')))

active <- c("1", "AB" ,"AF", "AL", "AS", "AM", "DB", "DL", "DM", "DS", "RB", "RF", "RL", "RS")

tree_evs_old <- left_join(plotevs_old, trees %>% select(Tree_ID:Tree_Notes),
                          by = intersect(names(plotevs_old), names(trees %>% select(Tree_ID:Tree_Notes)))) %>%
  left_join(., treedata %>% select(Tree_Data_ID:Notes),
            by = intersect(names(.), names(treedata %>% select(Tree_Data_ID:Notes)))) %>%
  select(Plot_Name, Year, Event_QAQC, Event_ID, Tree_ID, Tree_Number_NETN, Total_Foliage_Condition,
         Tree_Data_ID, Status_ID) %>% filter(Status_ID %in% active)

table(tree_evs_old$Status_ID)

tlucond <- read.csv("./testing_scripts/tlu_Tree_Conditions.csv")
cond_old1 <- left_join(xrtreecond %>% select(Tree_Data_ID, Tree_Condition_ID),
                       tlucond %>% select(Tree_Condition_ID, Code), by = c("Tree_Condition_ID")) %>%
             right_join(tree_evs_old, ., by = intersect(names(tree_evs_old), names(.))) %>%
             select(Plot_Name, Year, Event_QAQC, Tree_Number_NETN, Status_ID, Code) %>%
             filter(!is.na(Code)) %>% filter(!is.na(Plot_Name)) %>%
             arrange(Plot_Name, Year, Tree_Number_NETN) %>% # drop trees without conditions
             mutate(pres = 1, TagCode = as.numeric(Tree_Number_NETN)) %>% select(-Tree_Number_NETN)

cond_old <- cond_old1 %>% mutate(pres = 1) %>% unique() %>%
  pivot_wider(names_from = Code, values_from = pres, values_fill = 0)

cond_merge <- full_join(cond_new, cond_old, by = c("Plot_Name" = "Plot_Name",
                                                   "StartYear" = "Year",
                                                   "IsQAQC" = "Event_QAQC",
                                                   "TagCode" = "TagCode"),
                        suffix = c("_new", "_old"))
cond_merge[,18:67][is.na(cond_merge[,18:67])] <- 0 # just so rowwise checking works. There were 0s in new and NAs in old
names(cond_merge)
check_conds(cond_merge, "H_new", "H_old") # 0
check_conds(cond_merge, "NO_new", "NO_old") # 0
check_conds(cond_merge, "AD_new", "AD_old") #0
# no ALB, BC, SOD so no check for it
check_conds(cond_merge, "BBD_new", "BBD_old") #0
check_conds(cond_merge, "BWA_new", "BWA_old") #0
check_conds(cond_merge, "CAVL_new", "CAVL_old") #0
check_conds(cond_merge, "CAVS_new", "CAVS_old") #0
check_conds(cond_merge, "CW_new", "CW_old") # 0
check_conds(cond_merge, "DBT_new", "DBT_old") #0
check_conds(cond_merge, "DOG_new", "DOG_old") #0
check_conds(cond_merge, "EAB_new", "EAB_old") #0
check_conds(cond_merge, "EB_new", "EB_old") #0
check_conds(cond_merge, "EHS_new", "EHS_old") #0
check_conds(cond_merge, "G_new", "G_old") #0
check_conds(cond_merge, "GM_new", "GM_old") #0
check_conds(cond_merge, "HWA_new", "HWA_old") #0
check_conds(cond_merge, "ID_new", "ID_old") #0
check_conds(cond_merge, "OTH_new", "OTH_old") #0
check_conds(cond_merge, "RPS_new", "RPS_old") #0
check_conds(cond_merge, "SB_new", "SB_old") #0

#++++++ Remaining issue: NC and PM for dead trees are currently NA.
#++++++ Missing conditions for live trees are NA instead of PM.
# 5/3 both issues resolved

#------ Vines -----
vines_new <- do.call(joinTreeVineSpecies, c(compev_arglist, speciesType = 'all')) %>%
  select(Plot_Name, StartYear, IsQAQC, TagCode, VinePositionCode, VinePositionLabel, ScientificName, TSN)

names(vines_new)
#View(VIEWS_NETN$COMN_TreesVine)
vines_old <- left_join(xrtreecond %>% select(Tree_Data_ID, Tree_Condition_ID, Species_ID),
                       tlucond %>% select(Tree_Condition_ID, Code), by = c("Tree_Condition_ID")) %>%
  right_join(tree_evs_old, ., by = intersect(names(tree_evs_old), names(.))) %>%
  left_join(., plants %>% select(TSN, Latin_Name), by = c("Species_ID" = "TSN")) %>%
  select(Plot_Name, Year, Event_QAQC, Tree_Number_NETN, Status_ID, Code, Species_ID, Latin_Name) %>%
  filter(!is.na(Code)) %>% filter(!is.na(Plot_Name)) %>% filter(Code %in% c("VIN", "VOB")) %>%
  arrange(Plot_Name, Year, Tree_Number_NETN) %>% # drop trees without conditions
  mutate(TagCode = as.numeric(Tree_Number_NETN)) %>% select(-Tree_Number_NETN)

nrow(vines_old) #455
nrow(vines_new) #455

vines_merge <- full_join(vines_new, vines_old, by = c("Plot_Name" = "Plot_Name",
                                                      "StartYear" = "Year",
                                                      "IsQAQC" = "Event_QAQC",
                                                      "TagCode" = "TagCode",
                                                      "TSN" = "Species_ID"),
                         suffix = c("_new", "_old"))
head(vines_merge)
table(vines_merge$VinePositionCode, vines_merge$StartYear, useNA = 'always') # B only >2019. good.
table(vines_merge$Code, vines_merge$StartYear, useNA = 'always') # Numbers are the same between old and new and correct by year
table(vines_merge$ScientificName, vines_merge$Latin_Name, useNA = 'always') # Looks good

# check if trees with multiple vines are migrating
mult_vines <- vines_new %>% group_by(Plot_Name, StartYear, IsQAQC, TagCode) %>% summarize(num_vines = n()) %>% filter(num_vines > 1)
# Multiple vines are now migrating into the database, which is great. The view is actually duplicating them, so there are 4 instead of 2.
# The xref table is correct.
#+++++ No issues remaining

#----- Quadrat Character -----
qchar_new <- joinQuadData(park = 'all', from = 2006, to = 2019, locType = 'all', eventType = 'all',
                          valueType = 'all', QAQC = T)

locev <- merge(loc, event, by = "Location_ID", all.x = T, all.y = T)
drops <- anti_join(locev, plotevs_old, by = "Event_ID")
plotevs_old <- forestNETNarch::joinLocEvent(park = 'all', from = 2006, to = 2019, eventType = 'all',
                                            locType = 'all', QAQC = T, rejected = FALSE)

qchar_old <- merge(plotevs_old, quadchr, by = intersect(names(plotevs_old), names(quadchr)), all.x = T, all.y = F) %>%
  select(Plot_Name,Year, Event_QAQC, Quadrat_ID, UC:UR)
quad_names = c("UC", "UR", "MR", "BR", "BC", "BL", "ML", "UL")
head(qchar_old)

qchar_old[ , quad_names][qchar_old[ , quad_names] == 1] <- 0.1
qchar_old[ , quad_names][qchar_old[ , quad_names] == 2] <- 1.5
qchar_old[ , quad_names][qchar_old[ , quad_names] == 3] <- 3.5
qchar_old[ , quad_names][qchar_old[ , quad_names] == 4] <- 7.5
qchar_old[ , quad_names][qchar_old[ , quad_names] == 5] <- 17.5
qchar_old[ , quad_names][qchar_old[ , quad_names] == 6] <- 37.5
qchar_old[ , quad_names][qchar_old[ , quad_names] == 7] <- 62.5
qchar_old[ , quad_names][qchar_old[ , quad_names] == 8] <- 85
qchar_old[ , quad_names][qchar_old[ , quad_names] == 9] <- 97.5

qchar_old <- qchar_old %>% mutate(Cover_Type = case_when(Quadrat_ID == 2 ~ "Soil",
                                                         Quadrat_ID == 3 ~ "Rock",
                                                         Quadrat_ID == 4 ~ "Stem",
                                                         Quadrat_ID == 5 ~ "Wood",
                                                         Quadrat_ID == 6 ~ "Sphagnum",
                                                         Quadrat_ID == 7 ~ "NonSphagnum",
                                                         Quadrat_ID == 8 ~ "Lichens")) %>% # 9 = Herbs MIDN
  select(Plot_Name, Year, Event_QAQC, Cover_Type, UC, UR, MR, BR, BC, BL, ML, UL)

head(qchar_old)
table(qchar_old$Cover_Type, useNA = 'always') # Lichens are the only thing different b/c added later
table(qchar_new$CharacterLabel, useNA = 'always') # Lichens are NC for early years. OK.
table(qchar_new$Txt_Cov_BC, qchar_new$CharacterLabel, qchar_new$StartYear, useNA = 'always') # Make sure Lichens are NC for early years. They're all NC.
# Herbs shouldn't be in here for NETN! Issue line 206.
names(qchar_old)
names(qchar_new)

check_qchr <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", "IsQAQC", "CharacterLabel", col1, col2)]}
  )) %>% bind_rows()
}

quadchr_merge <- merge(qchar_new, qchar_old,
                       by.x = c("Plot_Name", "StartYear", "IsQAQC", "CharacterLabel"),
                       by.y = c("Plot_Name", "Year", "Event_QAQC", "Cover_Type"),
                       all.x = T, all.y = T)

check_qchr(quadchr_merge, "Pct_Cov_UC", "UC") # 0
check_qchr(quadchr_merge, "Pct_Cov_UR", "UR") # 0
check_qchr(quadchr_merge, "Pct_Cov_MR", "MR") # 0
check_qchr(quadchr_merge, "Pct_Cov_BR", "BR") # 0
check_qchr(quadchr_merge, "Pct_Cov_BC", "BC") # 0
check_qchr(quadchr_merge, "Pct_Cov_BL", "BL") # 0
check_qchr(quadchr_merge, "Pct_Cov_UL", "UL") # 0

qchar_vw <- get("COMN_QuadCharacter", envir = VIEWS_NETN)
table(qchar_vw$CharacterLabel)
# Herbs were dropped correctly.
#+++++ No remaining Quadrat character issues

 #----- Quadrat Species -----
quadspp_old <- forestNETNarch::joinQuadData(from = 2006, to = 2019, QAQC = T, eventType = "all", locType = "all")
quadspp_new <- joinQuadSpecies(from = 2006, to = 2019,
                               QAQC = T, eventType = 'all', locType = 'all', valueType = 'midpoint')

nrow(quadspp_new)#25296
nrow(quadspp_old) #24858
#438 new quadspp rows b/c germinants have their own row
names(quadspp_new)
names(quadspp_old)

germs <- quadspp_old %>% filter(germ.cover > 0) %>% select(-avg.cover, -avg.freq) %>%
  rename(avg.cover = germ.cover, avg.freq = germ.freq) %>% mutate(IsGerminant = 1)

rest <- quadspp_old %>% filter(germ.cover == 0) %>% select(-germ.cover, -germ.freq) %>% mutate(IsGerminant = 0)

quadspp_old2 <- rbind(rest, germs)

quadspp_merge <- full_join(quadspp_new %>% filter(IsGerminant == FALSE),
                           quadspp_old2,
                           by = c("Plot_Name" = "Plot_Name",
                                  "StartYear" = "Year",
                                  "IsQAQC" = "Event_QAQC",
                                  "TSN" = "TSN",
                                  "IsGerminant" = "IsGerminant")) %>%
  select(Plot_Name, PlotID, EventID, StartYear, cycle.x, cycle.y, IsQAQC,
         IsGerminant, Confidence, TSN, ScientificName, Latin_Name,
         num_quads, quad_avg_cov,
         quad_pct_freq, avg.cover, avg.freq, Pct_Cov_UC: Pct_Cov_UL,
         UC, UR, MR, BR, BC, BL, ML, UL)

check_qspp <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("PlotID", "EventID", "Plot_Name", "Plot_Name2", "StartYear", "IsQAQC", "ScientificName", "Latin_Name",
              "IsGerminant", col1, col2)]}
  )) %>% bind_rows()
}

# Returns 0 records now
quadspp_merge %>% mutate(cov_diff = quad_avg_cov - avg.cover) %>%
  filter(cov_diff > 0.01) %>%
  select(PlotID, EventID, Plot_Name, StartYear, IsQAQC, TSN, ScientificName, num_quads,
         quad_avg_cov, avg.cover, quad_pct_freq, avg.freq, cov_diff) #0 records

# Returns 0 records
quadspp_merge %>% mutate(freq_diff = quad_pct_freq - 100*(avg.freq)) %>%
  filter(freq_diff > 0.01) %>%
  select(PlotID, EventID, Plot_Name, StartYear, IsQAQC, TSN, ScientificName, num_quads,
         quad_avg_cov, avg.cover, quad_pct_freq, avg.freq, freq_diff)

# Unknown Spp ##s were swapped for 5 visits in 2007 that had multiple unknowns of a type (eg herb).
# No issues remain

quadsamp$numQuads <- apply(quadsamp[,c(15:22)], 1, sum)
quads1 <- merge(plotevs_old, quadsamp[, c("Event_ID", "numQuads")], all = TRUE)

quadspp <- merge(quads[,c("Event_ID","TSN","Germinant","qUC_Cover_Class_ID","qUL_Cover_Class_ID",
                        "qML_Cover_Class_ID", "qBL_Cover_Class_ID","qBC_Cover_Class_ID","qBR_Cover_Class_ID",
                        "qMR_Cover_Class_ID","qUR_Cover_Class_ID")],
                 plants[,c("TSN","Latin_Name")],
                 by = "TSN", all.x = T) %>% filter(Event_ID != "4AFBA34C-83F8-4F67-8B7C-8F6E096AB21D")

new_quads <- c("Pct_Cov_UC", "Pct_Cov_UR", "Pct_Cov_MR", "Pct_Cov_BR", "Pct_Cov_BC",
               "Pct_Cov_BL", "Pct_Cov_ML", "Pct_Cov_UL")

quads2 <- merge(quads1, quadspp, by = "Event_ID", all.x = T)
quads2[,15:22][quads2[,15:22]==1]<-0.1
quads2[,15:22][quads2[,15:22]==2]<-1.5
quads2[,15:22][quads2[,15:22]==3]<-3.5
quads2[,15:22][quads2[,15:22]==4]<-7.5
quads2[,15:22][quads2[,15:22]==5]<-17.5
quads2[,15:22][quads2[,15:22]==6]<-37.5
quads2[,15:22][quads2[,15:22]==7]<-62.5
quads2[,15:22][quads2[,15:22]==8]<-85
quads2[,15:22][quads2[,15:22]==9]<-97.5
old.names<-names(quads2[,15:22])
new.names<-c('UC','UL','ML','BL','BC','BR','MR','UR')
quads2<-quads2 %>% rename_at(all_of(vars(old.names)),~new.names)
quads2[,c(15:22)][is.na(quads2[,c(15:22)])]<-0
quads2$Plot_Name2 <- quads2$Plot_Name

quadspp_merge <- full_join(quadspp_new,
                           quads2 %>% select(Plot_Name, Plot_Name2, Year, Event_QAQC, Germinant, numQuads,
                                             TSN, Latin_Name, UC:UR),
                           by = c("Plot_Name" = "Plot_Name",
                                  "StartYear" = "Year",
                                  "IsQAQC" = "Event_QAQC",
                                  "IsGerminant" = "Germinant",
                                  "TSN" = "TSN")) %>% filter(!is.na(Plot_Name)) # drops 1 mostly NA record came in from old
head(quadspp_merge)

check_qspp(quadspp_merge, "Pct_Cov_UC", "UC") # ACAD-029. ok
check_qspp(quadspp_merge, "Pct_Cov_UR", "UR") # ACAD-029. ok
check_qspp(quadspp_merge, "Pct_Cov_MR", "MR") # ACAD-029. ok
check_qspp(quadspp_merge, "Pct_Cov_BR", "BR") # ACAD-029. ok
check_qspp(quadspp_merge, "Pct_Cov_BC", "BC") # ACAD-029. ok
check_qspp(quadspp_merge, "Pct_Cov_BL", "BL") # ACAD-029, and NS ROVA-008-2011. ok
check_qspp(quadspp_merge, "Pct_Cov_ML", "ML") # ACAD-029. ok
check_qspp(quadspp_merge, "Pct_Cov_UL", "UL") # ACAD-029. ok

head(quadspp_merge)
check_qspp(quadspp_merge, "ScientificName", "Latin_Name") # 5 weird UTF issues. all ok.
#++++++ 5/3 Duplicate species introduced in ACAD-001-2018 are the only remaining problem, which is a big problem.
# Reported issue in line 213.

#----- Microplot Shrubs -----
shrubs_vw <- get("COMN_MicroplotShrubs", envir = env) %>%
  select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, SQShrubCode,
         MicroplotCode, TSN, ScientificName, CoverClassCode, CoverClassLabel)
table(shrubs_vw$SQShrubCode, shrubs_vw$StartYear) # 6 NS in 2010 ACAD-029; SAGA-008; and UL/B in 2006
table(shrubs_vw$CoverClassCode, shrubs_vw$StartYear)
# All cover < 2010 are correctly NC.

shrub_old <- forestNETNarch::joinMicroShrubData(from = 2006, to = 2019, locType = 'all', eventType = 'all', QAQC = T)
shrub_old$Latin_Name[shrub_old$Latin_Name == "No species recorded"] <- "None present"
shrub_new <- joinMicroShrubData(from = 2006, to = 2019, locType = 'all', eventType = 'all', QAQC = T)

shrub_merge <- full_join(shrub_new, shrub_old, by = c("Plot_Name" = "Plot_Name",
                                                      "StartYear" = "Year",
                                                      "IsQAQC" = "Event_QAQC",
                                                      "ScientificName" = "Latin_Name") )

table(complete.cases(shrub_merge$Event_ID)) # all T

check_shrbs <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("PlotID", "EventID", "Plot_Name", "StartYear", "IsQAQC",
              "ScientificName", col1, col2)]}
  )) %>% bind_rows()
}

names(shrub_merge)
table(shrub_merge$Pct_Cov_UR, shrub_merge$StartYear, useNA = 'always')

shrub_merge %>% select(Plot_Name, StartYear, IsQAQC, ScientificName, shrub_avg_cov, cover) %>%
  mutate(cov_diff = abs(shrub_avg_cov - cover)) %>% filter(cov_diff > 0.1)
# The only record >2009 is SARA-023-2012 b/c duplicate Vitis in UR. Second doesn't migrate, which is fine.

#++++++ No remaining shrub issues

#----- Microplot Seedlings ------
seeds_vw <- get("NETN_MicroplotSeedlings", envir = VIEWS_NETN) %>%
           select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, SQSeedlingCode,
                  MicroplotCode, TSN, ScientificName, SizeClassCode, SizeClassLabel, Count)

table(seeds_vw$SQSeedlingCode, seeds_vw$StartYear) # NS 178 in 2006 (good).
# 2010: 6 of 7 NS are ACAD-029 & SAGA-008, which is correct.

seed_new <- joinMicroSeedlings(from = 2006, to = 2019, eventType = 'all', locType = 'all', QAQC = TRUE)
#seed_inv <- joinMicroSeedlings(speciesType = 'invasive')
table(seed_new$SQSeedlingCode, seed_new$StartYear, useNA = 'always')

# SAGA-007-2010 UL micro is now migrating correctly as NP.
# rest of NS are correctly SAGA-008-2010 and ACAD-029-2010 or seedlings
# initially entered as shrubs, so no counts available

#-------------------------------
# Microplot saplings
#-------------------------------
# Checking raw view first, so make sure SQs are correct
saps_vw <- get("NETN_MicroplotSaplings", envir = VIEWS_NETN) %>%
  select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, StartDate, IsQAQC, SQSaplingCode,
         MicroplotCode, TSN, ScientificName, DBHcm)

nrow(saps_vw[saps_vw$ScientificName == "No species recorded",])
table(saps_vw$SQSaplingCode)
  #  NP   NS   SS
  # 1819  184 5720

table(saps_vw$SQSaplingCode, saps_vw$StartYear) # Most of the NS are in 2006 for UL/B, and 6 are 2010. Correct.
table(saps_vw$SQSaplingCode, saps_vw$MicroplotCode) # Most NS are for UL/B in 2006. Correct.

saps_new <- joinMicroSaplings(locType = "all", QAQC = T, eventType = 'all', canopyForm = 'all', speciesType = 'all')
length(unique(saps_new$EventID)) #1280

saps_prep <- merge(plotevs_old, micro, by = 'Event_ID', all = TRUE)
saps_prep2 <- merge(saps_prep, saps[, 1:6], by = "Microplot_Characterization_Data_ID", all = TRUE)
saps_old <- merge(saps_prep2, plants[, c("Latin_Name", "TSN")], by = "TSN", all.x = TRUE) %>%
            mutate(Latin_Name2 = ifelse(Latin_Name == "No species recorded", "None present", Latin_Name))
head(saps_old)
head(saps_new)
saps_merge <- full_join(saps_new, saps_old, by = c("Plot_Name" = "Plot_Name",
                                                  "StartYear" = "Year",
                                                  "IsQAQC" = "Event_QAQC",
                                                  "ScientificName" = "Latin_Name2",
                                                  "MicroplotCode" = "Microplot_Name"),
                        suffix = c("_new", "_old"))

table(complete.cases(saps_merge$ScientificName)) # 6 F ACAD-029-2010 and SAGA-008-2010 b/c NS and join didn't match. OK
# Hard to check much more than this, since NETN doesn't track individual saplings.
# Will check that joinRegen returns the same values.

#----- joinRegenData -----
reg_old <- forestNETNarch::joinRegenData(from = 2006, to = 2019, QAQC = T, locType = 'all', speciesType = 'all', canopyForm = 'all') %>%
  mutate(Latin_Name2 = ifelse(Latin_Name %in% c("No species recorded", 'no species recorded'), "None present", Latin_Name),
         Latin_Name2 = ifelse(Latin_Name == "MissingData", "Not Sampled", Latin_Name2))

reg_new <- joinRegenData(from = 2006, to = 2019, QAQC = T, locType = 'all', eventType = 'all', speciesType = 'all', canopyForm = 'all')

reg_merge <- full_join(reg_new, reg_old, by = c('Plot_Name' = 'Plot_Name',
                                                'StartYear' = 'Year',
                                                'IsQAQC' = "Event_QAQC",
                                                "ScientificName" = "Latin_Name2"),
                       suffix = c("_new", "_old"))

reg_merge_ss <- reg_merge %>% filter(ScientificName != "None present")

check_reg <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("PlotID", "EventID", "Plot_Name", "StartYear", "IsQAQC",
              "ScientificName", col1, col2)]}
  )) %>% bind_rows()
}

table(reg_new$ScientificName)
table(reg_old$Latin_Name2)

stock_check <- check_reg(reg_merge_ss, "stock_new", "stock_old") # Different b/c change in stocking.
check_reg(reg_merge_ss, "seed_15_30cm", "seed15.30") # only diff is b/c SAGA-008-2010 is correctly NA in new db
check_reg(reg_merge_ss, "seed_30_100cm", "seed30.100") # SAGA-008-2010 data handled diff.
check_reg(reg_merge_ss, "seed_100_150cm", "seed100.150") # SAGA-008-2010 data handled diff.
check_reg(reg_merge_ss, "seed_p150cm", "seed150p") # SAGA-008-2010 data handled diff.

check_reg(reg_merge_ss, "seed_den", "seed.den") # SAGA-008-2010 data handled diff.
check_reg(reg_merge_ss, "sap_den", "sap.den") # SAGA-008-2010 data handled diff.

#++++++ Regen checks complete. no remaining issues

#----- Additional Species -----
addspp_vw <- get("COMN_AdditionalSpecies", envir = VIEWS_NETN)
addspp_vw$Plot_Name <- paste(addspp_vw$ParkUnit, sprintf("%03d", addspp_vw$PlotCode), sep = "-")
addspp_vw <- addspp_vw %>%
  select(Plot_Name, PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, SQAddSppCode,
         TSN, ScientificName, ConfidenceClassCode, IsCollected, Note, SQAddSppNotes)
table(addspp_vw$SQAddSppCode)
  # NP    NS    SS
  # 6     1 15257 # 5/3 15260
  # NS is ACAD-029-2010. 6 NPs are ACADs, and are correct

nrow(filter(addspp_vw, ScientificName == "No species recorded")) #0 Resolved!

addspp_new <- do.call(joinAdditionalSpecies, arglist)

addspp_old <- forestNETNarch::joinLocEvent(locType = 'all', eventType = 'all', from = 2006, to = 2019, QAQC = T) %>%
  left_join(., addspp) %>% left_join(., plants[, c("TSN", "Latin_Name")]) %>%
  select(Plot_Name, Year, Event_QAQC, TSN, Latin_Name, Confidence_ID, Collected, Notes) %>%
  mutate(Latin_Name2 = ifelse(Latin_Name == "No species recorded", "None present", Latin_Name))

addspp_merge <- merge(addspp_new, addspp_old,
                      by.x = c("Plot_Name", "StartYear", "IsQAQC", "TSN"),
                      by.y = c("Plot_Name", "Year", "Event_QAQC", "TSN"),
                      all.x = T, all.y = T)

check_spp <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", "IsQAQC", col1, col2)]}
  )) %>% bind_rows()
}

check_spp(addspp_merge, "ScientificName", "Latin_Name2")
# Some weirdness here with unknown TSNs getting switched around
# These are the same plots where Unknowns got switched around in the quad. species. Not sure it matters...
#   Plot_Name StartYear IsQAQC     ScientificName        Latin_Name2
# 1  ACAD-029      2010      0        Not Sampled               <NA> # OK
# 2  ACAD-043      2007      0               <NA>  Unknown Herb - 04 # Mig changed to Unknown Herb - 02, which is causing dups. Fix after final migration
# 3  ACAD-043      2007      0  Unknown Herb - 02               <NA> # Changed from Unknown Herb - 04; Fix after final migration
# 4  ACAD-059      2007      0               <NA> Unknown Grass - 02 # This was dropped b/c duplicate in quadspp. ~OK
# 5  ROVA-002      2007      0               <NA>  Unknown Herb - 01 # This is entirely missing from new database.
# 6  ROVA-020      2007      0               <NA>  Unknown Herb - 01 # Missing from database
# 7  ROVA-020      2007      0               <NA> Unknown Grass - 01 # Missing from database
# 8  SARA-013      2006      1 Unknown Carex - 02               <NA> # The row below was replaced with this. OK, I guess. ugh
# 9  SARA-013      2006      1               <NA>              Carex # This was changed to Unknown Carex - 02. OK, I guess.

#++++++ Issues resolved! Only returns are None present handled differently.

#------ Taxa/tlu_Plants
head(plants)
taxa_wide <- prepTaxa()
head(taxa_wide)
taxa_merge <- merge(taxa_wide, plants[, c(1,4:9,11:21,24)], by = "TSN",
                    all.x = T, all.y = T)

check_taxa <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("TSN", "ScientificName", "Latin_Name", col1, col2)]}
  )) %>% bind_rows()
}

check_taxa(taxa_merge, "ScientificName", "Latin_Name") # 2 weird UTF things. NO issues
check_taxa(taxa_merge, "Order.x", "Order.y") # 3 weird UTF things. NO issues
check_taxa(taxa_merge, "Family.x", "Family.y") # 3 weird UTF things. NO issues
check_taxa(taxa_merge, "Genus.x", "Genus.y") #0
check_taxa(taxa_merge, "Tree.x", "Tree.y") # Differences are b/c of new Tree/Shrub column
check_taxa(taxa_merge, "Shrub.x", "Shrub.y") # Differences are b/c of new Tree/Shrub and Vine exclusive columns
check_taxa(taxa_merge, "Vine.x", "Vine.y") # Persicaria was removed from the list b/c herbaceous
check_taxa(taxa_merge, "Herbaceous.x", "Herbaceous.y")
check_taxa(taxa_merge, "Graminoid.x", "Graminoid.y")

check_taxa(taxa_merge, "CommonName", "Common") # Differences are b/c old common has lists

taxa_wide %>% filter(TSN == -9999999951)

#+++++ Only remaining issue is remove "No species recorded" from list +++++

#----- Soil data
# Soil sample data
soilsamp_old <- read.csv("./testing_scripts/soilsamp_data_old.csv")
soillab_old <- read.csv("./testing_scripts/soillab_data_old.csv")
tbl_ssd <- read.csv("C:/Forest_Health/exports/tbl_Soil_Sample_Data.csv")

length(unique(soilsamp_old$Event_ID)) # 819
length(unique(soillab_old$Event_ID)) # 749

# Check views
soilsamp_vw <- get("COMN_SoilSample", envir = env) %>%
  select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC,
         SQSoilCode, SampleSequenceCode, SoilLayerLabel,
         Depth_cm, Note) %>%
  filter(StartYear > 2006 & !is.na(SoilLayerLabel) #& StartYear < 2020
  )

table(soilsamp_vw$SQSoilCode) # All SS. Good.
length(unique(soilsamp_vw$EventID)) # 820

soillab_vw <- get("COMN_SoilLab", envir = env) %>%
  select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC,
         LabLayer, LabDateSoilCollected, UMOSample:ECEC, LabNotes, EventID, PlotID) %>%
  filter(StartYear > 2006 #& StartYear < 2020
  )

length(unique(soillab_vw$EventID)) # 823. Looks similar enough to move on.

# Now to compare old and new

# need to remove records that weren't sampled but had earthworms recorded
soildata2 <- soildata[!grepl("[++]", soildata$Notes), -c(10:13)] # drop updated/created cols

soil_old <- merge(soildata2, soilsamp[,-c(13:16)],
                  by = intersect(names(soildata2), names(soilsamp[,-c(13:16)])), all = TRUE)

plotevs_old <- forestNETNarch::joinLocEvent(from = 2007, to = 2019, QAQC = T, locType = 'all', eventType = 'all')
soil_old2 <- merge(plotevs_old, soil_old, by = intersect(names(plotevs_old), names(soil_old)), all.x = FALSE, all.y = TRUE) %>%
  filter(!is.na(Location_ID)) %>% select(Plot_Name, Year, Event_QAQC,
                                         Sampling_Position, Sample_Type, Horizon_Type, Archived,
                                         Sample_Number, Litter_Depth, FF_Depth,
                                         A_Horizon_Depth, Total_Excavation_Depth, Notes, Comments, Sample_Missed) %>%
  filter(Year > 2006)

# convert NA horizons to 0, but not total
soil_old2[,c(9:11)][is.na(soil_old2[,c(9:11)])]<-0
names(soil_old2)

soilold_sum <- soil_old2 %>% group_by(Plot_Name, Year, Event_QAQC, Sample_Type, Archived) %>%
                             summarize(litter = mean(Litter_Depth, na.rm = T),
                                      O_hor = mean(FF_Depth, na.rm = T),
                                      A_hor = mean(A_Horizon_Depth, na.rm = T),
                                      Tot_dep = mean(Total_Excavation_Depth, na.rm =T),
                                      numsamps = length(unique(!is.na(Sample_Number))))

  #soilsamp_wide comes from line 130 in joinSoilSampleData.R
soilsamp_wide$Plot_Name <-  paste(soilsamp_wide$ParkUnit, sprintf("%03d", soilsamp_wide$PlotCode), sep = "-")

soilsamp_merge <- merge(soilsamp_wide, soilold_sum,
                        by.x = c("Plot_Name", "StartYear", "IsQAQC"),
                        by.y = c("Plot_Name", "Year", "Event_QAQC"), all = T)

soilsamp_merge %>% filter(is.na(EventID))
# Only ACAD-029-2010 and SARA-015-2018. OK

check_soils <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", "IsQAQC", "Sampling_Position", "SampleSequenceCode", col1, col2)]}
  )) %>% bind_rows()
}
names(soilsamp_merge)
soilsamp_merge %>% mutate(lit_diff = abs(Litter_cm - litter),
                                            O_diff = abs(O_Horizon_cm - O_hor),
                                            A_diff = abs(A_Horizon_cm - A_hor),
                                            tot_diff = abs(Total_Depth_cm - Tot_dep)) %>%
                  filter(lit_diff > 0.5 | O_diff > 0.5 | A_diff > 0.5 | tot_diff > 0.5)

# Only returns 2 records
# SAGA-009-2010 is missing a total depth for Sample 3. Fixed missing value. Should migrate corrected in next version.
# MORR-008-2011 did not have a 3rd sample collected. Removed note, so hopefully migrates as NS instead of SS.
#+++++5/3 0 records returned.

check_soils(soilsamp_merge, "Note", "Comments") # 0

# Soil lab data
names(soildata2)
names(soillab)

soillab_old <- merge(soildata2, soillab[,-c(1, 34, 35)],
                     by = intersect(names(soildata2), names(soillab[,-c(1, 34, 35)])), all = TRUE)

names(soillab_old)

soillab_old2 <- merge(plotevs_old, soillab_old, by = intersect(names(plotevs_old), names(soillab_old)),
                      all.x = FALSE, all.y = TRUE) %>%
  filter(!is.na(Location_ID) & Year > 2006) %>% select(Plot_Name, Year, Event_QAQC, Layer, UMO_Sample:ECEC, Notes,
                                         Sampling_Position, Sample_Type, Archived)
head(soillab_old2)

soillab_new <- get("COMN_SoilLab", envir = VIEWS_NETN) %>%
                     select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC,
                            LabLayer, LabDateSoilCollected, UMOSample:ECEC, LabNotes, EventID, PlotID) %>%
                            filter(!is.na(UMOSample))# %>%
 # filter(LabLayer %in% c("10 cm", "10cm - NonVS", "A", "A - NonVS", "O", "O/A")) #%>% filter(StartYear > 2006)
soillab_new$Plot_Name <-  paste(soillab_new$ParkUnit, sprintf("%03d", soillab_new$PlotCode), sep = "-")

soillab_merge <- full_join(soillab_new, soillab_old2, by = c("Plot_Name" = "Plot_Name",
                                                             "StartYear" = "Year",
                                                             "UMOSample" = "UMO_Sample"),
                           suffix = c("_new", "_old"))
names(soillab_merge)

check_lab <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", "IsQAQC", "UMOSample", "LabLayer", col1, col2)]}
  )) %>% bind_rows()
}

soillab_old_act <- soillab_old %>% filter(Layer %in% c("10cm", "A", "O", "O/A"))
nrow(soillab_old_act) #1193
table(soillab_old_act$Layer)
#10cm    A    O  O/A
#220  405  544   24

soillab_new_act <- soillab_new %>% filter(LabLayer %in% c("10 cm", "A", "O", "O/A"))
nrow(soillab_new_act) #1193
table(soillab_new_act$LabLayer)
# 10 cm     A     O   O/A
# 220     405   544    24

# Same numbers. Good
#tn <- check_lab(soillab_merge, "pctTN", "X..TN") # Something weird is going on with join that I can't seem to figure out.
# Going to spot check instead.

#++++++++ No lab-related issues to report based on spot checks

# Done with 4/26 migration check

#-------------------------------------
# Testing summary functions with latest updates
plotTreeMap(plotName = "WEFA-001", from = 2016, to = 2019 ) # worked

quadtest <- sumQuadGuilds(speciesType = 'invasive')
saptest <- sumSapDBHDist()
spplist <- sumSpeciesList()
spplist <- sumSpeciesList(speciesType = 'invasive')
strstg <- sumStrStage()
trdist <- sumTreeDBHDist()
