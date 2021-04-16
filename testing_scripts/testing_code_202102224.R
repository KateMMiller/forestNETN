#------------------------
# Code for testing and retooling forest database
#------------------------
library(forestNETN)
library(tidyverse)
#importData()

path = "C:/Forest_Health/exports/NETN"
importCSV(path = path)#, zip_name = "NETN_Forest_20210318.zip")
# microbenchmark::microbenchmark(importData(),
#                                importCSV("C:/Forest_Health/exports/NETN"),
#                                times = 1) #importCSV is 4+ times faster

# microbenchmark::microbenchmark(importData(),
#                                importData(server = "INPNETN-078644"),
#                                times = 1) #no difference; not surprising

# Function to check that the rows in each col. 1 and 2 are identical
# Will return 0 if none, or the values that differ

check_data <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", col1, col2)]}
  )) %>% bind_rows()
}

#------------------------
# Plot and Visit data views
#------------------------
# Comparing full plots events join from views with arch db.
plot_events <- joinLocEvent(park = 'all', from = 2006, to = 2019, QAQC = TRUE, panels = 1:4,
                            locType = 'all', eventType = 'all', abandoned = T, output = 'verbose')

head(plot_events)
pe_testing <- joinLocEvent(park = "all")

pe_testing <- joinLocEvent(park = c("ACAD"))
pe_testing <- joinLocEvent(park = c("ACAD", "MABI"))
table(pe_testing$ParkUnit)
pe_testing <- joinLocEvent(park = c("ACAD", "MABI"), from = 2015, to = 2021)
table(pe_testing$StartYear)
pe_testing <- joinLocEvent(QAQC = F)
table(pe_testing$IsQAQC)
pe_testing <- joinLocEvent(locType = "all")
table(pe_testing$PlotTypeCode)
pe_testing$Plot_Name[pe_testing$PlotTypeCode == 'Non-VS'] # all MORR-014. good.
pe_testing <- joinLocEvent(eventType = 'complete') #removes ACAD-029. good.
nrow(pe_testing)
pe_testing <- joinLocEvent(abandoned = TRUE)
table(pe_testing$IsAbandoned)

plot_events_old <- read.csv("./testing_csvs/plot_events.csv")

pe_merge <- merge(plot_events, plot_events_old, by.x = c("EventLegacyID", "Plot_Name"),
                   by.y = c("Event_ID", "Plot_Name"), all.x = T, all.y = T)
names(pe_merge)


names(pe_merge)
check_data(pe_merge, "ParkSubUnit", "Unit_ID")
check_data(pe_merge,"xCoordinate", "X_Coord")
check_data(pe_merge,"yCoordinate", "Y_Coord")
check_data(pe_merge,"StartDate", "Start_Date")
check_data(pe_merge,"PanelCode", "Panel")
check_data(pe_merge,"Event_QAQC", "IsQAQC")
test <- check_data(pe_merge,"ZoneCode", "UTM_Zone")
test
table(pe_merge$ZoneCode, pe_merge$UTM_Zone)
check_data(pe_merge,"Orientation.x", "Orientation.y")
check_data(pe_merge,"cycle.x", "cycle.y")
check_data(pe_merge,"PlotTypeCode", "Loc_Type")
check_data(pe_merge,"PlotLegacyID", "Location_ID")
check_data(pe_merge,"Aspect.x", "Aspect.y")
check_data(pe_merge,"PhysiographyCode", "Physiographic_Class")
plot_check <- unique(pe_merge[, c("ParkUnit", "Plot_Name")])
table(plot_check$ParkUnit) # ACAD-088 is back

names(pe_merge)

dir_dif <- check_data(pe_merge,"Directions.x", "Directions.y")

write.csv(dir_dif, "./testing_csvs/Directions_check.csv") # Didn't find any issues to report

#---------------------------
#  Stand data views
#---------------------------
# Views haven't completely settled for this function, so I'm just going to compare raw
# view data to original function to find migration issues via setdiff and table checks
# ---- StandInfoPhotos
stand_old <- read.csv("./testing_csvs/Stand_Data.csv")
stand_new <- get("NETN_StandInfoPhotos", envir = VIEWS_NETN)[, -c(41:46)]
stand_new$Plot_Name <- paste(stand_new$ParkUnit, sprintf("%03d", stand_new$PlotCode), sep = "-")
stand_new$Event_QAQC <- ifelse(stand_new$IsQAQC == 0, FALSE, TRUE)
st_merge <- merge(stand_new, stand_old, by.x = c("Plot_Name", "StartYear", "Event_QAQC"),
                  by.y = c("Plot_Name", "Year", "Event_QAQC"),
                  all.x = T, all.y = T) %>% filter(StartYear < 2020)


table(complete.cases(st_merge$Event_ID))
table(complete.cases(st_merge$Location_ID))

check_data(st_merge, "ParkUnit", "Unit_Code")
check_data(st_merge,"Stand_Structure", "StandStructureSummary") # 2 plots correctly missing ACAD-029.2010/MIMA-015.2008
check_data(st_merge, "Earthworms", "EarthwormCode") #NA changed to PM
check_data(st_merge,"Microtopography_ID", "MicrotopographyCode") #NA changed to PM
check_data(st_merge,"Panel", "PanelCode")

table(st_merge$Crown_Closure_ID, st_merge$CrownClosureLabel)
table(st_merge$Event_QAQC, st_merge$IsQAQC)
table(st_merge$Stand_Structure, st_merge$StandStructureSummary)

#table(st_merge$Pct_Understory_Low, st_merge$)
#---- StandForestFloor
stand2_new <- get("COMN_StandForestFloor", envir = VIEWS_NETN)[, -c(19:23)]
names(stand2_new)
stand2_new$Plot_Name <- paste(stand2_new$ParkUnit, sprintf("%03d", stand2_new$PlotCode), sep = "-")
stand2_new <- stand2_new[, c("Plot_Name", "Network", "ParkUnit", "ParkSubUnit", "PlotTypeCode",
                             "PlotTypeLabel", "PlotCode", "IsAbandoned", "PanelCode",
                             "PanelLabel", "StartDate", "IsQAQC", "StartYear",
                             "EventID", "PlotID","ForestFloorCode", "ForestFloorLabel", "CoverClassLabel")]

st_wide <- stand2_new %>% pivot_wider(id_cols = c(Plot_Name:PlotID), names_from = ForestFloorLabel,
                                      values_from = CoverClassLabel) %>%
           mutate(Event_QAQC = ifelse(IsQAQC == 0, FALSE, TRUE))

st_wide <- st_wide %>% rename_all(function(x) gsub(" ", "_", x))
st_wide <- st_wide %>% rename_all(function(x) gsub("-", "_", x))

head(st_wide)

st_merge2 <- merge(st_wide, stand_old, by.x = c("Plot_Name", "StartYear", "Event_QAQC"),
                   by.y = c("Plot_Name", "Year", "Event_QAQC"),
                  all.x = T, all.y = T) %>% filter(StartYear <2020)
table(complete.cases(st_merge2$Event_ID))
table(complete.cases(st_merge2$Location_ID))

table(st_merge2$Pct_Lichen_Cover, st_merge2$Lichen)
table(st_merge2$Pct_Bare_Soil_Cover, st_merge2$Bare_Soil)
table(st_merge2$Pct_Bryophyte_Cover, st_merge2$Non_Vascular)
table(st_merge2$Pct_Rock_Cover, st_merge2$Rock)
table(st_merge2$Pct_Surface_Water_Cover, st_merge2$Water)
table(st_merge2$Pct_Trampled_Cover, st_merge2$Trampled)

#---- StandPlantCoverStrata
names(VIEWS_NETN)
stand3_new <- get("COMN_StandPlantCoverStrata", envir = VIEWS_NETN)[, -c(19:23)]
stand3_new$Plot_Name <- paste(stand3_new$ParkUnit, sprintf("%03d", stand3_new$PlotCode), sep = "-")
stand3_new$Event_QAQC <- ifelse(stand3_new$IsQAQC == 0, FALSE, TRUE)
names(stand3_new)
head(stand3_new)

st_wide3 <- stand3_new %>% select(-StrataCode, -StrataSummary) %>%
  pivot_wider(id_cols = c(Network:StartYear, EventID:Event_QAQC),
              names_from = StrataLabel,
              values_from = CoverClassCode) %>% select(-"NA")

st_wide3 <- st_wide3 %>% rename_all(function(x) gsub(" ", "_", x))
st_wide3 <- st_wide3 %>% rename_all(function(x) gsub("-", "_", x))

names(st_wide3)
st_merge3 <- merge(stand_old, st_wide3, by.x = c("Plot_Name", "Year", "Event_QAQC"),
                  by.y = c("Plot_Name", "StartYear", "Event_QAQC"), all.x = T, all.y = T) %>% filter(Year < 2020)

table(st_merge3$Pct_Understory_Low, st_merge3$Ground)
table(st_merge3$Pct_Understory_Mid, st_merge3$Mid_understory)
table(st_merge3$Pct_Understory_High, st_merge3$High_understory)

#---- StandDisturbances
stand_dist <- get("COMN_StandDisturbances", envir = VIEWS_NETN)[,-c(23:29)]
stand_dist$Plot_Name <- paste(stand_dist$ParkUnit, sprintf("%03d", stand_dist$PlotCode), sep = "-")
stand_dist$Event_QAQC <- ifelse(stand_dist$IsQAQC == 0, FALSE, TRUE)
head(stand_dist)

# Didn't summarize this in forestNETN yet, so have to do some importing/joining
library(RODBC)
db <-odbcConnect("NETNFVM") #
disturb<-sqlFetch(db,"tbl_Disturbances")
disttlu<-sqlFetch(db,"tlu_Disturbance_Codes")
disttlutc<-sqlFetch(db,"tlu_Disturbance_Threshhold_Codes")
odbcClose(db)
head(disturb)

intersect(names(disturb), names(plot_events_old))
st_dist_o <- merge(plot_events_old[, c("Plot_Name", "Event_ID", "Event_QAQC", "Year")],
                   disturb[, c(2:6)], by = "Event_ID", all.x = T, all.y = T)
head(st_dist_o)
names(st_dist_o)
names(stand_dist)
table(complete.cases(st_dist_o$Disturbance_Code))

dist_merge <- merge(stand_dist, st_dist_o, by.x = c("Plot_Name", "StartYear", "Event_QAQC"),#, "Disturbance.Code"),
                    by.y = c("Plot_Name", "Year", "Event_QAQC"),#, "Disturbance_Code"),
                     all.x = T, all.y = T) %>%
  filter(StartYear<2020)

dist_merge <- merge(stand_dist, st_dist_o, by.x = c("Plot_Name", "StartYear", "Event_QAQC", "DisturbanceCode"),
                    by.y = c("Plot_Name", "Year", "Event_QAQC", "Disturbance_Code"),
                    all.x = T, all.y = T) %>%
  filter(StartYear<2020)


dist_merge[which(!complete.cases(dist_merge$DisturbanceCode)),]

#check_data(dist_merge, "Disturbance.Code", "Disturbance_Code")
check_data(dist_merge, "Disturbance_Threshold", "ThresholdCode") %>% arrange(Plot_Name)
check_data(dist_merge, "Disturbance_Notes", "DisturbanceNote")
table(dist_merge$CoverClass.Label, dist_merge$Disturbance_Cover_Class_ID)

#---- StandSlopes
slopes <- get("COMN_StandSlopes", envir = VIEWS_NETN) %>% filter(StartDate < 2020)
slopes$Plot_Name <- paste(slopes$ParkUnit, sprintf("%03d", slopes$PlotCode), sep = "-")
slopes$Event_QAQC <- ifelse(slopes$IsQAQC == 0, FALSE, TRUE)

library(RODBC)
db <-odbcConnect("NETNFVM") #
stand_d<-sqlFetch(db,"tbl_Stand_Data")
odbcClose(db)

names(stand_d)
names(plot_events_old)
stand_df <- merge(plot_events_old[,c("Event_ID", "Plot_Name", "Year","Event_QAQC")],
                  stand_d[,c("Event_ID", "Derived_Plot_Slope", "Slope_UP",
                             "Slope_BR" ,"Slope_BL")], by = "Event_ID", all.x = T, all.y = F)

head(stand_df)

table(complete.cases(stand_df$Event_ID))
table(complete.cases(stand_df$Plot_Name))

sl_wide <- slopes %>% select(Plot_Name, Event_QAQC, StartYear,
                             PlotSlope, CWDSlope, TransectCode) %>%
           pivot_wider(id_cols = c(Plot_Name:PlotSlope),
                       names_from = TransectCode,
                       values_from = CWDSlope) %>% select(-"NA")

names(sl_wide)
sl_merge <- merge(sl_wide, stand_df, by.x = c("Plot_Name", "StartYear", "Event_QAQC"),
                  by.y = c("Plot_Name", "Year", "Event_QAQC"), all.x = T, all.y = T)

options(scipen = 100)
names(sl_merge)

der_slope <- check_data(sl_merge, "Derived_Plot_Slope", "PlotSlope") %>%
             mutate(diff = Derived_Plot_Slope - PlotSlope) %>%
  filter(abs(diff) > 0.5)# diff. in rounding
der_slope # no issues

# otherwise okay
check_data(sl_merge, "UP", "Slope_UP") %>% mutate(diff = UP - Slope_UP) %>% filter(abs(diff)>0.1)
check_data(sl_merge, "BR", "Slope_BR") %>% mutate(diff = BR - Slope_BR) %>% filter(abs(diff)>0.1)
check_data(sl_merge, "BL", "Slope_BL") %>% mutate(diff = BL - Slope_BL) %>% filter(abs(diff)>0.1)

#-----StandTreeHeights
# This is not going to be fun...
tr_ht <- stand_d %>% select(Event_ID, Stand_Structure_ID, Stunted_Woodland, Tree_1_Number_Codom:Height_Tree_3_Inter) %>%
           filter(Stand_Structure_ID != 5) %>% filter(Stunted_Woodland == 0)

tr_ht2 <- merge(plot_events_old[,c("Event_ID", "Plot_Name", "Year", "Event_QAQC")],
                tr_ht, by = "Event_ID", all.x = FALSE, all.y = TRUE) %>% filter(!is.na(Plot_Name))

table(complete.cases(tr_ht2$Plot_Name))

tr_ht_w1 <- tr_ht2 %>% select(Event_ID:Tree_3_Number_Inter) %>%
  pivot_longer(cols = c(Tree_1_Number_Codom:Tree_3_Number_Inter),
               names_to = "Samp",
               values_to = "Tree_Number") %>%
  mutate(Samp_Num = case_when(str_detect(Samp, "_1_") ~ 1L,
                              str_detect(Samp, "_2_") ~ 2L,
                              str_detect(Samp, "_3_") ~ 3L),
         Crown = ifelse(str_detect(Samp, "Codom"), "Codom", "Inter")
)


tr_ht_w2 <- tr_ht2 %>% select(Event_ID:Event_QAQC, Height_Tree_1_Codom:Height_Tree_3_Inter) %>%
  pivot_longer(cols = c(Height_Tree_1_Codom:Height_Tree_3_Inter),
               names_to = "Samp_ht",
               values_to = "Height_m") %>%
  mutate(Samp_Num = case_when(str_detect(Samp_ht, "_1_") ~ 1L,
                              str_detect(Samp_ht, "_2_") ~ 2L,
                              str_detect(Samp_ht, "_3_") ~ 3L),
         Crown = ifelse(str_detect(Samp_ht, "Codom"), "Codom", "Inter")
  )

tr_ht3 <- merge(tr_ht_w1, tr_ht_w2, by = c("Event_ID", "Plot_Name", "Year", "Event_QAQC", "Samp_Num", "Crown"),
                all.x = T, all.y = T) %>% select(-Samp, -Samp_ht) %>% filter(!is.na(Height_m))
head(tr_ht3)
# tr_ht3 is ready to compare with the view
tr_ht_vw <- get("COMN_StandTreeHeights", envir = VIEWS_NETN)[, -c(18:24, 27)]
tr_ht_vw$Plot_Name <- paste(tr_ht_vw$ParkUnit, sprintf("%03d", tr_ht_vw$PlotCode), sep = "-")
tr_ht_vw$Event_QAQC <- ifelse(tr_ht_vw$IsQAQC == 0, FALSE, TRUE)
names(tr_ht3)
names(tr_ht_vw)

tree_height_comps <- merge(tr_ht_vw,
                           tr_ht3,
                           by.x = c("Plot_Name", "StartYear", "Event_QAQC", "TagCode"),
                           by.y = c("Plot_Name", "Year", "Event_QAQC", "Tree_Number"),
                     all.x = T, all.y = T) %>%
                     select(Plot_Name:Event_QAQC, Stand_Structure_ID, Stunted_Woodland,
                            CrownClassCode, CrownClassLabel, CrownClassSummary,
                            TagCode, Height, Height_m, Crown, Samp_Num) %>% filter(StartYear < 2020)


table(complete.cases(tree_height_comps$Height),
        tree_height_comps$StartYear)

miss_heights <- tree_height_comps[which(is.na(tree_height_comps$Height)),]
write.csv(miss_heights, "./testing_scripts/NETN_missing_tree_heights.csv", row.names = F)
trht_comps <- tree_height_comps %>% filter(StartYear > 2010)

check_data(trht_comps, "Height", "Height_m") %>%
  mutate(diff = Height - Height_m) %>% filter(abs(diff)>0.1)

table(trht_comps$Crown, trht_comps$CrownClassLabel) #all good

#----- joinStandData -----
names(VIEWS_NETN)
# stand views: NETN_StandInfoPhotos, COMN_StandPlantCoverStrata, COMN_StandDisturbances,
# COMN_StandForestFloor, COMN_StandSlopes, COMN_StandTreeHeights
#forestNETNarch::importData()
stand_old <- forestNETNarch::joinStandData(from = 2006, to = 2019, QAQC = T)
head(stand_old)
stand_old2 <- merge(stand_old, stand[,c("Event_ID", "Deer_Browse_Line_pre09_ID")],
                    by = "Event_ID", all.x = T, all.y = T)

dbi_check <- merge(stand_old2[,c("Plot_Name", "Year", "Event_QAQC", "Deer_Browse_Line_ID", "Deer_Browse_Line_pre09_ID")],
                   standinfo[,c("Plot_Name", "StartYear", "IsQAQC", "Deer_Browse_Index")],
                   by.x = c("Plot_Name", "Year", "Event_QAQC"),
                   by.y = c("Plot_Name", "StartYear", "IsQAQC"),
                   all.x = T, all.y = T) # checks out.
names(standinfo)
head(standinfo)
head(stand_old)
names(stand_old)
trht_old <- stand_old %>% select(Plot_Name, Year, Event_QAQC, Avg_Codom_HT, Avg_Inter_HT)
#pulled in from joinStandData.R strand ht calcs.
treeht_sum$Event_QAQC <- ifelse(treeht_sum$IsQAQC == 1, TRUE, FALSE)
treeht_sum$Plot_Name <- paste(treeht_sum$ParkUnit, str_pad(treeht_sum$PlotCode, 3, side = 'left', "0"), sep = "-")
treeht_sum$StartYear <- as.numeric(treeht_sum$StartYear)

treeht_check <- merge(stand_old[, c("Plot_Name", "Year", "Event_QAQC", "Avg_Codom_HT", "Avg_Inter_HT")],
                      treeht_sum,
                      by.x = c("Plot_Name", "Year", "Event_QAQC"),
                      by.y = c("Plot_Name", "StartYear", "Event_QAQC"),
                      all.x = T,
                      all.y = T
                      )

treeht_check2 <- treeht_check %>% mutate(diff_cod = abs(Avg_Codom_HT - Avg_height_Codom),
                                        diff_int = abs(Avg_Inter_HT - Avg_height_Inter)) %>%
                                 filter(diff_cod > 0.1 | diff_int > 0.1) # ACAD-083-2012; MORR-009-2011

slope_check <- stand_comb %>% filter(is.na(PlotSlope)) # it's all the QAQC visits

stand_new <- joinStandData(park = 'all', from = 2006, to = 2019, output = 'short', QAQC = T)
table(complete.cases(stand_new[,c(1:16, 19)])) # all TRUE
table(complete.cases(stand_new[,17])) # 1
table(complete.cases(stand_new[,18])) # 4
table(complete.cases(stand_new[,20])) # DBI- lots NC/PM
table(stand_new$ParkUnit, stand_new$StartYear)
stand_new$Plot_Name[stand_new$StartYear == 2012 & stand_new$ParkUnit == "ACAD"] # ACAD-088 is still missing

head(stand_new)
head(stand_old)
names(stand_new)

stand_check <- merge(stand_new,
                     stand_old, all.x = T, all.y = T,
                     by.x = c("Plot_Name", "StartYear", "IsQAQC"),
                     by.y = c("Plot_Name", "Year", "Event_QAQC"))
names(stand_check)

table(stand_check$Stand_Structure.x, stand_check$Stand_Structure.y)
check_data(stand_check, "Earthworms.x", "Earthworms.y") #ACAD-088
check_data(stand_check, "Microtopography", "Microtopography_ID") #ACAD-088
check_data(stand_check, "Deer_Browse_Index", "Deer_Browse_Line_ID") #missing <2009
table(stand_check$Deer_Browse_Index, stand_check$Deer_Browse_Line_ID)
check_data(stand_check, "Pct_Understory_Low.x", "Pct_Understory_Low.y")#ACAD-088
check_data(stand_check, "Pct_Understory_Mid.x", "Pct_Understory_Mid.y")#ACAD-088
check_data(stand_check, "Pct_Understory_High.x", "Pct_Understory_High.y")#ACAD-088
check_data(stand_check, "Pct_Bare_Soil", "Pct_Bare_Soil_Cover")#ACAD-088
check_data(stand_check, "Pct_Bryophyte", "Pct_Bryophyte_Cover")#ACAD-088
check_data(stand_check, "Pct_Rock", "Pct_Rock_Cover")#ACAD-088
check_data(stand_check, "Pct_Water", "Pct_Surface_Water_Cover")#ACAD-088
check_data(stand_check, "Pct_Trampled", "Pct_Trampled_Cover")#ACAD-088
check_data(stand_check, "Pct_Crown_Closure.x", "Pct_Crown_Closure.y")#ACAD-088

#---------------------------
#  CWD views
#---------------------------
cwd_old <- read.csv("./testing_csvs/CWD_old_db.csv")

library(RODBC)
db <-odbcConnect("NETNFVM") #
cwd <- sqlFetch(db,"tbl_CWD_Transect_Data")
odbcClose(db)

cwd_old_check <- cwd %>% select(Event_ID, Transect) %>%
  group_by(Event_ID) %>% summarize(num_trans = length(unique(Transect))) %>%
  filter(num_trans < 3) %>% left_join(., plot_events_old, by = "Event_ID")
# 2 visits ACAD-018-2010 and SARA-014-2010 missing No spp. for BL.
cwd_vol <- joinCWDData()
head(cwd_vol)
table(cwd_vol$ParkUnit)
cwd_vol1 <- joinCWDData(park = c("ACAD", "MABI"))
cwd_vol2 <- joinCWDData(park = c("ROVA", "WEFA"), from = 2007, to = 2011,
                        locType = "all", output = 'verbose')
cwd_vol3 <- joinCWDData(park = 'all', panels = c(1,3), QAQC = TRUE)
cwd_vol4 <- joinCWDData(park = 'all', QAQC = TRUE, units = 'acres')

head(cwd_vol2)


table(cwd_vol2$ParkUnit)
names(cwd_vol)
names(cwd_old)

cwd_check1 <- merge(cwd_vol[cwd_vol$StartYear == 2016 & cwd_vol$ParkUnit == "ACAD", c("Plot_Name", "StartYear", "CWD_Vol")],
              cwd_old[cwd_old$Year == 2016 & cwd_old$Unit_Code == "ACAD", c("Plot_Name", "Year", "CWD_Vol")],
              by.x = c("Plot_Name", "StartYear"),
              by.y = c("Plot_Name", "Year"), all.x = T, all.y = T) %>% filter(is.na(CWD_Vol.x))
# ACAD-088 is still missing and hasn't been resolved yet.

cwd_new_calc <- cwd_vol %>% group_by(ParkUnit, StartYear, IsQAQC) %>%
  summarize(num_plots = sum(!is.na(Plot_Name)),
            sum_CWD = sum(CWD_Vol),
            mean_CWD = mean(CWD_Vol))

cwd_old_calc <- cwd_old %>% filter(Plot_Name != "MORR-014") %>%
  group_by(Unit_Code, Year, Event_QAQC) %>%
  summarize(num_plots = sum(!is.na(Plot_Name)),
            sum_CWD = sum(CWD_Vol, na.rm = T),
            mean_CWD = mean(CWD_Vol, na.rm = T)) %>%
  mutate(IsQAQC = ifelse(Event_QAQC == TRUE, 1, 0))

options(scipen = 100)
cwd_merge1 <- merge(cwd_new_calc, cwd_old_calc,
                    by.x = c("ParkUnit", "StartYear", "IsQAQC"),
                    by.y = c("Unit_Code", "Year", "IsQAQC"),
                    all.x = T, all.y = T) %>%
              mutate(diff_plots = num_plots.x - num_plots.y,
                     diff_sum = sum_CWD.x - sum_CWD.y,
                     diff_mean = mean_CWD.x - mean_CWD.y) %>%
  filter(abs(diff_plots) > 0 | abs(diff_sum) > 0.5 | abs(diff_mean) > 0.5)
# The 2 plots that are diff (ACAD/SARA) will be fixed for migration


cwd_merge <- merge(cwd_old, cwd_vol,
                   by.x = c("Plot_Name", "Year", "Event_QAQC", "Latin_Name", "Decay_Class_ID"),
                   by.y = c("Plot_Name", "StartYear", "Event_QAQC", "ScientificName", "DecayClassCode"),
                   all.x = TRUE, all.y = TRUE) #%>%

cwd_check <- cwd_merge %>% mutate(diff = CWD_Vol.x - CWD_Vol.y) %>% filter(abs(diff) > 0.1) %>%
             select(Plot_Name, Year, IsQAQC, Latin_Name, Decay_Class_ID, CWD_Vol.x, CWD_Vol.y, diff) %>%
  filter(IsQAQC == FALSE) # Original function didn't pull in cwd slopes from first visit, and
 # calculated volume assuming 0 slope, which was wrong. We never used the function for QAQC visits
 # so it didn't introduce any issues. Otherwise, the only 2 plots with diff. b/t old and new are
 # missing a NP in 1 transect, so denominator is diff. b/t old and new.

#---------------------------
# Tree Data
#---------------------------
forestNETNarch::importData()

# library(RODBC)
# db <-odbcConnect("NETNFVM") #
# plants <- sqlFetch(db,"tlu_Plants")
# odbcClose(db)

netn_tree <- forestNETNarch::joinTreeData(from = 2006, to = 2019, locType = 'all', QAQC = TRUE) %>%
             mutate(TagCode = as.numeric(Tree_Number_NETN))

#++++ AFTER MIGRATION RERUN CHECKS WITH THIS tree_new INSTEAD OF THE ONE GENERATED BY THE FUNCTION
# tree_new <- VIEWS_NETN$COMN_TreesByEvent %>%
#   select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, TreeLegacyID,
#          TagCode, TaxonID, TSN, ScientificName, Fork, Azimuth, Distance, DBHcm, IsDBHVerified,
#          IsDBHUnusual, TreeStatusCode, TreeStatusLabel, CrownClassCode, CrownClassLabel,
#          DecayClassCode, HWACode, BBDCode, TreeEventNote) %>%
#   filter(StartYear < 2020)

#Subsets and Left joins early makes things a lot faster!
microbenchmark::microbenchmark(
tree_new <- joinTreeData(from = 2006, to = 2019, locType = 'all', eventType = 'complete', QAQC = TRUE,
                         output = 'verbose'),
tree_new2 <- joinTreeData(from = 2019, to = 2019, locType = 'all', eventType = 'complete', QAQC = TRUE,
                         output = 'verbose'), times =1)

head(tree_new)

tree_new <- joinTreeData(from = 2006, to = 2019, locType = 'all', eventType = 'complete', QAQC = TRUE,
                         output = 'verbose', status = 'all')

head(joinTreeData(park = c("MIMA", "WEFA")))
head(joinTreeData(park = c("MIMA", "WEFA"), from = 2006, to = 2009))
head(joinTreeData(park = 'all', status = 'active'))

trees_live <- joinTreeData(park = 'all', from = 2006, to = 2019, speciesType = 'all', locType = 'all',
                           status = 'live', QAQC = T, eventType = 'all')
trees_all <- joinTreeData(park = 'all', from = 2006, to = 2019, speciesType = 'all', locType = 'all',
                          status = 'all', QAQC = T, eventType = 'all')
trees_dead <- joinTreeData(park = 'all', from = 2006, to = 2019, speciesType = 'all', locType = 'all',
                           status = 'dead', QAQC = T, eventType = 'all')
trees_active <- joinTreeData(park = 'all', status = 'active', QAQC = T, locType = 'all',
                              eventType = 'all')

tree_fol_live <- joinTreeFoliageCond(park = 'all', from = 2006, to = 2019, speciesType = 'all',
                                     locType = 'all', status = 'live', QAQC = T, eventType = 'all')

names(trees_live)
tree_fol_sm <- joinTreeFoliageCond(park = "WEFA", from = 2016, to = 2019, speciesType = 'native')
tree_fol_sm <- joinTreeFoliageCond(park = "WEFA", from = 2016, to = 2019, speciesType = 'native',
                                   valueType = 'classes')

head(tree_fol_sm)

table(tree_new$TreeStatusCode)
table(netn_tree$Status_ID) # codes 2, DS, EX, NL, XP are slightly different. Need to figure out why

# intermediate step in function
tree_taxa <- merge(tree_vw,
                   taxa[,c('TSN','ScientificName','CommonName','Family', 'Genus', 'IsExotic')],
                   by = c("TSN", "ScientificName"), all.x = TRUE, all.y = FALSE) %>% filter(StartYear < 2020)

tree_taxa$Plot_Name <- paste(tree_taxa$ParkUnit, str_pad(tree_taxa$PlotCode, 3, side = 'left', "0"), sep = "-")
intersect(names(trees), names(treedata))
names(trees)
names(treedata)
tree_old <- merge(trees[,1:15], treedata[,1:16], by = "Tree_ID", all.x = T, all.y = T)
plot_events_old <- forestNETNarch::joinLocEvent(from = 2006, to = 2019, QAQC = T, locType = 'all')

#------------
tree_old2 <- merge(plot_events_old, tree_old, by = c("Event_ID", "Location_ID"), all.x = T, all.y = T)
tree_old2$TagCode <- as.numeric(tree_old2$Tree_Number_NETN)
head(tree_old2)

unique(tree_old2$Event_ID[tree_old2$Plot_Name == "ACAD-029" & tree_old2$Year == 2010])

tree_merge <- merge(tree_new, tree_old2[tree_old2$Location_ID !="8CC21CA9-9528-4A1D-A06D-64B95E89886D" &
  tree_old2$Event_ID != "3BF64BE2-7089-42B6-B610-09B3511BF1B4",],
                    by.x = c("Plot_Name", "StartYear", "IsQAQC", "TagCode"),
                    by.y = c("Plot_Name", "Year", "Event_QAQC", "TagCode"),
                    all.x = T, all.y = T)

check_trees <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", "TagCode", "IsQAQC", "ScientificName",
              "Status_ID", "TreeStatusCode",col1, col2)]}
  )) %>% bind_rows()
}

# The old tree join didn't include the fork column. Need to add that for checking
check_trees(tree_merge, "TSN.x", "TSN.y") #SARA-015-2012. Should be fixed in next migration
check_trees(tree_merge, "Azimuth.x", "Azimuth.y") #SARA-015-2012
check_trees(tree_merge, "Fork.x", "Fork.y")

names(tree_merge)
#check_data(tree_merge, "Distance.x", "Distance.y")
tree_dist <- tree_merge %>% mutate(dist_diff = abs(Distance.x - Distance.y)) %>%
  filter(dist_diff > 0.1) # 0 records
tree_dist

tree_dbh <- tree_merge %>% mutate(dbh_diff = abs(DBH - DBHcm)) %>%
  filter(dbh_diff > 0) %>%
  select(Plot_Name, StartYear, IsQAQC, TagCode, ScientificName, DBHcm, DBH, dbh_diff)
tree_dbh
# Interestingly there's a ghost tree ACAD-019-2006-QAQC tag 30 that has dbh 17.5.
# The migration is dropping it, so nothing to be worried about.
# Documented DBHs that need to be fixed so migration rounding doesn't round up.

names(tree_merge)
status_check <- check_trees(tree_merge, "TreeStatusCode", "Status_ID")
status_check2 <- tree_merge %>% select(Plot_Name, StartYear, IsQAQC, TagCode,
                                       ScientificName, Status_ID, TreeStatusCode)
status_check2
names(tree_merge)

crown_check <- check_trees(tree_merge, "Crown_Class_ID", "CrownClassCode")
crown_check

table(tree_new$TreeStatusCode, tree_new$StartYear)
table(netn_tree$Crown_Class_ID, netn_tree$Status_ID)
table(tree_new$CrownClassCode, tree_new$TreeStatusCode)

table(tree_merge$TreeStatusCode, tree_merge$CrownClassCode, useNA = "always")
#5+9+221+169+102 #506
#CrownClassCode for trees <2010 aren't migrating correctly.
#It's partially our fault, because we inconsistently recorded crown classes
#for trees with status_ID = 2 (dead trees). Trees with Status_ID = 2 and NULL
#Crown_Class_ID are migrating in as "PM", but should be NULL (605 records).
#Trees with Status_ID = 2 and a value in Crown_Class_ID should also be set to NULL
#(506 records) to be consistent.


decay_check<-check_trees(tree_merge, "Decay_Class_ID", "DecayClassCode")
# Status codes for live trees (1) from first cycle are coming in as NC.
# but should be NULL
#The TreeEventDecayClass_Insert.sql script logic isn't quite right. We've always
#collected Decay_Class_ID, so any missing values on dead trees should be "PM".
#But PM should only be used for dead trees (not live trees). Currently the script
#is assigning PM for decay class for live trees <2010. In case it helps:
#Status_ID from 2006-2009 was 0 (unk), 1 (live), 2 (dead), then switched to 2-letter
#system (eg AS, AB, DS, DF, etc) starting in 2010.
# THere are several DFs that had a decay class in original db, that are NA now. That's good.


hwa_check <- check_trees(tree_merge, "HWACode", "HWA_Status")
hwa_check
table(tree_old2$HWA_Status, tree_old2$Year, useNA = 'always')
# Cycle one not treated correctly
hwa_check <- tree_merge %>% filter(#ScientificName == "Tsuga canadensis" &
                                     is.na(HWACode))
table(tree_merge$ScientificName, tree_merge$HWALabel, useNA = 'always')
# TSUCAN Not Applicables are all on dead trees
#The TreeEventHWA_Insert.sql script logic isn't quite right.
#The start date in the CASE WHEN should be 1/1/2010 instead of 1/1/2009 (will change 48 PMs in 2009 to NC).
#There are also a 46 records of HWACodes that are NA in 2010 and 2 for 2011 for non-hemlock trees.
#I think these are supposed to be 0 instead of NA, based on the rest of the data in the xref.

tree10 <- tree_merge %>% filter(StartYear == 2010)
table(tree10$ScientificName, tree10$HWACode, useNA = 'always')
tree11 <- tree_merge %>% filter(StartYear == 2011)
table(tree11$ScientificName, tree11$HWACode, useNA = 'always')
#8+37+1+2 #48
#The TreeEventHWA_Insert.sql script logic isn't quite right. The start date in the
#CASE WHEN should be 1/1/2010 instead of 1/1/2009 (will change 48 PMs in 2009 to NC).
#There are also a 46 records of HWACodes that are NA in 2010 and 2 for 2011 for non-hemlock trees.
#I think these are supposed to be 0 instead of NA, based on the rest of the data in the xref.

check_trees(tree_merge, "BBDCode", "BBD_Status")
# BBD is migrating correctly

names(tree_merge)
dbh_ver <- check_trees(tree_merge, "IsDBHVerified", "DBH_Verified")

table(tree_merge$IsDBHVerified, tree_merge$TreeStatusCode, tree_merge$StartYear, useNA = 'always')
# differences in whether 0 or NA are for dead or excluded trees. Looks okay.

names(VIEWS_NETN$COMN_TreesFoliageCond)

# totfol <- VIEWS_NETN$COMN_TreesFoliageCond %>%
#   mutate(Plot_Name = paste(ParkUnit, str_pad(PlotCode, 3, side = 'left', "0"), sep = "-")) %>%
#   select(Plot_Name, StartYear, IsQAQC, TagCode, TotalFoliageCondition.Label) %>% unique()

# head(totfol)
# head(tree_merge)
# tree_merge2 <- merge(tree_merge, totfol, by = c("Plot_Name", "StartYear", "IsQAQC", "TagCode"),
#                      all.x = T, all.y = T)
# head(tree_merge2)
table(tree_merge$Txt_Tot_Foliage_Cond, tree_merge$Total_Foliage_Condition, useNA = 'always')

fol_check <- tree_merge %>% select(Plot_Name, StartYear, IsQAQC, TagCode, ScientificName, TreeStatusCode,
                                    Status_ID, Txt_Tot_Foliage_Cond, Total_Foliage_Condition)
# Lots of NC where should be NO or NULL (dead). Camilla captured this in the legacy tracker. Will check
# this again after Stephen makes the changes.

# Done checking records that only have 1/tree/visit. Now to check foliage and tree conditions

#---------------------
# Foliage Conditions
#---------------------

fol_cond <- joinTreeFoliageCond()
fol_test <- joinTreeFoliageCond(park = "ACAD", from = 2016, to = 2019, speciesType = 'native',
                                valueType = 'classes')
head(fol_test)

# Will wait to test the actual values until after Stephen's fixes
#---------------------
# Tree Conditions
#---------------------
