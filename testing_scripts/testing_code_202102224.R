#------------------------
# Code for testing and retooling forest database
#------------------------
library(forestNETN)
library(tidyverse)
#importData()
# ?importData()
# importData(instance = 'local', server = "localhost", new_env = T) # release 1.0.21 with IsGerminant added to NETNquadspp

path = "C:/Forest_Health/exports/NETN"
list.files(path)

#exportCSV(path, zip = TRUE)
importCSV(path = path, zip_name = "NETN_Forest_20210409.zip") #includes soils views with my name changes

#path = "C:/Forest_Health/exports/NETN"
#importCSV(path = path, zip_name = "NETN_Forest_20210322.zip") # Release from 1.0.21
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
# dplyr::left_join is WAY faster than merge
microbenchmark::microbenchmark(
tree_new <- joinTreeData(from = 2006, to = 2019, locType = 'all', eventType = 'complete', QAQC = TRUE,
                         output = 'verbose'),
tree_new2 <- joinTreeData(from = 2019, to = 2019, locType = 'all', eventType = 'complete', QAQC = TRUE,
                         output = 'verbose'), times =1)

head(tree_new)

tree_new <- joinTreeData(from = 2006, to = 2019, locType = 'all', eventType = 'all', QAQC = TRUE,
                         output = 'verbose', status = 'all')

head(joinTreeData(park = c("MIMA", "WEFA")))
test <- (joinTreeData(park = c("MIMA", "WEFA"), from = 2006, to = 2009))
table(test$ParkUnit)

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
plot_events_old <- forestNETNarch::joinLocEvent(locType = 'all', eventType = 'all')

tree_old2 <- merge(plot_events_old, netn_tree, by = intersect(names(plot_events_old), names(netn_tree)),
                   all.x = T, all.y = T)
tree_old2$TagCode <- as.numeric(tree_old2$Tree_Number_NETN)
head(tree_old2)

unique(tree_old2$Event_ID[tree_old2$Plot_Name == "ACAD-029" & tree_old2$Year == 2010])
names(tree_old2)

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


hwa_check <- check_trees(tree_merge, "HWALabel", "HWA_Status")
hwa_check

table(tree_old2$HWA_Status, tree_old2$Year, useNA = 'always')
table(tree_merge$HWALabel, tree_merge$StartYear, useNA = 'always')

hwa10p <- tree_merge %>% filter(StartYear > 2009)

table(tree_merge$HWALabel, tree_merge$HWA_Status, tree_merge$StartYear, useNA = 'always')
write.csv(tree_merge, "testing_scripts/tree_HWA_checking.csv", row.names = F)
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
head(fol_cond)
fol_test <- joinTreeFoliageCond(park = "ACAD", from = 2016, to = 2019, speciesType = 'native',
                                valueType = 'classes')
head(fol_test)

# Will wait to test the actual values until after Stephen's fixes
#---------------------
# Tree Conditions
#---------------------
park = 'all'
from = 2006
to = 2019
QAQC = T
locType = 'all'
eventType = 'all'
panels = 1:4
#status = 'all'
# status = 'active'
# status = 'dead'
status = 'live'
speciesType = 'all'
dist_m <- NA
# Vines did not migrate correctly
# NETN correct condition counts
  #H	    AD	BBD	BWA	CAVL CAVS	 CW	   DBT	EAB	 EB	  EHS 	G	GM	HWA	ID	NO	  OTH	RPS	SB	VIN	VOB
  #15682	927	569	162	362	 349	3880	2412	2	   932	163	193	1	 206	30	1854	136	2	  2	  422	35
table(VIEWS_NETN$COMN_TreesConditions$TreeConditionCode)
head(VIEWS_NETN$COMN_TreesVine)
table(VIEWS_NETN$COMN_TreesVine$VinePositionCode, VIEWS_NETN$COMN_TreesVine$ParkUnit)

trcond <- joinTreeConditions(from = 2010, to = 2010, speciesType = 'exotic', QAQC = T)

#------------------
# Vines
#------------------

vines <- joinTreeVineSpecies()
vine_test <- joinTreeVineSpecies(park = 'MABI')
vine_test <- joinTreeVineSpecies(from = 2015, to = 2015)
vine_test <- joinTreeVineSpecies(from = 2006, to = 2019, speciesType = 'exotic')

#-----------------
# Quadrat Character
#-----------------
library(forestNETN)
library(tidyverse)
#importData()

path = "C:/Forest_Health/exports/NETN"
importCSV(path = path, zip_name = "NETN_Forest_20210327.zip") # Release from 1.0.21 fixed IsGerminant
forestNETNarch::importData()

qchar_new <- joinQuadData(park = 'all', from = 2006, to = 2019, locType = 'all', eventType = 'all',
                      valueType = 'all', QAQC= T)
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
table(qchar_old$Cover_Type, useNA = 'always') # after dropping SARA.915, matches perfect
table(qchar_new$CharacterLabel, useNA = 'always')
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

check_qchr(quadchr_merge, "Pct_Cov_UC", "UC") # all good
check_qchr(quadchr_merge, "Pct_Cov_UR", "UR") # all good
check_qchr(quadchr_merge, "Pct_Cov_MR", "MR") # all good
check_qchr(quadchr_merge, "Pct_Cov_BR", "BR") # all good
check_qchr(quadchr_merge, "Pct_Cov_BC", "BC") # all good
check_qchr(quadchr_merge, "Pct_Cov_BL", "BL") # all good
check_qchr(quadchr_merge, "Pct_Cov_UL", "UL") # all good

# Quadrat character data checks out

#------------------------
# A lot of functions rely on joinLocEvent, so benchmarking to see how much faster dplyr makes it
microbenchmark::microbenchmark(
  joinLocEvent(),
  joinLocEvent(park = 'WEFA', from = 2019, to = 2019),
  times = 5
  )

#-------------------------
# Quadrat Species
#-------------------------
forestNETNarch::importData()
park = 'all'
from = 2006
to = 2019
QAQC = T
locType = 'all'
eventType = 'all'
panels = 1:4
#status = 'all'
# status = 'active'
# status = 'dead'
status = 'live'
speciesType = 'all'
dist_m <- NA
microbenchmark::microbenchmark(
quadspp_old <- forestNETNarch::joinQuadData(from = 2006, to = 2019, QAQC = T, eventType = "all", locType = "all"),
quadspp_new <- joinQuadSpecies(from = 2006, to = 2019, QAQC = T, eventType = 'all'),
times = 5
) # despite more cumbersome dataset, new function is faster.

quadspp_new <- joinQuadSpecies(from = 2006, to = 2019,
                               QAQC = T, eventType = 'all', locType = 'all', valueType = 'midpoint')
nrow(quadspp_new[quadspp_new$IsGerminant == FALSE,])
nrow(quadspp_old)

quadspp_test <- joinQuadSpecies(park = 'ACAD', from = 2006, to = 2019, QAQC = T, eventType = 'all')
quadspp_test <- joinQuadSpecies(park = 'ACAD', from = 2006, to = 2019, speciesType = 'invasive')
quadspp_test <- joinQuadSpecies(park = 'MABI', speciesType= 'native')
quadspp_test <- joinQuadSpecies(park = 'MABI', speciesType= 'invasive')
quadspp_test <- joinQuadSpecies(park = 'MABI', speciesType= 'native')
quadspp_test <- joinQuadSpecies(park = "ROVA", speciesType = 'native',
                                eventType = "complete", valueType = 'averages')
nrow(quadspp_test)
head(quadspp_old)
head(quadspp_new)

# I changed how I'm handling germinants, so have to drop for now
quadspp_old2 <- quadspp_old %>% #filter(!(avg.cover == 0 & germ.cover > 0)) %>%
                                filter(Latin_Name != "No species") %>%
                                mutate(germ_old = ifelse(germ.cover > 0, 1, 0))

quadspp_merge <- full_join(quadspp_new %>% filter(IsGerminant == FALSE),
                           quadspp_old2, by = c("Plot_Name" = "Plot_Name",
                                               "StartYear" = "Year",
                                               "IsQAQC" = "Event_QAQC",
                                               "TSN" = "TSN")) %>%
                 select(Plot_Name, PlotID, EventID, StartYear, cycle.x, cycle.y, IsQAQC,
                        IsGerminant, germ_old, Confidence, TSN, ScientificName, Latin_Name,
                        num_quads, quad_avg_cov,
                        quad_pct_freq, avg.cover, avg.freq, Pct_Cov_UC: Pct_Cov_UL,
                        UC, UR, MR, BR, BC, BL, ML, UL)
head(quadspp_merge)
names(quadspp_merge)

check_qspp <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("PlotID", "EventID", "Plot_Name", "StartYear", "IsQAQC", "ScientificName", "Latin_Name",
              "IsGerminant", "germ_old", col1, col2)]}
  )) %>% bind_rows()
}

quadspp_cov <- quadspp_merge %>% mutate(cov_diff = quad_avg_cov - avg.cover) %>%
                                 filter(cov_diff > 0.01) %>%
                                 select(PlotID, EventID, Plot_Name, StartYear, IsQAQC, TSN, ScientificName, num_quads,
                                        quad_avg_cov, avg.cover, quad_pct_freq, avg.freq, cov_diff)

quadfreq_cov <- quadspp_merge %>% mutate(freq_diff = quad_pct_freq - 100*(avg.freq)) %>%
  filter(freq_diff > 0.01) %>%
  select(PlotID, EventID, Plot_Name, StartYear, IsQAQC, TSN, ScientificName, num_quads,
         quad_avg_cov, avg.cover, quad_pct_freq, avg.freq, freq_diff)

# Unknown Spp ##s got swapped occassionally in the migration
# see NETN_quad_spp_mismatch.xlsx for list of affected plots
new_quads <- c("Pct_Cov_UC", "Pct_Cov_UR", "Pct_Cov_MR", "Pct_Cov_BR", "Pct_Cov_BC",
               "Pct_Cov_BL", "Pct_Cov_ML", "Pct_Cov_UL")

names(quadspp_merge)

quadspp_merge[new_quads][quadspp_merge[new_quads] > 0] <- 1

UC<-check_qspp(quadspp_merge, "Pct_Cov_UC", "UC") %>% filter(germ_old == 0) %>%
  filter(!is.na(Pct_Cov_UC))

UR<-check_qspp(quadspp_merge, "Pct_Cov_UR", "UR") %>% filter(germ_old == 0) %>%
  filter(!is.na(Pct_Cov_UR))

MR<-check_qspp(quadspp_merge, "Pct_Cov_MR", "MR") %>% filter(germ_old == 0) %>%
  filter(!is.na(Pct_Cov_MR))

BR<-check_qspp(quadspp_merge, "Pct_Cov_BR", "BR") %>% filter(germ_old == 0) %>%
  filter(!is.na(Pct_Cov_BR))

BC<-check_qspp(quadspp_merge, "Pct_Cov_BC", "BC") %>% filter(germ_old == 0) %>%
  filter(!is.na(Pct_Cov_BC))

BL<-check_qspp(quadspp_merge, "Pct_Cov_BL", "BL") %>% filter(germ_old == 0) %>%
  filter(!is.na(Pct_Cov_BL))

ML<-check_qspp(quadspp_merge, "Pct_Cov_ML", "ML") %>% filter(germ_old == 0) %>%
  filter(!is.na(Pct_Cov_ML))

UL<-check_qspp(quadspp_merge, "Pct_Cov_UL", "UL") %>% filter(germ_old == 0) %>%
  filter(!is.na(Pct_Cov_UL))

head(quadspp_merge)

spp_check <- check_qspp(quadspp_merge, "ScientificName", "Latin_Name") %>% filter(germ_old == 0)


#----------------------
# Quad Notes
#----------------------
quadspp_test <- read.csv(paste0(path, "NETN_QuadSpecies.csv"))
head(quadspp_test)

# quadspp_wide <- quadspp_test %>% select(-QuadratLabel, -CoverClassLabel, - CoverClassSummary, -IsTrampled, -SQQuadSppCode,
#                                         -SQQuadSppLabel, -SQQuadSppNotes, -SQQuadSppSummary) %>%
#   group_by(PlotID, EventID, TSN, QuadratCode) %>%
#   mutate(ScientificName = ifelse(row_number() > 1,
#                                  paste0(ScientificName, "_", row_number()),
#                                  paste(ScientificName))) %>%
#                                  pivot_wider(names_from = QuadratCode,
#                                              values_from = CoverClassCode,
#                                              values_fill = NA_real_)
#
# write.csv(quadspp_wide, "NETN_QuadSpecies_wide.csv", row.names = F)
#

quad_notes <- joinQuadNotes()
#------------------------
# Microplot- Shrubs
#------------------------
forestNETNarch::importData()
shrub_old <- forestNETNarch::joinMicroShrubData(from = 2006, to = 2019, locType = 'all', eventType = 'all', QAQC = T)
shrub_old$Latin_Name[shrub_old$Latin_Name == "No species recorded"] <- "None present"
shrub_new <- joinMicroShrubData(from = 2006, to = 2019, locType = 'all', eventType = 'all', QAQC = T)

shrub_exo <- joinMicroShrubData(from = 2006, to = 2019, speciesType = 'invasive')
shrub2 <- joinMicroShrubData(park = c("ACAD", "MABI"))
length(unique(shrub2$Plot_Name))
length(unique(shrub_exo$Plot_Name)) #352

shrub_merge <- full_join(shrub_new, shrub_old, by = c("Plot_Name" = "Plot_Name",
                                                       "StartYear" = "Year",
                                                       "IsQAQC" = "Event_QAQC",
                                                       "ScientificName" = "Latin_Name") )

table(complete.cases(shrub_merge$Event_ID)) # 2 NAs ACAD-029-2010 & SAGA-008-2010; have a better way to handle now


check_shrbs <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("PlotID", "EventID", "Plot_Name", "StartYear", "IsQAQC",
              "ScientificName", col1, col2)]}
  )) %>% bind_rows()
}

names(shrub_merge)
shrub_dif <- shrub_merge %>% select(Plot_Name, StartYear, IsQAQC, ScientificName, shrub_avg_cov, cover) %>%
                mutate(cov_diff = abs(shrub_avg_cov - cover)) #%>% filter(cov_diff > 0.1)
# Shouldn't have any cover data for records < 2009; Not sure why sometimes we have that in the database
# The only record >2009 is SARA-023-2012 b/c duplicate Vitis in UR. Second doesn't migrate, which is fine.

#---------------------------------
# Connect to NETN_Forest_Dev to get updated taxa
#---------------------------------
params <- list(Driver = "SQL Server Native Client 11.0",  # You may need to change this depending on what driver you have
               Server = "INP2300VTSQL16\\IRMADEV1",  # Your server name goes here
               Database = "NETN_Forest_Dev",  # Your db name goes here
               Trusted_Connection = "Yes",
               drv = odbc::odbc())

conn <- do.call(pool::dbPool, params)
taxa_test <- dplyr::tbl(con, dbplyr::in_schema("Analysis", "COMN_Taxa")) %>% collect()
write.csv(taxa_test, paste0(path, "/", "COMN_Taxa.csv"), row.names = F)

#----------------------------------
# Microplot Seedlings
#----------------------------------
library(forestNETN)
library(tidyverse)
path = "C:/Forest_Health/exports/NETN"
importCSV(path = path, zip_name = "NETN_Forest_20210405.zip") #includes manually fixed taxa

park = 'all'
from = 2006
to = 2019
QAQC = T
locType = 'all'
eventType = 'all'
panels = 1:4
#speciesType = 'all'
speciesType = 'exotic'
numMicros = 3
canopyForm = 'all'

microbenchmark::microbenchmark(joinMicroSeedlings(from = 2006, to = 2019, canopyForm = 'all',
                                                  eventType = 'all', locType = 'all', QAQC = TRUE), times = 5)

seed_new <- joinMicroSeedlings(from = 2006, to = 2019, eventType = 'all', locType = 'all', QAQC = TRUE)
length(unique(seed_new$EventID))

sinv <- joinMicroSeedlings(from = 2006, to = 2019, eventType = 'all', locType = 'all', QAQC = TRUE, speciesType = 'invasive')
length(unique(sinv$EventID))
snat <- joinMicroSeedlings(from = 2006, to = 2019, eventType = 'all', locType = 'all', QAQC = TRUE, speciesType = 'native')
length(unique(snat$EventID))
sexo <- joinMicroSeedlings(from = 2006, to = 2019, eventType = 'all', locType = 'all', QAQC = TRUE, speciesType = 'exotic')
length(unique(sexo$EventID))

m1 <- joinMicroSeedlings(numMicros = 1)
length(unique(m1$EventID))

acad_sd <- joinMicroSeedlings(park = 'ACAD')
p2 <- joinMicroSeedlings(park = c("MABI", "MIMA"))
table(p2$ParkUnit)
# SAGA-007-2010 UL micro is migrating as ND instead of NP. Not sure why that's happening
# rest of issues in seedling data are SAGA-007-2010 and ACAD-029-2010 or seedlings
# initially entered as shrubs, so no counts available

#-------------------------------
# Microplot saplings
#-------------------------------
saps_new <- joinMicroSaplings(locType = "all", QAQC = T, eventType = 'all')
length(unique(saps_new$EventID))
sap_exo <- joinMicroSaplings(speciesType = 'exotic')
length(unique(sap_exo$EventID))

sap19 <- joinMicroSaplings(from = 2019, to = 2019, speciesType = 'native', canopyForm = 'canopy')

#--------------------------
# Regen
#----------------------
forestNETNarch::importData()

microbenchmark::microbenchmark(
reg_new <- joinRegenData(eventType = 'all', locType = 'all', QAQC = T, canopyForm = 'all'),
reg_old <- forestNETNarch::joinRegenData(eventType = 'all', locType = 'all', QAQC = T,
                                         from = 2006, to = 2019, canopyForm = 'all'),
times = 5) # new one is faster, even though it's clunky too

length(unique(reg_new$EventID)) #1278 - dropped the 2 problem plots ACAD-029 and SAGA-008

reg_old$Latin_Name[reg_old$Latin_Name == "No species recorded"] <- "None present"
head(reg_new)
head(reg_old)
reg_old2 <- reg_old %>% filter(!Latin_Name %in% c("None present", "no species recorded"))

reg_merge <- merge(reg_new, reg_old2, by.x = c("Plot_Name", "StartYear", "IsQAQC", "ScientificName"),
                   by.y = c("Plot_Name", "Year", "Event_QAQC", "Latin_Name"),
                   all.x=T, all.y=T) %>% filter(ScientificName != "None present")
merge_nas <- reg_merge[is.na(reg_merge$Network),] # all NAs explainable by blank/NP handling
# old reg fun included None present when saplings or seedlings had a spp and were non pres.

check_reg <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", "IsQAQC", "ScientificName", col1, col2)]}
  )) %>% bind_rows()
}
head(reg_merge)
# Issues below (n=36) are all due to diff/better error handling in new function
check_reg(reg_merge, "seed_15_30cm", "seed15.30")
check_reg(reg_merge, "seed_30_100cm", "seed30.100")
check_reg(reg_merge, "seed_100_150cm", "seed100.150")
check_reg(reg_merge, "seed_p150cm", "seed150p")
check_reg(reg_merge, "sap_den", "sap.den")

reg_merge2 <- reg_merge %>% select(Plot_Name, StartYear, IsQAQC, ScientificName, num_micros:regen_den,
                                   seed15.30:stock.y)
names(reg_merge2)
# Stocking is going to change because dropping saplings >2.5 cm per FIA and EFWG
check_stock <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", "IsQAQC", "ScientificName", "sap_stems", "sap_stems_SI",
              "sap_den", "sap.den", col1, col2)]}
  )) %>% bind_rows()
}

stock_diff <- check_stock(reg_merge2, "stock.x", 'stock.y')
View(stock_diff)
check_reg(reg_merge2, "sap_den", "sap.den")
# when all saps < 2.5" same stocking index between old and new

#---------------------
# Additional Species
#---------------------
addspp_vw <- get("COMN_AdditionalSpecies", envir = VIEWS_NETN)
addspp_vw$Plot_Name <- paste(addspp_vw$ParkUnit, sprintf("%03d", addspp_vw$PlotCode), sep = "-")
addspp_vw <- addspp_vw %>%
  select(Plot_Name, PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, TSN, ScientificName,
         ConfidenceClassCode, IsCollected, Note, SQAddSppNotes)

table(addspp_vw$SQAddSppCode)

head(addspp_vw)
head(addspp)

addspp2 <- forestNETNarch::joinLocEvent(locType = 'all', eventType = 'all', from = 2006, to = 2019, QAQC = T) %>%
                             left_join(., addspp) %>% left_join(., plants[, c("TSN", "Latin_Name")]) %>%
                           select(Plot_Name, Year, Event_QAQC, TSN, Latin_Name, Confidence_ID, Collected, Notes)
names(addspp_vw)
names(addspp2)
addspp_merge <- merge(addspp_vw, addspp2,
                      by.x = c("Plot_Name", "StartYear", "IsQAQC", "TSN"),
                      by.y = c("Plot_Name", "Year", "Event_QAQC", "TSN"),
                      all.x = T, all.y = T)


check_spp <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", "IsQAQC", col1, col2)]}
  )) %>% bind_rows()
}

check_spp(addspp_merge, "ScientificName", "Latin_Name")
# Some weirdness here with unknown TSNs getting switched around
# These are the same plots where Unknowns got switched around in the quad. species. Not sure it matters...
#   Plot_Name  StartYear IsQAQC   ScientificName           Latin_Name
#1  ACAD-043      2007      0               <NA>     Unknown Herb - 04
#2  ACAD-043      2007      0  Unknown Herb - 02                  <NA>
#3  ACAD-059      2007      0               <NA>    Unknown Grass - 02
#4  ROVA-002      2007      0               <NA>     Unknown Herb - 01
#5  ROVA-020      2007      0               <NA>     Unknown Herb - 01
#6  ROVA-020      2007      0               <NA>    Unknown Grass - 01
#7  SARA-013      2006      1 Unknown Carex - 02                  <NA>
#8  SARA-013      2006      1               <NA>                 Carex

addspp_test <- joinAdditionalSpecies()


#---------------------
# Check taxa table
#---------------------
head(plants)

taxa_wide <- prepTaxa()
head(taxa_wide)
names(plants)
taxa_merge <- merge(taxa_wide, plants[, c(1,4:9,11:21,24)], by = "TSN",
                    all.x = T, all.y = T)

check_taxa <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("TSN", "ScientificName", "Latin_Name", col1, col2)]}
  )) %>% bind_rows()
}
head(taxa_merge)

check_taxa(taxa_merge, "ScientificName", "Latin_Name")
check_taxa(taxa_merge, "Order.x", "Order.y")
check_taxa(taxa_merge, "Family.x", "Family.y")
check_taxa(taxa_merge, "Genus.x", "Genus.y")
check_taxa(taxa_merge, "Tree.x", "Tree.y")
check_taxa(taxa_merge, "Shrub.x", "Shrub.y")
check_taxa(taxa_merge, "Vine.x", "Vine.y")
check_taxa(taxa_merge, "Herbaceous.x", "Herbaceous.y")
check_taxa(taxa_merge, "Graminoid.x", "Graminoid.y")

check_taxa(taxa_merge, "CommonName", "Common")
names(taxa_merge)

#-----------------------------
# Soil data
#-----------------------------
# Soil sample data
soilsamp_old <- read.csv("./testing_scripts/soilsamp_data_old.csv")
soillab_old <- read.csv("./testing_scripts/soillab_data_old.csv")

tbl_ssd <- read.csv("C:/Forest_Health/exports/tbl_Soil_Sample_Data.csv")
head(tbl_ssd)
names(tbl_ssd)
ff_o <- table(tbl_ssd$FF_Depth, tbl_ssd$O_Horizon_Depth)
ff_check <- tbl_ssd %>% mutate(ff_o = ifelse (O_Horizon_Depth > 0, FF_Depth - O_Horizon_Depth, 0))

View(ff_o)
#------ Running Mark's SQL------------
# Server connection
connect_serv <- "Driver={SQL Server};server=INP2300VTSQL16\\IRMADEV1;database=NETN_Forest;trusted_connection=TRUE;ReadOnly=True"
# Local connection
connect <- "Driver={SQL Server};server=localhost\\SQLEXPRESS;database=NETN_Forest;trusted_connection=TRUE;ReadOnly=True"

con <- RODBC::odbcDriverConnect(connection = connect, readOnlyOptimize = TRUE, rows_at_time = 1)

path_db <- "C:/Forest_Health/schema/"

# Import Soil header
soilheader_qry <- readr::read_lines(paste0(path_db, "COMN_SoilHeader.sql")) %>%
  glue::glue_collapse(sep = "\n") %>%
  glue::glue_sql(.con = conn)

COMN_SoilHeader <- RODBC::sqlQuery(con, soilheader_qry)

# Import soil lab data
soillab_qry <- readr::read_lines(paste0(path_db, "COMN_SoilLab.sql")) %>%
  glue::glue_collapse(sep = "\n") %>%
  glue::glue_sql(.con = conn)

COMN_SoilLab <- RODBC::sqlQuery(con, soillab_qry)
names(COMN_SoilLab)
#Import Soil sample data
soilsample_qry <- readr::read_lines(paste0(path_db, "COMN_SoilSample.sql")) %>%
  glue::glue_collapse(sep = "\n") %>%
  glue::glue_sql(.con = conn)

COMN_SoilSample <- RODBC::sqlQuery(con, soilsample_qry)
RODBC::odbcClose(con)
names(COMN_SoilSample)

assign("COMN_SoilHeader", COMN_SoilHeader, envir = VIEWS_NETN)
assign("COMN_SoilLab", COMN_SoilLab, envir = VIEWS_NETN)
assign("COMN_SoilSample", COMN_SoilSample, envir = VIEWS_NETN)

names(VIEWS_NETN)
names(VIEWS_NETN$COMN_SoilHeader)

# Exporting, so the soil views are included when I run importCSV
exportCSV(path = "C:/Forest_Health/exports/NETN", zip = T)
#--------------------------------------
# This is form the joinSoilData function
soilsamp_wide$Plot_Name <-  paste(soilsamp_wide$ParkUnit, sprintf("%03d", soilsamp_wide$PlotCode), sep = "-")

#tbl_ssd <- read.csv("C:/Forest_Health/exports/tbl_Soil_Sample_Data.csv")

forestNETNarch::importData(type = 'file',
  path = 'D:/NETN/Monitoring_Projects/Forest_Health/Database/2021_Forest_Database/Forest_Backend_NETN_20210409_Migration.mdb')
names(soildata)
names(soilsamp)
# need to remove records that weren't sampled but had earthworms recorded
soildata2 <- soildata[!grepl("[++]", soildata$Notes), -c(10:13)] # drop updated/created cols


soil_old <- merge(soildata2, soilsamp[,-c(13:16)],
                  by = intersect(names(soildata2), names(soilsamp[,-c(13:16)])), all = TRUE)
plotevs_old <- forestNETNarch::joinLocEvent(from = 2006, to = 2019, QAQC = T, locType = 'all', eventType = 'all')
soil_old2 <- merge(plotevs_old, soil_old, by = intersect(names(plotevs_old), names(soil_old)), all.x = FALSE, all.y = TRUE) %>%
             filter(!is.na(Location_ID)) %>% select(Plot_Name, Year, Event_QAQC,
                                                    Sampling_Position, Sample_Type, Horizon_Type, Archived,
                                                    Sample_Number, Litter_Depth, FF_Depth,
                                                    A_Horizon_Depth, Total_Excavation_Depth, Notes, Comments, Sample_Missed)
names(soil_old2)
# convert NA horizons to 0, but not total
soil_old2[,c(9:11)][is.na(soil_old2[,c(9:11)])]<-0

soilsamp_merge <- merge(soilsamp_wide, soil_old2,
                        by.x = c("Plot_Name", "StartYear", "IsQAQC", "SampleSequence.Code"),
                        by.y = c("Plot_Name", "Year", "Event_QAQC", "Sample_Number"), all = T)
names(soilsamp_merge)

soil_miss <- soilsamp_merge %>% filter(is.na(SQSoil.Code)) # I think these are all okay and will get cleaned up in next migration

check_soils <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", "IsQAQC", "SQSoil.Code", "Sampling_Position", "SampleSequence.Code", col1, col2)]}
  )) %>% bind_rows()
}
names(soilsamp_merge)

check_soils(soilsamp_merge, "Archived", "SoilEvent.IsArchived")

p5 <- soilsamp_merge %>% filter(Litter_Depth == 0.5 |A_Horizon_Depth == 0.5 | FF_Depth == 0.5)

soilsamp_merge <- soilsamp_merge %>% mutate(A_diff = abs(A_Horizon_Depth - A_Horizon),  #%>% filter(A_diff > 0.5) # 2 records due to rounding errors
                                            O_diff = abs(O_Horizon - FF_Depth),
                                            tot_diff = abs(Total_Excavation_Depth - Total_Depth))

check_soils(soilsamp_merge, "Note", "Comments") # no issues
names(soilsamp_merge)

table(soilsamp_merge$SQSoil.Code) # All SS.

# Soil lab data

head(COMN_SoilLab)
head(soillab) # old data
head(soildata2)
names(soillab)
# need to remove records that weren't sampled but had earthworms recorded
soillab_old <- merge(soildata2, soillab[,-c(1, 34, 35)],
                  by = intersect(names(soildata2), names(soillab[,-c(1, 34, 35)])), all = TRUE)
soillab_old2 <- merge(plotevs_old, soillab_old, by = intersect(names(plotevs_old), names(soillab_old)),
                   all.x = FALSE, all.y = TRUE) %>%
  filter(!is.na(Location_ID)) %>% select(Plot_Name, Year, Event_QAQC, Layer, UMO_Sample:ECEC, Notes,
                                         Sampling_Position, Sample_Type, Archived)
head(soillab_old2)

#---------------------------
# There are 2729 records in the NETN soil data to soil sample data query after removing the +++ Soil not sampled notes +++
#
# SELECT tbl_Locations.Unit_ID, tbl_Locations.Plot_Number, tbl_Events.Start_Date, tbl_Events.Event_QAQC,
#        tbl_Soil_Data.Sampling_Position, tbl_Soil_Data.Sample_Type, tbl_Soil_Sample_Data.Sample_Number,
#        tbl_Soil_Sample_Data.Litter_Depth, tbl_Soil_Sample_Data.FF_Depth, tbl_Soil_Sample_Data.A_Horizon_Depth,
#        tbl_Soil_Sample_Data.Total_Excavation_Depth, tbl_Soil_Sample_Data.Comments, tbl_Soil_Sample_Data.Sample_Missed,
#        tbl_Soil_Data.Notes
# FROM (tbl_Locations INNER JOIN tbl_Events ON tbl_Locations.Location_ID = tbl_Events.Location_ID)
# INNER JOIN (tbl_Soil_Data INNER JOIN tbl_Soil_Sample_Data ON tbl_Soil_Data.Soil_Data_ID = tbl_Soil_Sample_Data.Soil_Data_ID) ON
# tbl_Events.Event_ID = tbl_Soil_Data.Event_ID;

#------------------------
# Testing joinSoilLabData
# Function is a bit slow, but willing to let that be

park = 'all'
from = 2016
from = 2007
to = 2019
QAQC = T
locType = 'all'
eventType = 'all'
panels = 1:4
layer = 'all'


soil_test <- joinSoilLabData()
summary(soil_test)
# need to figure out why base sat could be > 100
# may have something to do with weighted averages with 0 for rounded down soil sample depths
# hopefully will be fixed with next migration. For now, I'm converting them to NA in the function.

acad_soil <- joinSoilLabData(park = "ACAD")
sara_soil <- joinSoilLabData(park = "SARA", layer = "A", from = 2010, to = 2019, QAQC = T)
soil2 <- joinSoilLabData(park = "ACAD", from = 2019, to = 2019)

table(sara_soil$ParkUnit)
table(sara_soil$Horizon_QC)

yr4_soil <- joinSoilLabData(from = 2016, to = 2019)


shtest <- joinSoilSampleData()
shtest2 <- joinSoilSampleData(park = 'ACAD', from = 2019, to = 2019)


# ------------------------------
# Joining notes functions

test <- joinVisitNotes() %>% filter(StartDate > "2019-06-01")

head(test)
t2 <- joinVisitNotes(park = "ACAD", from = 2016, to = 2019, QAQC = TRUE)
head(t2)
table(test$IsQAQC)
table(t2$IsQAQC)

t3 <- t2 %>% filter(StartDate > "2019-06-06")
table(t3$StartDate)
table(t2$StartDate)

#---------------------
# sumSpeciesList

test <- sumSpeciesList()
head(test)

head(sumSpeciesList(park = 'MABI', speciesType = "exotic", from = 2019, to = 2019))

#--------------------
# Tree map testing

plotTreeMap(park = "ACAD", from = 2016, to = 2019, output_to = 'view', plotName = "ACAD-101",
            canopyPosition = 'canopy', speciesType = 'exotic')

plotTreeMap(park = "ACAD", from = 2016, to = 2019, output_to = 'view', plotName = "ACAD-101")


plotTreeMap(park = "ACAD", from = 2016, to = 2019, panels = c(3,4), output_to = "file",
                      path = "D:/NETN/Monitoring_Projects/Forest_Health/2021_Data/Maps&Data/NETN_Tree_Maps")

#--------------------
# sum DBH
test <- sumTreeDBHDist()
head(sumTreeDBHDist(units = "BA"))
head(sumTreeDBHDist(units = "dens"))
head(sumTreeDBHDist(units = "both", stauts = 'live'))
head(sumTreeDBHDist(park = "ROVA", units = 'both', canopyPosition = 'canopy'))
head(sumTreeDBHDist(park = "ROVA", units = 'both', speciesType = 'exotic'))

#--------------------
# sumQuadGuilds
test <- sumQuadGuilds()
head(test)
test2 <- sumQuadGuilds(park = "ACAD", speciesType = "invasive")
head(test2)
table(test2$Group, test2$StartYear)
head(sumQuadGuilds(park = "MABI", from = 2016, to = 2019, speciesType = 'native', splitHerb = TRUE))
head(sumQuadGuilds(park = "MABI", from = 2016, to = 2019, speciesType = 'exotic', splitHerb = TRUE))
head(sumQuadGuilds(park = "MABI", from = 2016, to = 2019, speciesType = 'invasive', splitHerb = TRUE))

#-----------------------
# sumSapDBHDist
test <- sumSapDBHDist()
head(sumSapDBHDist(park = 'MORR', speciesType = 'invasive'))
head(sumSapDBHDist(park = 'MORR', speciesType = 'native'))
head(sumSapDBHDist(park = 'MABI', speciesType = 'native', canopyForm = 'canopy'))
head(sumSapDBHDist(park = 'MORR', from = 2006, to = 2010, speciesType = 'invasive'))











