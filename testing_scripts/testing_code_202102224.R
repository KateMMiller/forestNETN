#------------------------
# Code for testing and retooling forest database
#------------------------

library(forestNETN)
library(tidyverse)
importData()

# microbenchmark::microbenchmark(importData(),
#                                importCSV("C:/Forest_Health/exports/NETN"),
#                                times = 1) #importCSV is 4+ times faster

# microbenchmark::microbenchmark(importData(),
#                                importData(server = "INPNETN-078644"),
#                                times = 1) #no difference; not surprising


#------------------------
# Plot and Visit data views
#------------------------
# Comparing full plots events join from views with arch db.
plot_events <- joinLocEvent(park = 'all', from = 2006, to = 2019, QAQC = TRUE, panels = 1:4,
                            locType = 'all', eventType = 'all', abandoned = T)

pe_testing <- joinLocEvent(park = "all")

pe_testing <- joinLocEvent(park = c("ACAD"))
pe_testing <- joinLocEvent(park = c("ACAD", "MABI"))
table(pe_testing$Park.Unit)
pe_testing <- joinLocEvent(park = c("ACAD", "MABI"), from = 2015, to = 2021)
table(pe_testing$Event.StartYear)
pe_testing <- joinLocEvent(QAQC = F)
table(pe_testing$Event.IsQAQC)
pe_testing <- joinLocEvent(locType = "all")
table(pe_testing$PlotType.Code)
pe_testing$Plot_Name[pe_testing$PlotType.Code == 'Non-VS'] # all MORR-014. good.
pe_testing <- joinLocEvent(eventType = 'complete') #removes ACAD-029. good.
nrow(pe_testing)
pe_testing <- joinLocEvent(abandoned = TRUE)
table(pe_testing$Plot.IsAbandoned)

plot_events_old <- read.csv("./testing_csvs/plot_events.csv")

pe_merge <- merge(plot_events, plot_events_old, by.x = c("Event.LegacyID", "Plot_Name"),
                   by.y = c("Event_ID", "Plot_Name"), all.x = T, all.y = T)
names(pe_merge)

# Function to check that the rows in each col. 1 and 2 are identical
# for the pe_merge df. Will return 0 if none, or the values that differ
# check_plotev <- function(col1, col2){
#  setdiff(pe_merge[,col1], pe_merge[,col2])
# }

check_data <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "Event.StartYear", col1, col2)]}
    )) %>% bind_rows()
}

check_data(pe_merge, "Park.SubUnit", "Unit_ID")
check_data(pe_merge,"Plot.xCoordinate", "X_Coord")
check_data(pe_merge,"Plot.yCoordinate", "Y_Coord")
check_data(pe_merge,"Event.StartDate", "Start_Date")
check_data(pe_merge,"Panel.Code", "Panel")
check_data(pe_merge,"Event_QAQC", "Event.IsQAQC")
test <- check_data(pe_merge,"Zone.Code", "UTM_Zone")
test
table(pe_merge$Zone.Code, pe_merge$UTM_Zone)
check_data(pe_merge,"Plot.Orientation", "Orientation")
check_data(pe_merge,"cycle.x", "cycle.y")
check_data(pe_merge,"PlotType.Code", "Loc_Type")
check_data(pe_merge,"Plot.LegacyID", "Location_ID")
check_data(pe_merge,"Plot.Aspect", "Aspect")
check_data(pe_merge,"Physiography.Code", "Physiographic_Class")
plot_check <- unique(pe_merge[, c("Park.Unit", "Plot_Name")])
table(plot_check$Park.Unit) # ACAD is missing 1 plot (ACAD-088)

names(pe_merge)

dir_dif <- check_data(pe_merge,"Plot.Directions", "Directions")
write.csv(dir_dif, "./testing_csvs/Directions_check.csv") # Didn't find any issues to report

#---------------------------
#  Stand data views
#---------------------------
# Views haven't completely settled for this function, so I'm just going to compare raw
# view data to original function to find migration issues via setdiff and table checks
# ---- StandInfoPhotos
stand_old <- read.csv("./testing_csvs/Stand_Data.csv")
stand_new <- get("NETN_StandInfoPhotos", envir = VIEWS_NETN)[, -c(41:46)]
stand_new$Plot_Name <- paste(stand_new$Park.Unit, sprintf("%03d", stand_new$Plot.Code), sep = "-")
stand_new$Event_QAQC <- ifelse(stand_new$Event.IsQAQC == 0, FALSE, TRUE)
st_merge <- merge(stand_new, stand_old, by.x = c("Plot_Name", "Event.StartYear", "Event_QAQC"),
                  by.y = c("Plot_Name", "Year", "Event_QAQC"),
                  all.x = T, all.y = T) %>% filter(Event.StartYear < 2020)


table(complete.cases(st_merge$Event_ID))
table(complete.cases(st_merge$Location_ID))

check_data(st_merge, "Park.Unit", "Unit_Code")
check_data(st_merge,"Park.Unit", "Unit_Code")
check_data(st_merge,"Stand_Structure", "StandStructure.Summary") # 2 plots correctly missing ACAD-029.2010/MIMA-015.2008
check_data(st_merge, "Earthworms", "Earthworm.Code") #NA changed to PM
check_data(st_merge,"Microtopography_ID", "Microtopography.Code") #NA changed to PM
check_data(st_merge,"Panel", "Panel.Code")

table(st_merge$Crown_Closure_ID, st_merge$CrownClosure.Label)
table(st_merge$Event_QAQC, st_merge$Event.IsQAQC)
table(st_merge$Stand_Structure, st_merge$StandStructure.Summary)

#table(st_merge$Pct_Understory_Low, st_merge$)
#---- StandForestFloor
stand2_new <- get("COMN_StandForestFloor", envir = VIEWS_NETN)[, -c(19:23)]
stand2_new$Plot_Name <- paste(stand2_new$Park.Unit, sprintf("%03d", stand2_new$Plot.Code), sep = "-")
stand2_new <- stand2_new[, c("Plot_Name", "Park.Network", "Park.Unit", "Park.SubUnit", "PlotType.Code",
                             "PlotType.Label", "Plot.Code", "Plot.IsAbandoned", "Panel.Code",
                             "Panel.Label", "Event.StartDate", "Event.IsQAQC", "Event.StartYear",
                             "Event.ID", "Plot.ID",
                             #"ForestFloor.Code",
                             "ForestFloor.Label", "CoverClass.Label")]
st_wide <- stand2_new %>% pivot_wider(id_cols = c(Plot_Name:Plot.ID), names_from = ForestFloor.Label,
                                      values_from = CoverClass.Label) %>%
           mutate(Event_QAQC = ifelse(Event.IsQAQC == 0, FALSE, TRUE))
st_wide <- st_wide %>% rename_all(function(x) gsub(" ", "_", x))
st_wide <- st_wide %>% rename_all(function(x) gsub("-", "_", x))

head(st_wide)

st_merge2 <- merge(st_wide, stand_old, by.x = c("Plot_Name", "Event.StartYear", "Event_QAQC"),
                   by.y = c("Plot_Name", "Year", "Event_QAQC"),
                  all.x = T, all.y = T) %>% filter(Event.StartYear <2020)
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
stand3_new <- get("COMN_StandPlantCoverStrata", envir = VIEWS_NETN)[, -c(16:20)]
stand3_new$Plot_Name <- paste(stand3_new$Park.Unit, sprintf("%03d", stand3_new$Plot.Code), sep = "-")
stand3_new$Event_QAQC <- ifelse(stand3_new$Event.IsQAQC == 0, FALSE, TRUE)
names(stand3_new)
head(stand3_new)

st_wide3 <- stand3_new %>% select(-Strata.Code, -Strata.Summary) %>%
  pivot_wider(id_cols = c(Park.Network:Event.StartYear, Event.ID:Event_QAQC),
              names_from = Strata.Label,
              values_from = StandCoverClassID)

st_wide3 <- st_wide3 %>% rename_all(function(x) gsub(" ", "_", x))
st_wide3 <- st_wide3 %>% rename_all(function(x) gsub("-", "_", x))

names(st_wide3)
st_merge3 <- merge(stand_old, st_wide3, by.x = c("Plot_Name", "Year", "Event_QAQC"),
                  by.y = c("Plot_Name", "Event.StartYear", "Event_QAQC"), all.x = T, all.y = T) %>% filter(Year < 2020)

table(st_merge3$Pct_Understory_Low, st_merge3$Ground)
table(st_merge3$Pct_Understory_Mid, st_merge3$Mid_understory)
table(st_merge3$Pct_Understory_High, st_merge3$High_understory)

#---- StandDisturbances
stand_dist <- get("COMN_StandDisturbances", envir = VIEWS_NETN)[,-c(23:29)]
stand_dist$Plot_Name <- paste(stand_dist$Park.Unit, sprintf("%03d", stand_dist$Plot.Code), sep = "-")
stand_dist$Event_QAQC <- ifelse(stand_dist$Event.IsQAQC == 0, FALSE, TRUE)
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

dist_merge <- merge(stand_dist, st_dist_o, by.x = c("Plot_Name", "Event.StartYear", "Event_QAQC", "Disturbance.Code"),
                    by.y = c("Plot_Name", "Year", "Event_QAQC", "Disturbance_Code"),
                     all.x = T, all.y = T) %>%
  filter(Event.StartYear<2020)

dist_merge[which(!complete.cases(dist_merge$Disturbance.Code)),]

check_data(dist_merge, "Disturbance.Code", "Disturbance")
check_data(dist_merge, "Disturbance_Threshold", "Threshold.Code") %>% arrange(Plot_Name)
check_data(dist_merge, "Disturbance_Notes", "DisturbanceEvent.Note")
table(dist_merge$CoverClass.Label, dist_merge$Disturbance_Cover_Class_ID)

#---- StandSlopes
slopes <- get("COMN_StandSlopes", envir = VIEWS_NETN) %>% filter(Event.StartDate < 2020)
slopes$Plot_Name <- paste(slopes$Park.Unit, sprintf("%03d", slopes$Plot.Code), sep = "-")
slopes$Event_QAQC <- ifelse(slopes$Event.IsQAQC == 0, FALSE, TRUE)

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

sl_wide <- slopes %>% select(Plot_Name, Event_QAQC, Event.StartYear,
                             StandEventTransectSlope.Slope, Transect.Code) %>%
           pivot_wider(id_cols = c(Plot_Name:Event.StartYear),
                       names_from = Transect.Code,
                       values_from = StandEventTransectSlope.Slope)

sl_simp <- slopes[,c(1:13, 17, 23, 26, 27)] %>% unique()

sl_merge1 <- merge(sl_simp, sl_wide,
                   by = intersect(names(sl_simp), names(sl_wide)),
                   all.x = T, all.y = T) %>% select(-`NA`)

names(sl_merge1)

sl_merge <- merge(sl_merge1, stand_df, by.x = c("Plot_Name", "Event.StartYear", "Event_QAQC"),
                  by.y = c("Plot_Name", "Year", "Event_QAQC"), all.x = T, all.y = T)

options(scipen = 100)
der_slope <- check_data(sl_merge, "Derived_Plot_Slope", "StandEventQuadratSlope.Slope") %>%
             mutate(diff = Derived_Plot_Slope - StandEventQuadratSlope.Slope) # diff. in rounding
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
tr_ht_vw$Plot_Name <- paste(tr_ht_vw$Park.Unit, sprintf("%03d", tr_ht_vw$Plot.Code), sep = "-")
tr_ht_vw$Event_QAQC <- ifelse(tr_ht_vw$Event.IsQAQC == 0, FALSE, TRUE)
names(tr_ht3)
names(tr_ht_vw)
tree_height_comps <- merge(tr_ht_vw,
                           tr_ht3,
                           by.x = c("Plot_Name", "Event.StartYear", "Event_QAQC", "Tag.Code"),
                           by.y = c("Plot_Name", "Year", "Event_QAQC", "Tree_Number"),
                     all.x = F, all.y = F) %>% select(Plot_Name:Event_QAQC, Stand_Structure_ID, Stunted_Woodland,
                                                      CrownClass.Code,
                                                  CrownClass.Label, CrownClass.Summary,
                                                  Tag.Code, StandTreeCanopyPosition.Height,
                                                  Height_m, Crown, Samp_Num) %>% filter(Event.StartYear < 2020)


table(complete.cases(tree_height_comps$StandTreeCanopyPosition.Height),
        tree_height_comps$Event.StartYear)

trht_comps <- tree_height_comps %>% filter(Event.StartYear > 2010)

check_data(trht_comps, "StandTreeCanopyPosition.Height", "Height_m") %>%
  mutate(diff = StandTreeCanopyPosition.Height - Height_m) %>% filter(abs(diff)>0.1)

table(trht_comps$Crown, trht_comps$CrownClass.Label) #all good
