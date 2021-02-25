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
check_plotev <- function(col1, col2){
 setdiff(pe_merge[,col1], pe_merge[,col2])
}


check_plotev("Park.SubUnit", "Unit_ID")
check_plotev("Plot.xCoordinate", "X_Coord")
check_plotev("Plot.yCoordinate", "Y_Coord")
check_plotev("Event.StartDate", "Start_Date")
check_plotev("Panel.Code", "Panel")
check_plotev("Event_QAQC", "Event.IsQAQC")
check_plotev("Zone.Code", "UTM_Zone")
table(pe_merge$Zone.Code, pe_merge$UTM_Zone)
check_plotev("Plot.Orientation", "Orientation")
check_plotev("cycle.x", "cycle.y")
check_plotev("PlotType.Code", "Loc_Type")
check_plotev("Plot.LegacyID", "Location_ID")
check_plotev("Plot.Aspect", "Aspect")
check_plotev("Physiography.Code", "Physiographic_Class")
plot_check <- unique(pe_merge[, c("Park.Unit", "Plot_Name")])
table(plot_check$Park.Unit) # ACAD is missing 1 plot (ACAD-088)


dir_dif <- data.frame(diff = check_plotev("Plot.Directions", "Directions"))
plot_events_t$Directions2 <- plot_events_old$Directions

diff_dir_s <- anti_join(plot_events, plot_events_old, by = c("Plot.Directions" = "Directions")) %>%
              select(Plot_Name, Plot.Directions) %>% unique()

diff_dir_a <- anti_join(plot_events_old, plot_events, by = c("Directions" = "Plot.Directions")) %>%
  select(Plot_Name, Directions) %>% unique()

diff_dir <- merge(diff_dir_a, diff_dir_s, by = "Plot_Name", all.x = T, all.y = T)
write.csv(diff_dir, "./testing_csvs/Directions_check.csv") # Didn't find any issues to report

# Stand data
# Views haven't completely settled for this function, so I'm just going to compare raw
# view data to original function to find migration issues via setdiff and table checks
# ---- StandInfoPhotos
stand_old <- read.csv("./testing_csvs/Stand_Data.csv")
head(stand_old)
names(VIEWS_NETN)
stand_new <- get("NETN_StandInfoPhotos", envir = VIEWS_NETN)[, -c(41:46)]
stand_new$Plot_Name <- paste(stand_new$Park.Unit, sprintf("%03d", stand_new$Plot.Code), sep = "-")
stand_new$Event_QAQC <- ifelse(stand_new$Event.IsQAQC == 0, FALSE, TRUE)
st_merge <- merge(stand_old, stand_new, by.x = c("Plot_Name", "Year", "Event_QAQC"),
                  by.y = c("Plot_Name", "Event.StartYear", "Event_QAQC"), all.x = T, all.y = T) %>% filter(Year < 2020)
head(st_merge)
table(complete.cases(st_merge$Event_ID))
table(complete.cases(st_merge$Location_ID))

check_stand <- function(col1, col2){
  check <- setdiff(st_merge[,col1], st_merge[,col2])
  return(check)
}
?setdiff
check_stand("Park.Unit", "Unit_Code")
check_stand("Stand_Structure", "StandStructure.Summary")
check_stand("Earthworms", "Earthworm.Code")
check_stand("Microtopography_ID", "Microtopography.Code")
check_stand("Panel", "Panel.Code")
check_stand("Panel", "Panel.Code")

table(st_merge$Crown_Closure_ID, st_merge$CrownClosure.Label)
table(st_merge$Event_QAQC, st_merge$Event.IsQAQC)
table(st_merge$Stand_Structure, st_merge$StandStructure.Summary)

#table(st_merge$Pct_Understory_Low, st_merge$)
#---- StandForestFloor
stand2_new <- get("COMN_StandForestFloor", envir = VIEWS_NETN)[, -c(19:23)]
stand2_new$Plot_Name <- paste(stand2_new$Park.Unit, sprintf("%03d", stand2_new$Plot.Code), sep = "-")
head(stand2_new)
names(stand2_new)
stand2_new <- stand2_new[, c("Plot_Name", "Park.Network", "Park.Unit", "Park.SubUnit", "PlotType.Code",
                             "PlotType.Label", "Plot.Code", "Plot.IsAbandoned", "Panel.Code",
                             "Panel.Label", "Event.StartDate", "Event.IsQAQC", "Event.StartYear",
                             "Event.ID", "Plot.ID",
                             #"ForestFloor.Code",
                             "ForestFloor.Label", "CoverClass.Label")]
head(stand2_new)
st_wide <- stand2_new %>% pivot_wider(id_cols = c(Plot_Name:Plot.ID), names_from = ForestFloor.Label,
                                      values_from = CoverClass.Label) %>%
           mutate(Event_QAQC = ifelse(Event.IsQAQC == 0, FALSE, TRUE))
st_wide <- st_wide %>% rename_all(function(x) gsub(" ", "_", x))
st_wide <- st_wide %>% rename_all(function(x) gsub("-", "_", x))

head(st_wide)

st_merge2 <- merge(stand_old, st_wide,  by.x = c("Plot_Name", "Year", "Event_QAQC"),
                   by.y = c("Plot_Name", "Event.StartYear", "Event_QAQC"), all.x = T, all.y = T) %>% filter(Year <2020)
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


