#----------------------------------------
# Code for compiling and revising NETN Forest Views
# Written by Kate Miller, 12/02/2021
#----------------------------------------
library(tidyverse)
library(odbc) # for database connection
library(DBI) # for importing view schema
library(dbplyr) #for in_schema
library(RODBC)
library(readr)
library(glue)

library(RODBC)

forestNETN::importData()
table(VIEWS_NETN$COMN_TreesConditions$TreeConditionCode)

# NETN Run saved SQL query using glue readr and RODBC
# local connection:
connect_serv <- "Driver={SQL Server};server=localhost\\SQLEXPRESS;database=NETN_Forest;trusted_connection=TRUE;ReadOnly=True"
con <- RODBC::odbcDriverConnect(connection = connect_serv, readOnlyOptimize = TRUE, rows_at_time = 1)

# read in SQL query
qchar_qry <- read_file("./testing_scripts/sql/QuadCharacter_NETN_wide.sql") %>%
  glue_collapse(sep = "\n") %>%
  glue_sql(.con = con)

qchar_netn_w <- sqlQuery(con, qchar_qry)

write.csv(qchar_netn_w, "./testing_scripts/VIEWS_20211202/NETN_QuadCharacter_new.csv", row.names = F)

qspp_qry <- read_file("./testing_scripts/sql/QuadSpecies_NETN_wide.sql") %>%
  glue_collapse(sep = "\n") %>%
  glue_sql(.con = con)

qspp_netn_w <- sqlQuery(con, qspp_qry)
write.csv(qspp_netn_w, "./testing_scripts/VIEWS_20211202/QuadratSpecies_NETN_wide.csv", row.names = F)


# read in SQL query
qnote_qry <- read_file("./testing_scripts/sql/QuadNotes_NETN.sql") %>%
  glue_collapse(sep = "\n") %>%
  glue_sql(.con = con)

qnotes_netn <- sqlQuery(con, qnote_qry)

# SQL for microseedlings
mseed_qry <- read_file("./testing_scripts/sql/MicroplotSeedlings_NETN_wide.sql") %>%
  glue_collapse(sep = "\n") %>%
  glue_sql(.con = con)

mseed_netn_w <- sqlQuery(con, mseed_qry)

write.csv(mseed_netn_w, "./testing_scripts/VIEWS_20211202/MicroplotSeedlings_NETN_wide.csv", row.names = F)

# trees by event
tree_qry <- read_file("./testing_scripts/sql/TreesByEvent_NETN.sql") %>%
  glue_collapse(sep = "\n") %>%
  glue_sql(.con = con)

tree_netn <- sqlQuery(con, tree_qry)
head(tree_netn)
write.csv(tree_netn, "./testing_scripts/VIEWS_20211202/TreesByEvent_NETN.csv", row.names = F)

# tree conditions
treec_qry <- read_file("./testing_scripts/sql/TreeCondition_NETN_wide.sql") %>%
  glue_collapse(sep = "\n") %>%
  glue_sql(.con = con)

treec_netn <- sqlQuery(con, treec_qry)
write.csv(treec_netn, "./testing_scripts/VIEWS_20211202/TreeCondition_NETN_wide.csv", row.names = F)

# tree foliage conditions
treefc_qry <- read_file("./testing_scripts/sql/TreeFoliageCondition_NETN.sql") %>%
  glue_collapse(sep = "\n") %>%
  glue_sql(.con = con)

treefc_netn <- sqlQuery(con, treefc_qry)
write.csv(treefc_netn, "./testing_scripts/VIEWS_20211202/TreeFoliageCondition_NETN.csv", row.names = F)


RODBC::odbcClose(con)

write.csv(qnotes_netn, "./testing_scripts/VIEWS_20211202/NETN_QuadNotes_new.csv", row.names = F)


# MIDN Run saved SQL query using glue readr and RODBC
# local connection:
connect_serv <- "Driver={SQL Server};server=localhost\\SQLEXPRESS;database=MIDN_Forest;trusted_connection=TRUE;ReadOnly=True"
con <- RODBC::odbcDriverConnect(connection = connect_serv, readOnlyOptimize = TRUE, rows_at_time = 1)

# read in SQL query
qchar_qry <- read_file("./testing_scripts/sql/QuadCharacter_MIDN_wide.sql") %>%
  glue_collapse(sep = "\n") %>%
  glue_sql(.con = con)

qchar_midn_w <- sqlQuery(con, qchar_qry)

qseed_qry <- read_file("./testing_scripts/sql/QuadSeedlings_MIDN_wide.sql") %>%
  glue_collapse(sep = "\n") %>%
  glue_sql(.con = con)

qseed_qry <- sqlQuery(con, qseed_qry)

# tree conditions
treec_qry <- read_file("./testing_scripts/sql/TreeCondition_MIDN_wide.sql") %>%
  glue_collapse(sep = "\n") %>%
  glue_sql(.con = con)

treec_midn <- sqlQuery(con, treec_qry)
write.csv(treec_midn, "./testing_scripts/VIEWS_20211202/MIDN/TreeCondition_MIDN_wide.csv", row.names = F)


RODBC::odbcClose(con)

write.csv(qchar_midn_w, "./testing_scripts/VIEWS_20211202/MIDN/MIDN_QuadCharacter_wide.csv", row.names = F)
write.csv(qseed_qry, "./testing_scripts/VIEWS_20211202/MIDN/MIDN_QuadSeedling_wide.csv", row.names = F)

head(qseed_qry)

#----------------------------
# odbc version
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 #Server = "INP2300SQL03\\NTWKDEV1",
                 Server = "localhost\\SQLEXPRESS",
                 Database = "NETN_Forest", # for connecting to the dev version
                 Trusted_Connection = 'True'
)

dbIsValid(con) # test valid connection

# Import views
view_list <- dbListTables(con, schema = "ANALYSIS")

view_import <- lapply(seq_along(view_list), function(x){
  view <- view_list[x]
  tbl(con, in_schema("ANALYSIS", view))
  return(tbl)
})


view_import <- setNames(view_import, view_list)
VIEWS_NETN <<- new.env()
list2env(view_import, envir = VIEWS_NETN)
taxa <- tbl(con, in_schema("ANALYSIS", "COMN_Taxa"))

field_list <- lapply(seq_along(view_list), function(x){
  dbListFields(conn = con, name = view_list[x], schema = "ANALYSIS")}) %>%
  setNames(view_list) %>% unlist() %>% as.data.frame()

field_list
colnames(field_list) <- "Field_Name"
field_list$View_Name <- gsub("[0-9]" , "", row.names(field_list))
field_list$Field_order <- gsub(".*?([0-9]+).*", "\\1", row.names(field_list))
row.names(field_list) <- NULL
netn_view_list <- field_list[, c("View_Name", "Field_order", "Field_Name")]
head(netn_view_list)

fix_views <- netn_view_list %>% filter(str_detect(Field_Name, "[.]"))
fix_views

write.csv(netn_view_list, "./testing_scripts/NETN_view_list_20211202.csv", row.names = FALSE)

dbDisconnect(con)

# MIDN Views

con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 #Server = "INP2300SQL03\\NTWKDEV1",
                 Server = "localhost\\SQLEXPRESS",
                 Database = "MIDN_Forest", # for connecting to the dev version
                 Trusted_Connection = 'True'
)

dbIsValid(con) # test valid connection

# Import views
view_list <- dbListTables(con, schema = "ANALYSIS")

view_import <- lapply(seq_along(view_list), function(x){
  view <- view_list[x]
  tbl(con, in_schema("ANALYSIS", view))
  return(tbl)
})


view_import <- setNames(view_import, view_list)
VIEWS_MIDN <<- new.env()
list2env(view_import, envir = VIEWS_MIDN)
taxa <- tbl(con, in_schema("ANALYSIS", "COMN_Taxa"))

field_list <- lapply(seq_along(view_list), function(x){
  dbListFields(conn = con, name = view_list[x], schema = "ANALYSIS")}) %>%
  setNames(view_list) %>% unlist() %>% as.data.frame()

field_list
colnames(field_list) <- "Field_Name"
field_list$View_Name <- gsub("[0-9]" , "", row.names(field_list))
field_list$Field_order <- gsub(".*?([0-9]+).*", "\\1", row.names(field_list))
row.names(field_list) <- NULL
midn_view_list <- field_list[, c("View_Name", "Field_order", "Field_Name")]
head(midn_view_list)

fix_views <- midn_view_list %>% filter(str_detect(Field_Name, "[.]"))
fix_views

write.csv(netn_view_list, "./testing_scripts/MIDN_view_list_20211202.csv", row.names = FALSE)

dbDisconnect(con)

forestMIDN::importData()
forestMIDN::exportCSV(path = "./testing_scripts/VIEWS_20211202/MIDN")
