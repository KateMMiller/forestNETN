library(tidyverse)
library(forestNETN)
importData()

source("testing_scripts/quad_wide_sql.R")
con <-  RODBC::odbcDriverConnect(connection = paste0("Driver={SQL Server};server=",
                                                     "localhost", "\\SQLEXPRESS;database=", "NETN_Forest",
                                                     ";trusted_connection=TRUE;ReadOnly=True"),
                                 readOnlyOptimize = TRUE, rows_at_time = 1)

assign("NETN_QuadSpecies_wide",
       RODBC::sqlQuery(con, NETN_QuadSpecies_wide), envir = VIEWS_NETN)
RODBC::odbcClose(con)

quadspp_long <- joinQuadSpecies(shape = 'long') %>%
  select(Plot_Name, PlotID, EventID, StartYear, IsQAQC, num_quads, TSN, ScientificName, quad_avg_cov, quad_pct_freq,
         Pct_Cov_UC:Txt_Cov_UL, Confidence, IsGerminant, Exotic:QuadSppNote)

quadspp_wide <- joinQuadSpecies(shape = 'wide') %>%
  select(Plot_Name, PlotID, EventID, StartYear, IsQAQC, SQQuadSum, TSN, ScientificName, quad_avg_cov, quad_pct_freq,
         Pct_Cov_UC:Txt_Cov_UL, Confidence, IsGerminant, Exotic:QuadSppNote)

quad_check <- full_join(quadspp_long, quadspp_wide,
                        by = c("Plot_Name", "PlotID", "EventID", "StartYear", "IsQAQC", "TSN", "ScientificName", "IsGerminant"),
                        suffix = c("_l", "_w"))

names(quad_check)

check_data <- function(df, col1, col2){
  lapply(1:nrow(df), function(x) (
    if(length(setdiff(union(df[x, col1], df[x, col2]), intersect(df[x, col1], df[x, col2]))) > 0){
      df[x, c("Plot_Name", "StartYear", "IsQAQC", "ScientificName", col1, col2)]}
  )) %>% bind_rows()
}

check_data(quad_check, "SQQuadSum", "num_quads") # Better handling of SQQuadSum in wide version

num_check <- quad_check %>% select(Plot_Name, StartYear, IsQAQC, TSN, ScientificName, SQQuadSum, num_quads,
                                   quad_avg_cov_l, quad_avg_cov_w, QuadSppNote_l, QuadSppNote_w)
names(quad_check)

check_data(quad_check, "quad_avg_cov_l", "quad_avg_cov_w") #0
check_data(quad_check, "quad_pct_freq_l", "quad_pct_freq_w") #0
check_data(quad_check, "Pct_Cov_UC_l", "Pct_Cov_UC_w") # 0
check_data(quad_check, "Pct_Cov_UR_l", "Pct_Cov_UR_w") # 0
check_data(quad_check, "Pct_Cov_MR_l", "Pct_Cov_MR_w") # 0
check_data(quad_check, "Pct_Cov_BR_l", "Pct_Cov_BR_w") # 0
check_data(quad_check, "Pct_Cov_BC_l", "Pct_Cov_BC_w") # 0
check_data(quad_check, "Pct_Cov_BL_l", "Pct_Cov_BL_w") # 0
check_data(quad_check, "Pct_Cov_ML_l", "Pct_Cov_ML_w") # 0
check_data(quad_check, "Pct_Cov_UL_l", "Pct_Cov_UL_w") # 0

check_data(quad_check, "Txt_Cov_UC_l", "Txt_Cov_UC_w") # 0
check_data(quad_check, "Txt_Cov_UR_l", "Txt_Cov_UR_w") # 0
check_data(quad_check, "Txt_Cov_MR_l", "Txt_Cov_MR_w") # 0
check_data(quad_check, "Txt_Cov_BR_l", "Txt_Cov_BR_w") # 0
check_data(quad_check, "Txt_Cov_BC_l", "Txt_Cov_BC_w") # 0
check_data(quad_check, "Txt_Cov_BL_l", "Txt_Cov_BL_w") # 0
check_data(quad_check, "Txt_Cov_ML_l", "Txt_Cov_ML_w") # Not Sampled to NA (since have more conf. in SQs)
check_data(quad_check, "Txt_Cov_UL_l", "Txt_Cov_UL_w") # 0

check_data(quad_check, "QuadSppNote_l", "QuadSppNote_w") # long version lost notes for plots missing cover. Wide version has them.
check_data(quad_check, "Confidence_l", "Confidence_w")
check_data(quad_check, "IsGerminant_l", "IsGerminant_w")

# Taxa checks show that wide version is better/easier to prevent NAs
check_data(quad_check, "InvasiveNETN_l", "InvasiveNETN_w")
check_data(quad_check, "Tree_l", "Tree_w")

names(quad_check)

table(VIEWS_NETN$NETN_QuadSpecies_wide$ConfidenceClassCode, useNA = 'always')
table(VIEWS_NETN$NETN_QuadSpecies$ConfidenceClassCode, useNA = 'always')

View(quad_check)

quad_na_check <- quad_check %>% filter(is.na(num_quads)) %>%
  select(Plot_Name, ScientificName, num_quads, SQQuadSum,
         quad_avg_cov_l, quad_avg_cov_w, quad_pct_freq_l, quad_pct_freq_w,
         Exotic_l:FernAlly_l, Exotic_w:FernAlly_w)
