library(forestNETN)
library(tidyverse)
library(data.table)
library(xlsx)

#------ ACAD-106 Data Recovery -----
# Must first restore backup containing the missing data via SMSS and name it NETN_106_recov
importData(instance = "local", server = "localhost", name = "NETN_106_recov")
path = "D:/NETN/Monitoring_Projects/Forest_Health/2021_data/weekly_databases/recovery_exports"
exportCSV(path = path, zip = T) #renamed NETN_Forest_106_20210719.zip
file.rename(paste0(path, "/NETN_Forest_20210719.zip"), paste0(path, "/NETN_Forest_106_20210719.zip"))

args <- list(park = "ACAD", from = 2021, to = 2021, QAQC = FALSE)

quad_data_106 <- do.call(joinQuadData, c(args, valueType = "classes")) %>%
  filter(Plot_Name == "ACAD-106") %>% select(Plot_Name, StartDate, StartYear, IsQAQC, CharacterLabel,
                                             num_quads, num_trampled, Txt_Cov_UC:Txt_Cov_UL)

quads_full <- names(quad_data_106[,9:16])
quads_short <- substr(quads_full, 9, 10)
setnames(quad_data_106, old = quads_full, new = quads_short)

quad_spp_106 <- do.call(joinQuadSpecies, c(args, valueType = "classes")) %>%
  filter(Plot_Name == "ACAD-106") %>% select(Plot_Name, StartDate, StartYear, IsQAQC, SQQuadSppCode,
                                             ScientificName, Confidence, IsGerminant, Txt_Cov_UC:Txt_Cov_UL,
                                             QuadSppNote)

setnames(quad_spp_106, old = quads_full, new = quads_short)

# quad_notes_106 <- do.call(joinQuadNotes, args) %>%
#   filter(Plot_Name == "ACAD-106") # No quad notes

micro_seeds_106 <- do.call(joinMicroSeedlings, args) %>%
  filter(Plot_Name == "ACAD-106") %>% select(Plot_Name, StartDate, StartYear, IsQAQC, SQSeedlingCode,
                                             MicroplotCode, ScientificName, sd_15_30cm:tot_seeds)

#------ ACAD-159 Data Recovery -----
# Must first restore backup containing the missing data via SMSS and name it NETN_159_recov
importData(instance = "local", server = "localhost", name = "NETN_159_recov")
path = "D:/NETN/Monitoring_Projects/Forest_Health/2021_data/weekly_databases/recovery_exports"
exportCSV(path = path, zip = T)
file.rename(paste0(path, "/NETN_Forest_20210719.zip"), paste0(path, "/NETN_Forest_159_20210719.zip"))

args <- list(park = "ACAD", from = 2021, to = 2021, QAQC = FALSE)

quad_data_159 <- do.call(joinQuadData, c(args, valueType = "classes")) %>%
  filter(Plot_Name == "ACAD-159") %>% select(Plot_Name, StartDate, StartYear, IsQAQC, CharacterLabel,
                                             num_quads, num_trampled, Txt_Cov_UC:Txt_Cov_UL)

quads_full <- names(quad_data_159[,9:16])
quads_short <- substr(quads_full, 9, 10)
setnames(quad_data_159, old = quads_full, new = quads_short)

quad_spp_159 <- do.call(joinQuadSpecies, c(args, valueType = "classes")) %>%
  filter(Plot_Name == "ACAD-159") %>% select(Plot_Name, StartDate, StartYear, IsQAQC, SQQuadSppCode,
                                             ScientificName, Confidence, IsGerminant, Txt_Cov_UC:Txt_Cov_UL,
                                             QuadSppNote)

setnames(quad_spp_159, old = quads_full, new = quads_short)

# quad_notes_159 <- do.call(joinQuadNotes, args) %>%
#   filter(Plot_Name == "ACAD-159") # No quad notes

micro_seeds_159 <- do.call(joinMicroSeedlings, args) %>%
  filter(Plot_Name == "ACAD-159") %>% select(Plot_Name, StartDate, StartYear, IsQAQC, SQSeedlingCode,
                                             MicroplotCode, ScientificName, sd_15_30cm:tot_seeds)


# Output all data.frames to tabbed spreadsheets for easier entry
file106 <- paste0(path, "/NETN_forest_data_recovery_ACAD-106.xlsx")
write.xlsx(quad_data_106, file106, sheetName = "106_QD", row.names = FALSE)
write.xlsx(quad_spp_106, file106, sheetName = "106_QS", row.names = FALSE, append = TRUE)
#write.xlsx(quad_notes_106, file, sheetName = "106_QN", row.names = FALSE, append = TRUE) # No quad notes, so can't append
write.xlsx(micro_seeds_106, file106, sheetName = "106_MS", row.names = FALSE, append = TRUE)

file159 <- paste0(path, "/NETN_forest_data_recovery_ACAD-159.xlsx")
write.xlsx(quad_data_159, file159, sheetName = "159_QD", row.names = FALSE)
write.xlsx(quad_spp_159, file159, sheetName = "159_QS", row.names = FALSE, append = TRUE)
#write.xlsx(quad_notes_159, file, sheetName = "106_QN", row.names = FALSE, append = TRUE) # No quad notes, so can't append
write.xlsx(micro_seeds_159, file159, sheetName = "159_MS", row.names = FALSE, append = TRUE)


