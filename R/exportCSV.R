#' @title exportData: Export Access tables in global enviroment to .csv
#'
#' @description This function exports database tables to .csv that were imported using importData.These
#' .csv files can then be imported via importCSV. This function is primarily for internal use to create flat
#' files for users to run the functions in this package without having to use an odbc driver to import and
#' query NETN data. This is particularly useful for Mac users and those without MS Access installed.
#'
#' @param path Quoted path to save files to.
#'
#' @export

exportCSV<- function(path=NA){
  path<-if(substr(path,nchar(path),nchar(path))!="/"){paste0(path,"/")} else(paste0(path))
  pb = txtProgressBar(min = 0, max = 23, style = 3)
  write.csv(loc, paste0(path,"tbl_Locations.csv"), row.names=F)
  setTxtProgressBar(pb,1)
  write.csv(parknames, paste0(path,"tlu_Park_Names.csv"), row.names=F)
  setTxtProgressBar(pb,2)
  write.csv(event, paste0(path,"tbl_Events.csv"), row.names=F)
  setTxtProgressBar(pb,3)
  write.csv(treedata, paste0(path,"tbl_Tree_Data.csv"), row.names=F)
  setTxtProgressBar(pb,4)
  write.csv(trees, paste0(path,"tbl_Trees.csv"), row.names=F)
  setTxtProgressBar(pb,5)
  write.csv(treecond, paste0(path,"tlu_Tree_Conditions.csv"), row.names=F)
  setTxtProgressBar(pb,6)
  write.csv(xrtreecond, paste0(path,"xref_Tree_Conditions.csv"), row.names=F)
  setTxtProgressBar(pb,7)
  write.csv(cwd, paste0(path,"tbl_CWD_Transect_Data.csv"), row.names=F)
  setTxtProgressBar(pb,8)
  write.csv(plants, paste0(path,"tlu_Plants.csv"), row.names=F)
  setTxtProgressBar(pb,9)
  write.csv(saps, paste0(path,"tbl_Microplot_Sapling_Data.csv"), row.names=F)
  setTxtProgressBar(pb,10)
  write.csv(micro, paste0(path,"tbl_Microplot_Characterization_Data.csv"), row.names=F)
  setTxtProgressBar(pb,11)
  write.csv(sdlg, paste0(path,"tbl_Microplot_Seedling_Data.csv"), row.names=F)
  setTxtProgressBar(pb,12)
  write.csv(shrub, paste0(path,"tbl_Microplot_Shrub_Data.csv"), row.names=F)
  setTxtProgressBar(pb,13)
  write.csv(quadsamp, paste0(path,"tbl_Quadrat_Sampled.csv"), row.names=F)
  setTxtProgressBar(pb,14)
  write.csv(quadchr, paste0(path,"tbl_Quadrat_Character_Data.csv"), row.names=F)
  setTxtProgressBar(pb,15)
  write.csv(quadchrtlu, paste0(path,"tlu_Quadrats.csv"), row.names=F)
  setTxtProgressBar(pb,16)
  write.csv(quads, paste0(path,"tbl_Quadrat_Species_Data.csv"), row.names=F)
  setTxtProgressBar(pb,17)
  write.csv(addspp, paste0(path,"tbl_Plot_Additional_Species.csv"))
  setTxtProgressBar(pb,18)
  write.csv(stand, paste0(path,"tbl_Stand_Data.csv"), row.names=F)
  setTxtProgressBar(pb,19)
  write.csv(stdtlu, paste0(path,"tlu_Stand_Structures.csv"), row.names=F)
  setTxtProgressBar(pb,20)
  write.csv(disturb, paste0(path,"tbl_Disturbances.csv"), row.names=F)
  setTxtProgressBar(pb,21)
  write.csv(disttlu, paste0(path,"tlu_Disturbance_Codes.csv"), row.names=F)
  setTxtProgressBar(pb,22)
  write.csv(disttlutc, paste0(path,"tlu_Disturbance_Threshhold_Codes.csv"), row.names=F)
  setTxtProgressBar(pb,23)
  close(pb)
  noquote('data export complete')
}


