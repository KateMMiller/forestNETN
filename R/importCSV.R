#' @title importCSV: Import NETN forest data that are formated as .csv files.
#'
#' @description This function imports exported CSV tables from the NETN Forest Database, and
#' assigns the tables to the global environment with names that functions in this package
#' depend on.
#'
#' @param path Quoted path of folder containing tables.
#' @return Assigns database tables to global environment
#'
#' @examples
#' importCSV("C:/Data/forest_csvs")
#'
#' @export

importCSV<- function(path=NA){
  path<-if(substr(path,nchar(path),nchar(path))!="/"){paste0(path,"/")} else(paste0(path))
  pb = txtProgressBar(min = 0, max = 23, style = 3)
  assign("loc", read.csv(paste0(path,"tbl_Locations.csv")), envir=.GlobalEnv)
  setTxtProgressBar(pb,1)
  assign("parknames",read.csv(paste0(path, "tlu_Park_Names.csv")), envir=.GlobalEnv)
  setTxtProgressBar(pb,2)
  assign("event",read.csv(paste0(path, "tbl_Events.csv")), envir=.GlobalEnv)
  setTxtProgressBar(pb,3)
  assign("treedata",read.csv(paste0(path, "tbl_Tree_Data.csv")), envir=.GlobalEnv)
  setTxtProgressBar(pb,4)
  assign("trees", read.csv(paste0(path, "tbl_Trees.csv")), envir=.GlobalEnv)
  setTxtProgressBar(pb,5)
  assign("treecond",read.csv(paste0(path,"tlu_Tree_Conditions.csv")), envir=.GlobalEnv)
  setTxtProgressBar(pb,6)
  assign("xrtreecond",read.csv(paste0(path,"xref_Tree_Conditions.csv")), envir=.GlobalEnv)
  setTxtProgressBar(pb,7)
  assign("cwd",read.csv(paste0(path,"tbl_CWD_Transect_Data.csv")), envir=.GlobalEnv)
  setTxtProgressBar(pb,8)
  assign("plants", read.csv(paste0(path, "tlu_Plants.csv")), envir=.GlobalEnv)
  setTxtProgressBar(pb,9)
  assign("saps", read.csv(paste0(path, "tbl_Microplot_Sapling_Data.csv")), envir=.GlobalEnv)
  setTxtProgressBar(pb,10)
  assign("micro",read.csv(paste0(path, "tbl_Microplot_Characterization_Data.csv")), envir=.GlobalEnv)
  setTxtProgressBar(pb,11)
  assign("sdlg",read.csv(paste0(path, "tbl_Microplot_Seedling_Data.csv")), envir=.GlobalEnv)
  setTxtProgressBar(pb,12)
  assign("shrub",read.csv(paste0(path, "tbl_Microplot_Shrub_Data.csv")), envir=.GlobalEnv)
  setTxtProgressBar(pb,13)
  assign("quadsamp",read.csv(paste0(path,"tbl_Quadrat_Sampled.csv")), envir=.GlobalEnv)
  setTxtProgressBar(pb,14)
  assign("quadchr", read.csv(paste0(path, "tbl_Quadrat_Character_Data.csv")), envir=.GlobalEnv)
  setTxtProgressBar(pb,15)
  assign("quadchrtlu", read.csv(paste0(path, "tlu_Quadrats.csv")), envir=.GlobalEnv)
  setTxtProgressBar(pb,16)
  assign("quads", read.csv(paste0(path, "tbl_Quadrat_Species_Data.csv")), envir=.GlobalEnv)
  setTxtProgressBar(pb,17)
  assign("addspp", read.csv(paste0(path, "tbl_Plot_Additional_Species.csv")), envir=.GlobalEnv)
  setTxtProgressBar(pb,18)
  assign("stand", read.csv(paste0(path, "tbl_Stand_Data.csv")), envir=.GlobalEnv)
  setTxtProgressBar(pb,19)
  assign("stdtlu", read.csv(paste0(path, "tlu_Stand_Structures.csv")), envir=.GlobalEnv)
  setTxtProgressBar(pb,20)
  assign("disturb", read.csv(paste0(path, "tbl_Disturbances.csv")), envir=.GlobalEnv)
  setTxtProgressBar(pb,21)
  assign("disttlu", read.csv(paste0(path, "tlu_Disturbance_Codes.csv")), envir=.GlobalEnv)
  setTxtProgressBar(pb,22)
  assign("disttlutc", read.csv(paste0(path, "tlu_Disturbance_Threshhold_Codes.csv")), envir=.GlobalEnv)
  setTxtProgressBar(pb,23)
  close(pb)
  noquote('data import complete')
}


