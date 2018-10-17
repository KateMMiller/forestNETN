#' @title importData
#'
#' @description This function imports database tables from a named ODBC datasource from
#' NETN or MIDN forest backend databases. You must use the 32-bit version of R to work.
#'
#' @param odbc Name of the ODBC datasource
#' @return Assigns database tables to global environment
#'
#' @export

importData<- function(odbc="NETNFVM"){
  pb = txtProgressBar(min = 0, max = 23, style = 3)
  db<- odbcConnect(odbc)
  assign("loc",sqlFetch(db, "tbl_Locations"), envir=.GlobalEnv)
  setTxtProgressBar(pb,1)
  assign("parknames",sqlFetch(db, "tlu_Park_Names"),envir=.GlobalEnv)
  setTxtProgressBar(pb,2)
  assign("event",sqlFetch(db, "tbl_Events"),envir=.GlobalEnv)
  setTxtProgressBar(pb,3)
  assign("treedata",sqlFetch(db, "tbl_Tree_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,4)
  assign("trees", sqlFetch(db, "tbl_Trees"),envir=.GlobalEnv)
  setTxtProgressBar(pb,5)
  assign("treecond",sqlFetch(db,"tlu_Tree_Conditions"),envir=.GlobalEnv)
  setTxtProgressBar(pb,6)
  assign("xrtreecond",sqlFetch(db,"xref_Tree_Conditions"),envir=.GlobalEnv)
  setTxtProgressBar(pb,7)
  assign("cwd",sqlFetch(db,"tbl_CWD_Transect_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,8)
  assign("plants", sqlFetch(db, "tlu_Plants"),envir=.GlobalEnv)
  setTxtProgressBar(pb,9)
  assign("saps", sqlFetch(db,"tbl_Microplot_Sapling_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,10)
  assign("micro",sqlFetch(db,"tbl_Microplot_Characterization_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,11)
  assign("sdlg",sqlFetch(db,"tbl_Microplot_Seedling_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,12)
  assign("shrub",sqlFetch(db,"tbl_Microplot_Shrub_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,13)
  assign("quadsamp",sqlFetch(db,"tbl_Quadrat_Sampled"),envir=.GlobalEnv)
  setTxtProgressBar(pb,14)
  assign("quadchr", sqlFetch(db,"tbl_Quadrat_Character_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,15)
  assign("quadchrtlu", sqlFetch(db,"tlu_Quadrats"),envir=.GlobalEnv)
  setTxtProgressBar(pb,16)
  assign("quads", sqlFetch(db,"tbl_Quadrat_Species_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,17)
  assign("addspp", sqlFetch(db,"tbl_Plot_Additional_Species"),envir=.GlobalEnv)
  setTxtProgressBar(pb,18)
  assign("stand", sqlFetch(db,"tbl_Stand_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,19)
  assign("stdtlu", sqlFetch(db,"tlu_Stand_Structures"),envir=.GlobalEnv)
  setTxtProgressBar(pb,20)
  assign("disturb", sqlFetch(db,"tbl_Disturbances"),envir=.GlobalEnv)
  setTxtProgressBar(pb,21)
  assign("disttlu", sqlFetch(db,"tlu_Disturbance_Codes"),envir=.GlobalEnv)
  setTxtProgressBar(pb,22)
  assign("disttlutc", sqlFetch(db,"tlu_Disturbance_Threshhold_Codes"),envir=.GlobalEnv)
  setTxtProgressBar(pb,23)
  odbcClose(db)
  close(pb)
  noquote('database import complete')
}


