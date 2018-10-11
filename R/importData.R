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
  db<- odbcConnect(odbc)
  assign("loc",sqlFetch(db, "tbl_Locations"), envir=.GlobalEnv)
  assign("parknames",sqlFetch(db, "tlu_Park_Names"),envir=.GlobalEnv)
  assign("event",sqlFetch(db, "tbl_Events"),envir=.GlobalEnv)
  assign("treedata",sqlFetch(db, "tbl_Tree_Data"),envir=.GlobalEnv)
  assign("trees", sqlFetch(db, "tbl_Trees"),envir=.GlobalEnv)
  assign("treecond",sqlFetch(db,"tlu_Tree_Conditions"),envir=.GlobalEnv)
  assign("xrtreecond",sqlFetch(db,"xref_Tree_Conditions"),envir=.GlobalEnv)
  assign("cwd",sqlFetch(db,"tbl_CWD_Transect_Data"),envir=.GlobalEnv)
  assign("plants", sqlFetch(db, "tlu_Plants"),envir=.GlobalEnv)
  assign("saps", sqlFetch(db,"tbl_Microplot_Sapling_Data"),envir=.GlobalEnv)
  assign("micro",sqlFetch(db,"tbl_Microplot_Characterization_Data"),envir=.GlobalEnv)
  assign("sdlg",sqlFetch(db,"tbl_Microplot_Seedling_Data"),envir=.GlobalEnv)
  assign("shrub",sqlFetch(db,"tbl_Microplot_Shrub_Data"),envir=.GlobalEnv)
  assign("quadsamp",sqlFetch(db,"tbl_Quadrat_Sampled"),envir=.GlobalEnv)
  assign("quadchr", sqlFetch(db,"tbl_Quadrat_Character_Data"),envir=.GlobalEnv)
  assign("quadchrtlu", sqlFetch(db,"tlu_Quadrats"),envir=.GlobalEnv)
  assign("quads", sqlFetch(db,"tbl_Quadrat_Species_Data"),envir=.GlobalEnv)
  assign("addspp", sqlFetch(db,"tbl_Plot_Additional_Species"),envir=.GlobalEnv)
  assign("stand", sqlFetch(db,"tbl_Stand_Data"),envir=.GlobalEnv)
  assign("stdtlu", sqlFetch(db,"tlu_Stand_Structures"),envir=.GlobalEnv)
  assign("disturb", sqlFetch(db,"tbl_Disturbances"),envir=.GlobalEnv)
  assign("disttlu", sqlFetch(db,"tlu_Disturbance_Codes"),envir=.GlobalEnv)
  assign("disttlutc", sqlFetch(db,"tlu_Disturbance_Threshhold_Codes"),envir=.GlobalEnv)
  odbcClose(db)
  noquote('database import complete')
}
