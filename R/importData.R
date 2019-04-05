#' @title importData
#'
#' @description This function imports database tables from a named ODBC datasource from
#' NETN or MIDN forest backend databases. You must use the 32-bit version of R to work.
#'
#' @param type Select whether to use the default DSN to import data or a different database
#' \describe{
#' \item{"DSN"}{Default. DSN database. If not specified, will use "NETNFVM" .}
#' \item{"file"}{A different database than default DSN}
#' }
#' @param path Quoted path of database backend file, including the name of the backend.
#' @return Assigns database tables to global environment
#'
#' @export

importData<- function(type=c('DSN','file'), odbc='NETNFVM', path=NA){
  type<-match.arg(type)
  if(!requireNamespace("odbc", quietly = TRUE)){
    stop("Package 'odbc' needed for this function to work. Please install it.", call. = FALSE)
  }
  if(!requireNamespace("DBI", quietly = TRUE)){
    stop("Package 'DBI' needed for this function to work. Please install it.", call. = FALSE)
  }
  pb = txtProgressBar(min = 0, max = 23, style = 3)
  db<- if (type=='DSN'){
        db<- DBI::dbConnect(drv=odbc::odbc(),dsn=odbc)
    }
      else if (type=='file'){
        db<- DBI::dbConnect(drv=odbc::odbc(),
        .connection_string=paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",path))
    }
  assign("loc", DBI::dbReadTable(db, "tbl_Locations"), envir=.GlobalEnv)
  setTxtProgressBar(pb,1)
  assign("parknames",DBI::dbReadTable(db, "tlu_Park_Names"),envir=.GlobalEnv)
  setTxtProgressBar(pb,2)
  assign("event",DBI::dbReadTable(db, "tbl_Events"),envir=.GlobalEnv)
  setTxtProgressBar(pb,3)
  assign("treedata",DBI::dbReadTable(db, "tbl_Tree_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,4)
  assign("trees", DBI::dbReadTable(db, "tbl_Trees"),envir=.GlobalEnv)
  setTxtProgressBar(pb,5)
  assign("treecond",DBI::dbReadTable(db,"tlu_Tree_Conditions"),envir=.GlobalEnv)
  setTxtProgressBar(pb,6)
  assign("xrtreecond",DBI::dbReadTable(db,"xref_Tree_Conditions"),envir=.GlobalEnv)
  setTxtProgressBar(pb,7)
  assign("cwd",DBI::dbReadTable(db,"tbl_CWD_Transect_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,8)
  assign("plants", DBI::dbReadTable(db, "tlu_Plants"),envir=.GlobalEnv)
  setTxtProgressBar(pb,9)
  assign("saps", DBI::dbReadTable(db,"tbl_Microplot_Sapling_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,10)
  assign("micro",DBI::dbReadTable(db,"tbl_Microplot_Characterization_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,11)
  assign("sdlg",DBI::dbReadTable(db,"tbl_Microplot_Seedling_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,12)
  assign("shrub",DBI::dbReadTable(db,"tbl_Microplot_Shrub_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,13)
  assign("quadsamp",DBI::dbReadTable(db,"tbl_Quadrat_Sampled"),envir=.GlobalEnv)
  setTxtProgressBar(pb,14)
  assign("quadchr", DBI::dbReadTable(db,"tbl_Quadrat_Character_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,15)
  assign("quadchrtlu", DBI::dbReadTable(db,"tlu_Quadrats"),envir=.GlobalEnv)
  setTxtProgressBar(pb,16)
  assign("quads", DBI::dbReadTable(db,"tbl_Quadrat_Species_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,17)
  assign("addspp", DBI::dbReadTable(db,"tbl_Plot_Additional_Species"),envir=.GlobalEnv)
  setTxtProgressBar(pb,18)
  assign("stand", DBI::dbReadTable(db,"tbl_Stand_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,19)
  assign("stdtlu", DBI::dbReadTable(db,"tlu_Stand_Structures"),envir=.GlobalEnv)
  setTxtProgressBar(pb,20)
  assign("disturb", DBI::dbReadTable(db,"tbl_Disturbances"),envir=.GlobalEnv)
  setTxtProgressBar(pb,21)
  assign("disttlu", DBI::dbReadTable(db,"tlu_Disturbance_Codes"),envir=.GlobalEnv)
  setTxtProgressBar(pb,22)
  assign("disttlutc", DBI::dbReadTable(db,"tlu_Disturbance_Threshhold_Codes"),envir=.GlobalEnv)
  setTxtProgressBar(pb,23)
  DBI::dbDisconnect(db)
  close(pb)
  noquote('database import complete')
}


