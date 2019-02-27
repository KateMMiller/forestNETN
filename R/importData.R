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
  pb = txtProgressBar(min = 0, max = 23, style = 3)
  db<- if (type=='DSN'){
        db<- dbConnect(drv=odbc(),dsn=odbc)
    }
      else if (type=='file'){
        db<- dbConnect(drv=odbc(),
        .connection_string=paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",path))
    }
  assign("loc", dbReadTable(db, "tbl_Locations"), envir=.GlobalEnv)
  setTxtProgressBar(pb,1)
  assign("parknames",dbReadTable(db, "tlu_Park_Names"),envir=.GlobalEnv)
  setTxtProgressBar(pb,2)
  assign("event",dbReadTable(db, "tbl_Events"),envir=.GlobalEnv)
  setTxtProgressBar(pb,3)
  assign("treedata",dbReadTable(db, "tbl_Tree_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,4)
  assign("trees", dbReadTable(db, "tbl_Trees"),envir=.GlobalEnv)
  setTxtProgressBar(pb,5)
  assign("treecond",dbReadTable(db,"tlu_Tree_Conditions"),envir=.GlobalEnv)
  setTxtProgressBar(pb,6)
  assign("xrtreecond",dbReadTable(db,"xref_Tree_Conditions"),envir=.GlobalEnv)
  setTxtProgressBar(pb,7)
  assign("cwd",dbReadTable(db,"tbl_CWD_Transect_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,8)
  assign("plants", dbReadTable(db, "tlu_Plants"),envir=.GlobalEnv)
  setTxtProgressBar(pb,9)
  assign("saps", dbReadTable(db,"tbl_Microplot_Sapling_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,10)
  assign("micro",dbReadTable(db,"tbl_Microplot_Characterization_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,11)
  assign("sdlg",dbReadTable(db,"tbl_Microplot_Seedling_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,12)
  assign("shrub",dbReadTable(db,"tbl_Microplot_Shrub_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,13)
  assign("quadsamp",dbReadTable(db,"tbl_Quadrat_Sampled"),envir=.GlobalEnv)
  setTxtProgressBar(pb,14)
  assign("quadchr", dbReadTable(db,"tbl_Quadrat_Character_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,15)
  assign("quadchrtlu", dbReadTable(db,"tlu_Quadrats"),envir=.GlobalEnv)
  setTxtProgressBar(pb,16)
  assign("quads", dbReadTable(db,"tbl_Quadrat_Species_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,17)
  assign("addspp", dbReadTable(db,"tbl_Plot_Additional_Species"),envir=.GlobalEnv)
  setTxtProgressBar(pb,18)
  assign("stand", dbReadTable(db,"tbl_Stand_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,19)
  assign("stdtlu", dbReadTable(db,"tlu_Stand_Structures"),envir=.GlobalEnv)
  setTxtProgressBar(pb,20)
  assign("disturb", dbReadTable(db,"tbl_Disturbances"),envir=.GlobalEnv)
  setTxtProgressBar(pb,21)
  assign("disttlu", dbReadTable(db,"tlu_Disturbance_Codes"),envir=.GlobalEnv)
  setTxtProgressBar(pb,22)
  assign("disttlutc", dbReadTable(db,"tlu_Disturbance_Threshhold_Codes"),envir=.GlobalEnv)
  setTxtProgressBar(pb,23)
  dbDisconnect(db)
  close(pb)
  noquote('database import complete')
}


