#' @title exportCSV: Export SQL views to .csv
#'
#' @description This function exports NETN_Forest database views either to individual .csv files, or as a zip file
#' with the database name (NETN_Forest), and the date it was zipped. The exported .csv or .zip files can then be imported
#' via importCSV(). This function is primarily for internal use to create flat files that users can import to run
#' functions in this package without having a connection to the database server. This is particularly useful for external
#' users who don't have access to NPS servers.
#'
#' @param path Quoted path to save files to. If not specified, will save to working directory.
#'
#' @param zip Logical. If TRUE, exports a zip file. If FALSE (Default), exports individual csvs.
#'
#' @return NETN database views exported to specified path
#'
#' @examples
#' \dontrun{
#' # RUN FIRST
#' library(forestNETN)
#' importData()
#'
#' # Export csvs to working directory
#' exportCSV()
#'
#' # Export a zip to the path specified
#' exportCSV(path = "C:/Forest_Health/exports/NETN", zip = TRUE)
#'
#' # Export views as .csvs to specified path
#' exportCSV(path = "C:/Forest_Health/exports/NETN")
#'}
#'
#' @export

exportCSV<- function(path = NA, zip = FALSE){

  # Check that suggested package required for this function are installed
  if(!requireNamespace("zip", quietly = TRUE) & zip == TRUE){
    stop("Package 'zip' needed to export to zip file. Please install it.", call. = FALSE)
  }

  options(scipen = 100) # For TSNs

  # Make sure all the views are loaded. If anything is missing, function stops.
  view_list <- c("AdditionalSpecies_NETN", "CWD_NETN", "EventObservers_NETN", "Events_NETN",
                 "MicroplotSaplings_NETN", "MicroplotSaplingsCount_NETN", "MicroplotSeedlings_NETN",
                 "MicroplotShrubs_NETN", "Plots_NETN", "QuadCharacter_NETN", "QuadNotes_NETN",
                 "QuadSpecies_NETN", "SoilHeader_NETN", "SoilLab_NETN", "SoilSample_NETN",
                 "StandDisturbances_NETN", "StandForestFloor_NETN", "StandInfoPhotos_NETN",
                 "StandPlantCoverStrata_NETN", "StandSlopes_NETN", "StandTreeHeights_NETN",
                 "Taxa_NETN", "TreesByEvent_NETN", "TreesConditions_NETN", "TreesFoliageCond_NETN",
                 "TreesVine_NETN")

  files <- if(exists("VIEWS_NETN")){ls(envir = VIEWS_NETN)} else {ls()}

  missing <- setdiff(view_list, files)

  if(length(missing) > 0 & length(missing) < length(view_list)){
    stop(paste0("Missing the following views: ", paste0(missing, collapse = ", ")))
  } else if (length(missing) == length(view_list)){
    stop("Views were not detected in your workspace. Please import the data first.")}

  # Error handling for path
  if(is.na(path)){path <- getwd()
  print(paste0("No path specified. Output saved to working directory: ", getwd()), quote = FALSE)
  } else if(!dir.exists(path)){
      stop("Specified directory does not exist.")
  } else{print(paste0("Output saving to ", path), quote = FALSE)}

  # Normalize path for zip
  pathn <- normalizePath(path)

  # Add / to end of path if it wasn't specified.
  pathn <- if(substr(pathn, nchar(pathn), nchar(pathn)) != "/"){
    paste0(pathn,"\\")} else {(paste0(pathn))}

  # Set up progress bar
  pb <- txtProgressBar(min = 0, max = length(view_list), style = 3)

  # Set up envir qualifier
  if(exists("VIEWS_NETN")){env = VIEWS_NETN} else {env = .GlobalEnv}

  # Export files
  if(zip == FALSE){
  invisible(lapply(seq_along(view_list), function(x){
    setTxtProgressBar(pb, x)
    print(view_list[[x]])
    write.csv(get(view_list[[x]], envir = env),
              paste0(path, view_list[x], ".csv"),
              row.names = FALSE)
  }))
 } else if(zip == TRUE){ #create tmp dir to export csvs, bundle to zip, then delete tmp folder

   dir.create(tmp <- tempfile())

   invisible(lapply(seq_along(view_list), function(x){
    setTxtProgressBar(pb, x)
    write.csv(get(view_list[[x]], envir = env),
              paste0(tmp, "\\", view_list[x], ".csv"),
              row.names = FALSE)}))

  close(pb)

  file_list <- list.files(tmp)

  zip::zipr(zipfile = paste0(pathn, "NETN_Forest_", format(Sys.Date(), "%Y%m%d"), ".zip"),
            root = tmp,
            files = file_list)
  # csvs will be deleted as soon as R session is closed b/c tempfile
 }
  noquote('Export complete.')
}


