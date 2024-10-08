#' @title importCSV: Import NETN forest data that are formatted as .csv files.
#'
#' @description This function imports all views in the ANALYSIS schema of the NETN_Forest backend that have been
#' previously exported as .csvs or a zip file using the exportCSV() function. Each view is added to a VIEWS_NETN
#' environment in your workspace, or to your global environment based on whether new_env = TRUE or FALSE.
#'
#' @param path Quoted path of folder containing tables.
#'
#' @param new_env Logical. Specifies which environment to store views in. If \code{TRUE}(Default), stores
#' views in VIEWS_NETN environment. If \code{FALSE}, stores views in global environment
#'
#' @param zip_name Quoted string ending in .zip. If specified, function looks for the specified file name and
#' imports .csvs from the zip file. If not specified, function looks for and imports individual csvs. Note that
#' this takes slightly longer than loading individual .csvs, due to the unzipping process.
#'
#' @return NETN database views in specified environment
#'
#' @examples
#' \dontrun{
#' # Import individual csvs into global environment
#' importCSV(path = "C:/Forest_Health/exports/NETN", new_env = FALSE)
#'
#' # Import zipped csvs into VIEWS_NETN environment
#' path <- "C:/Forest_Health/exports/NETN"
#' importCSV(path = path, zip_name = "NETN_Forest_20210406.zip")
#' }
#'
#' @export

importCSV<- function(path = NA, new_env = TRUE, zip_name = NA){

  # Error handling for path
  if(is.na(path)){stop("Must specify a path to import csvs.")
  } else if(!dir.exists(path)){stop("Specified path does not exist.")}

  # Add / to end of path if it wasn't specified.
  path <- if(substr(path, nchar(path), nchar(path)) != "/"){paste0(path, "/")} else {(paste0(path))}

  options(scipen = 100) # For TSNs

  view_list <- c("AdditionalSpecies_NETN", "CWD_NETN", "EventObservers_NETN", "Events_NETN",
                 "MicroplotSaplings_NETN", "MicroplotSaplingsCount_NETN", "MicroplotSeedlings_NETN",
                 "MicroplotShrubs_NETN", "Plots_NETN", "QuadCharacter_NETN", "QuadNotes_NETN",
                 "QuadSpecies_NETN", "SoilHeader_NETN", "SoilLab_NETN", "SoilSample_NETN",
                 "StandDisturbances_NETN", "StandForestFloor_NETN", "StandInfoPhotos_NETN",
                 "StandPlantCoverStrata_NETN", "StandSlopes_NETN", "StandTreeHeights_NETN",
                 "Taxa_NETN", "TreesByEvent_NETN", "TreesConditions_NETN", "TreesFoliageCond_NETN",
                 "TreesVine_NETN")

  # Make sure zip file exists and all the views are included
  if(!is.na(zip_name)){
    if(!file.exists(paste0(path, zip_name))){stop("Specified zip file doesn't exist in path.")}}

  # Make sure all the views are in the path or zip file. If anything is mission, function stops.
  files <-
    if(!is.na(zip_name)){
    zfiles1 <- unzip(paste0(path, zip_name), list = TRUE)$Name
    zfiles <- zfiles1[grepl(".csv$", zfiles1)]
    files <- substr(zfiles, 1, nchar(zfiles) - 4)
    } else if(is.na(zip_name)) {
    files <- substr(list.files(path), 1, nchar(list.files(path)) - 4)}

  missing <- setdiff(view_list, files)

  if(length(missing) > 0 & length(missing) < length(view_list)){
    stop(paste0("Missing the following views: ", paste0(missing, collapse = ", ")))
  } else if (length(missing) == length(view_list)){
      stop(paste0("Views were not detected in specified ", ifelse(is.na(zip_name), "path.", "zip file.")))}

  # Since the missing test passed, clean up files so only includes names in view_list, but
  # maintain order in files
  files <- intersect(files, view_list)

  # Import views now that all tests passed
  pb <- txtProgressBar(min = 0, max = length(view_list), style = 3)

  view_import <-
    if(!is.na(zip_name)){
    views1 <- unzip(paste0(path, zip_name), junkpaths = TRUE, exdir = tempdir())
    views <- views1[grepl(".csv$", views1)]
      lapply(seq_along(view_list), function(x){
        setTxtProgressBar(pb,x)
        read.csv(views[x])})
    } else if(is.na(zip_name)){
    lapply(seq_along(view_list), function(x){
      setTxtProgressBar(pb, x)
      read.csv(paste0(path, view_list[x], ".csv"))
      })
    }

  view_import <- setNames(view_import, files)

  if(new_env == TRUE){
    VIEWS_NETN <<- new.env()
    list2env(view_import, envir = VIEWS_NETN)
  } else {
    list2env(view_import, envir = .GlobalEnv)}

  close(pb)

  print(ifelse(new_env == TRUE, paste0("Import complete. Views are located in VIEWS_NETN environment."),
               paste0("Import complete. Views are located in global environment.")), quote = FALSE)

}


