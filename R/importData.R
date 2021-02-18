#' @title importData: Import tables directly from NETN forest database
#'
#' @description This function imports all views in the ANALYSIS schema of the local instance of the
#' NETN_Forest backend. Each view is added to a VIEWS environment in your workspace, or to your
#' global environment based on whether new_env = TRUE or FALSE.
#'
#' @param server Quoted name of the server to connect to, either your computer name or "localhost" (default).
#'
#' @param instance Specify whether you are connecting to the local instance or server.
#' \describe{
#' \item{"local"}{Default. Connects to local install of backend database}
#' \item{"server"}{Connects to main backend on server. Note that you must have permission to access the server, and
#' connection speeds are likely to be much slower than the local instance.}}
#'
#' @param new_env Specify which environment to store views in
#' \describe{
#' \item{TRUE}{Default. Stores views in VIEWS environment}
#' \item{FALSE}{Stores views in global environment}
#'}
#' @examples
#' # Import using default settings that specify server as localhost and add VIEWS environment
#' importData()
#'
#' # Import using computer name (# should have real numbers)
#' importData(server = "INPNETN-######", new_env = TRUE)
#'
#' # Import to main database on server
#' importData(server = "INP2300VTSQL16\\IRMADEV1", instance = "server", new_env = TRUE)
#'
#' @export


importData <- function(server = "localhost", instance = c("local", "server"), new_env = TRUE){

  instance <- match.arg(instance)

  # Check that suggested package required for this function are installed
  if(!requireNamespace("RODBC", quietly = TRUE)){
    stop("Package 'RODBC' needed for this function to work. Please install it.", call. = FALSE)
  }

  #server = "localhost"
  connect <- if(instance == 'local'){
    paste0("Driver={SQL Server};server=", server, "\\SQLEXPRESS;database=NETN_Forest;trusted_connection=TRUE;")
  } else if (instance == 'server'){
    paste0("Driver={SQL Server};server=", server, ";database=NETN_Forest;trusted_connection=TRUE;")
    }

  #Setup progress bar
  pb <- txtProgressBar(min = 0, max = 22, style = 3, width = 88) #22 Views

  # Test connection. Stop if connection fails.
  con <- tryCatch({RODBC::odbcDriverConnect(connection = connect)},
                   error = function(e){e$message <-
                     paste0(e$message, "\n", "Unable to connect to SQL Server. ", "Check that you have a local instance of ", "\n",
                     "NETN_Forest installed, or use importCSV() to import Views as .csvs")
                     stop(e)},
                  finally = function(e){e$message <- paste0("Importing NETN_Forest views")})

  # Fetch names of views
  view_list <- as.vector(RODBC::sqlTables(con, schema = "Analysis")$TABLE_NAME)

  # Import views using names and show progress bar
  view_import <- lapply(seq_along(view_list), function(x){
    RODBC::sqlQuery(con, paste0("SELECT * FROM ", "[NETN_Forest].[ANALYSIS].[", x, "]"))
    setTxtProgressBar(pb, x)
    }
    )

  close(pb)
  view_import <- setNames(view_import, view_list)
  RODBC::odbcClose(con)

  if(new_env == TRUE){
    VIEWS <<- new.env()
    list2env(view_import, envir = VIEWS)
  } else {
    list2env(view_import, envir = .GlobalEnv)}

  print(ifelse(new_env == TRUE, paste0("Import complete. Views are located in VIEWS environment."),
                                paste0("Import complete. Views are located in global environment.")))
  }

