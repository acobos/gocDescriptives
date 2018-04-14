#' Reads MS Access .mdb files
#' 
#' Reads MS Access .mdb files, shows limited info on (non-system) objects, and 
#' allows reading any such object of type TABLE or VIEW.
#' 
#' @param db_file The address of the .mdb file, as \code{character}.
#' @param read_object Optional. Name of the table or view object to be read, 
#' as \code{character}. Defaults to \code{NA}. See Value.
#' 
#' @return Nothing is returned if \code{read_object = NA} (default), but info on 
#' non-system objects is printed. When \code{read_object} is specified, a dataframe is returned.
#' 
#' @examples 
#' \dontrun{
#' # list (non-system) objects in MS Access file "my_data.mdb"
#' gd_access_mdb("my_data.mdb")
#' 
#' # read object "an_object" into dataframe dat
#' dat <- gd_access_mdb("my_data.mdb", read_object = "an_object")
#' }
#' 
#' 
gd_access_mdb <- function(db_file, read_object = NA) {
  
  # to connect with DB
  library(RODBC)
  con <- odbcConnectAccess(db_file)
  
  require(dplyr)
  sqlTables(con, tableType = c("TABLE", "VIEW")) %>%
    select(object = TABLE_NAME, 
           type = TABLE_TYPE) -> obj_info
  
  # list elements that are not "SYSTEM TABLE" 
  if (is.na(read_object)) {
    cat("The following (non-system) objects were found in the database:\n\n")
    print(obj_info)
  }
  else {
    if (read_object %in% obj_info$object) {
      res <- sqlFetch(con, read_object, 
                      stringsAsFactors = FALSE)
      close(con)
      return(res)
    } else {
      cat("read_object was not found.")
    }
  }
  
 
  # to close the connection
  close(con)
  rm(con)
} 



