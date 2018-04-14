#' Writes a script to define factors for coded variables in a dataframe
#' 
#' Writes a script to define factors for coded variables in a dataframe, from 
#' relevant info on the coding dictionaries, contained in auxiliar dataframes. 
#' The arguments of this function have defaults that assuming a particular names
#' of relevant objects.   
#' 
#' @param out_file File name for the script that will be written, as 
#' \code{character}. 
#' @param dat_df The \code{dataframe} with coded variables that need to be redefined as 
#' factors, according to a coding dictionary. Defaults to \code{dat}.
#' @param var_df A \code{dataframe} containing info on variables 
#' in \code{dat_df} and corresponding coding dictionaries. Defaults to 
#' \code{var}.
#' @param var_name The \code{character} name of the column in \code{var_df} 
#' containing the name of variables in \code{dat_df}. Defaults to 
#' \code{"var_name"}.
#' @param var_dictionary The \code{character} name of the column in 
#' \code{var_df} containing the name of the coding dictionary of coded 
#' variables. Defaults to \code{"dictionary"}. 
#' @param dic_df The \code{dataframe} containing coding dictionaries. Defaults 
#' to \code{dic}.
#' @param dic_dictionary The \code{character} name of the column in 
#' \code{dic_df} containing the name of dictionaries. Defaults to 
#' \code{"dictionary"}.
#' @param dic_value The \code{character} name of the column in \code{dic_df} 
#' containing the codes. Defaults to \code{"value"}. 
#' @param dic_value The \code{character} name of the column in \code{dic_df} 
#' containing the decodes or descriptors. Defaults to \code{"value_label"}.  
#'  
#' @return The function returns NULL, but has side effects: it writes file 
#' \code{out_file} containing the R script necessary to define factors for 
#' coded variables in \code{dat_df}.
#'    
#' @examples 
#' \dontrun{
#' # writes definitions into file "factor_definitions.R"
#' gd_write_factor_defs("factor_definitions.R")
#' }
#' 
#' 
#' @export       
gd_write_factor_defs <- function (out_file,
                            dat_df = dat,
                            var_df = var,  
                            var_name = "var_name",
                            var_dictionary = "dictionary",
                            dic_df = dic,
                            dic_dictionary = "dictionary", 
                            dic_value ="value", 
                            dic_label = "value_label") {
  
  # identifies vars with dictionary
  have_dic <- var_df[!is.na(var_df[[var_dictionary]]), var_name] 
  
  # to collect results 
  result <- character()
  
  # write factor definitions for vars with dictionary
  for (i in have_dic) {
    
    dicnum <- as.character(var_df[var_df[[var_name]] == i, var_dictionary])
    vdic <- as.data.frame(dic_df[as.character(dic_df[[dic_dictionary]]) == dicnum,])
    levels <-  paste0("c(", paste0(vdic[[dic_value]], collapse=", "), ")")
    labels <-  paste0("c(", paste0("'", vdic[[dic_label]], "'", collapse=", "), ")")
    
    var_vector <- paste0(deparse(substitute(dat_df)), "$", i)
    factor_def <- paste0(var_vector, 
                         " <- factor(", var_vector, ", \n\t",
                         "levels = ", levels,  ", \n\t",
                         "labels = ", labels, ")") 

    result <- c(result, factor_def)
  }
  
  # writes output file with script
  if (out_file %in% dir()) {
    cat("out_file already exists: provide different filename.")
  } else {
    write(result, out_file) -> k
    cat("out_file was created.")
  }
  write(result, out_file)
  
}
