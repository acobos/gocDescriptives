#' Subsets A Dataframe Selecting Variables Of A CRF section.
#'
#' Subsets the dataframe of all study variables, keepingonly those in specified CRF section. For
#' this function to work, the dataframe var must exists, and contain variable names in column
#' VAR, and var labels in column VARIABLE.
#'
#' @param df The dataframe containing all study variables.
#'
#' @param section_name Character. Name of the CRF section.
#'
#' @return A \code{dataframe} with all variables in df belonging to section_name.
#'
#' @examples
#' # See sections defined in column SECCION of the var dataframe.
#' unique(var$SECCION)
#'
#' # Create dataframe demo, containing only variables from the demographyc data section.
#' demo <- gd_subset_section(dat, "Cuestionario ACT")
#' @export
gd_subset_section <- function (df, section_name) {

  var %>%
    filter (SECCION == section_name) %>%
    select (VAR) -> v1

  var_der %>%
    filter (SECCION == section_name) %>%
    select (VAR) -> v2

  bind_rows(v1, v2) %>%
    pull (VAR) -> section_vars

  # # get varnames in section, as character vector
  # var %>%
  #   filter (SECCION == section_name) %>%
  #   pull (VAR)  -> section_vars

  # subset df (section_vars) and describe
  df %>%
    select(one_of(section_vars))
}
