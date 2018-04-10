#' Replaces Variable names by their labels in dataframes with descriptive results
#'
#' Replaces variable names by their labels, in dataframes with descriptive 
#' results. For this function to work, it is required that:  
#' 1) the dataframe with descriptive results has a \code{Variable} column, 
#' containing variable names, and  
#' 2) A second dataframe must exist, containing variable names and variable 
#' labels in separate columns (with arbitrary names).
#' Dataframes obtained from the descriptive functions in the 
#' `gocDescriptives` package, always have column \code{Variable} containing
#' variable names, and therefore comply with condition 1 above. Regarding 
#' condition 2, see Details.
#'
#' @param df A dataframe with decriptive results. 
#' @param var_df A second dataframe containing variable names and variable 
#' labels in separate columns.
#' @param var_name \code{character} The name of the column in \code{var_df} that
#' contains variable names. 
#' @param var_label \code{character} The name of the column in \code{var_df} that
#' contains variable labels. 
#' @return A similar dataframe, with variable names replaced by their labels.
#'
#' @examples
#' # Example data
#' set.seed(123) 
#' Sex <- sample(c("Male", "Female"), 100, replace=TRUE)
#' Age <- floor(sample(50 + 10 * rnorm(100)))
#' Pulse <- floor(sample(80 + 10 * rnorm(100)))
#' DM <- sample(c("No", "Type 1", "Type 2"), 100, replace=TRUE)
#' dat <- data.frame(Sex, Age, Pulse, DM, stringsAsFactors = FALSE)
#' 
#' # Example dataframe with vars info
#' vars <- data.frame(v_names = c("Sex", "Age", "Pulse", "DM"),
#'                    v_labels = c(NA, "Age (years)",
#'                                 "Pulse (bpm)", "Diabetes Mellitus"),
#'                    stringsAsFactors = FALSE)
#' 
#'
#' # Describe dataframe dat
#' gd_df(dat) -> results
#' # Substitute Variable names by labels
#' gd_use_labels(df = results,
#'               vars_df = vars,
#'               var_name = "v_names",
#'               var_label = "v_labels")
#'               
#' # Same as above in one step
#' gd_use_labels(df = gd_df(dat),
#'               vars_df = vars,
#'               var_name = "v_names",
#'               var_label = "v_labels")
#'               
#' # Same as above using pipes
#' library(dplyr)
#' dat %>%
#'     gd_df() %>%
#'     gd_use_labels(vars_df = vars,
#'                   var_name = "v_names",
#'                   var_label = "v_labels")
#'
#' @export
gd_use_labels = function (df,
                          vars_df,
                          var_name = "var_name",
                          var_label = "var_label")  {
    
    # input validation ----
    if (class(df)!= "data.frame") stop("df is not a dataframe")
    if (!("Variable" %in% names(df))) stop("df does not have a column Variable")
    if (class(vars_df)!= "data.frame") stop("vars_df is not a dataframe")
    
    if (!(var_name %in% names(vars_df))) 
        stop("var_name VAR not found in vars_df var")
    
    if (!(var_label %in% names(vars_df))) 
        stop("var_label VAR not found in vars_df var")
    
    
    #' @import dplyr
    
    # df with var names (Variable) and labels (Label)
    # unique to ensure no duplications to avoid problems
    # in the left_join below
    vars_df %>%
        select(Variable = var_name, 
               Label = var_label) %>%
        unique() -> df_labels
    
    # left join to df, assumes varnames in Variable 
    df %>%
        left_join(df_labels) %>%
        mutate(Variable = ifelse(!is.na(Label), 
                                  Label, 
                                  Variable)) %>%
        select(-Label) 
}



