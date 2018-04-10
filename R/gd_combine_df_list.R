.gd_combine_df_list <- function (df_list) {

  # combines the df in df_list from xx_by functions

  # to collect results
  res <- data.frame()

  for (i in 1:length(df_list)) {
    # the df in each list element
    df <- df_list[[i]]
    # create new df var named as the list element
    df[[names(df_list)[i]]] <- df$Value
    # drop df var Value
    df$Value <- NULL

    # collect results
    if (i == 1) {
      res <- df} else {
        res <- full_join(res, df)}
  }
  # return
  res
}



