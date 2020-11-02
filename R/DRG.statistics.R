#' DRG.statistics
#'
#' This function produces a statistic value feedback
#'
#' @param df Default dataset to use for statistical analysis
#' @param func a function for the type of statistical value to return. "mean" returns
#' the mean value; "median" returns the median; "sd" returns the standard deviation
#' the default is "mean"
#'
#' @return A value of mean, median or standard deviation based on the user input
#' @export
#'
#' @importFrom knitr kable
#'
#' @examples
#' DRG.statistics(DRG_data, mean)
#'
DRG.statistics <- function(df, func = mean) {
  # subtract DRG code from dataset
  newData <- df %>% mutate(DRGcode = substr(DRG.Definition, 1,3))
  result <- newData %>%
    # select only columns of interest
    select(DRGcode, Average.Medicare.Payments)%>%
    # group by variable to generate results
    group_by(DRGcode)%>%
    # summarize all columns by the input function
    summarise(across(.fns = func)) %>%
    # rename the column name to result
    rename(Result = "Average.Medicare.Payments")%>%
    #generate a table with the result
    kable(caption = "Medicare Payment Statistical Analysis")
  return(result)
}
