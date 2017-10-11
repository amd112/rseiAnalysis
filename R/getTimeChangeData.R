#' Transform data to be vector of rank or value difference across defined time difference
#'
#' @param group1.1 is meant to be a dataframe with 2 columns. The columns should be the toxicity in a location and the count of the population of interest in 
#' that location. Column names are not important, but columns must be ordered with concentration in the first column and population counts in the second column.
#' 1.1 should be the first group at the first time.
#' @param group1.2 should be the same format, but the first group at the second time
#' @param type defines how the function calcualtes the difference, with the options being "rank" and "value". The "rank" option caluclates the difference in 
#' percentile for a specific number. The "value" option calculates the difference in toxicity at a specific percentile. 
#' @param n is an optional parameter, with a default of 500. This defines at how many points the function calcualtes the differences.
#' @return Returns a dataframe with two columns. The first is either 'percentile' or 'value' depending on the type chosen, and the second is the difference between
#' groups over the selected time period at those input values. 
#' @examples getTimeChangeData(t1990[, c("lconc", "hispanic")], t2010[, c("lconc", "hispanic")], t1990[, c("lconc", "white")], t2010[c("lconc", "white")], "value")
#' @export
getTimeChangeData = function(group1.1, group1.2, group2.1, group2.2, type, n = 20) {
  if (type == "value") {
    percentile = seq(0, 0.98, length.out = n)
    difference = sapply(percentile, timeDifference, group1.1, group1.2, group2.1, group2.2, type)
    df = data.frame(percentile, difference)
    return(df)
  }
  else if (type == "rank") {
    tru_min = min(c(min(group1.1[,1]), min(group1.2[,1]), min(group2.1[,1]), min(group2.2[,1])))
    tru_max = max(c(max(group1.1[,1]), max(group1.2[,1]), max(group2.1[,1]), max(group2.2[,1])))
    value = seq(tru_min, tru_max, length.out = n)
    difference = sapply(value, timeDifference, group1.1, group1.2, group2.1, group2.2, type)
    df = data.frame(value, difference)
    return(df)
  }
  else {
    print("Variable 'type' must be either 'rank' or 'value'.")
    return(NA)
  }
}