#' Find value improvement for one group
#'
#' Based on the paper 'Divergent Paths: Structural Change, Economic Rank, and the Evolution of Black-White Earnings Differences, 1940-2014', this function finds the 
#' difference in toxicity level for the same percentile in different populations. This is calculated by individually finding the toxicity in each population and 
#' returning the difference. The return is group1 - group 2, and users should be aware that the calls valueDifference(group1, group2, 500) and 
#' valueDifference(group2, group1, 500) will not return the same answer. 
#' @param group1 is meant to be a dataframe with 2 columns. The columns should be the toxicity in a location and the count of the population of interest in 
#' that location. Column names are not important, but columns must be ordered with concentration in the first column and population counts in the second column.
#' @param group2 should have the same format as group1
#' @param value is the percentile you want to find the toxicity difference in.   
#' @importFrom spatstat ewcdf
#' @export
#' @examples valueDifference(group1, group2, .6)
#' valueDifference(data1990[, c(2, 3)], data1990[, c(2, 7)], .99)
valueDifference = function(group1, group2, value) {
  if (value > 1 || value < 0) {
    print("Value must be between 0 and 1.")
    return(NA)
  }
  group1 = as.data.frame(group1)
  group2 = as.data.frame(group2)
  dist1 = ewcdf(group1[,1], weights = group1[,2]/sum(group1[,2]))
  dist2 = ewcdf(group2[,1], weights = group2[,2]/sum(group2[,2]))
  val1 = quantile(dist1, value)
  val2 = quantile(dist2, value)
  return(as.double(val1 - val2))
}