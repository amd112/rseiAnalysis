#' Find rank improvement for one group
#'
#' Based on the paper 'Divergent Paths: Structural Change, Economic Rank, and the Evolution of Black-White Earnings Differences, 1940-2014', this function finds the 
#' change in rank for a given value. Aka: for any given toxicity level, and two different populations, this function returns the difference in the percentile for the toxicity.
#' This is calculated by individually finding the toxicity percentile in each population and returning the difference. The return is group1 - group 2, and users should be aware 
#' that the calls rankDifference(group1, group2, 500) and rankDifference(group2, group1, 500) will not return the same answer. 
#' @param group1 is meant to be a dataframe with 2 columns. The columns should be the toxicity in a location and the count of the population of interest in 
#' that location. Column names are not important, but columns must be ordered with concentration in the first column and population counts in the second column.
#' @param group2 should have the same format as group1
#' @param value is the level of toxicity you would like to find the rank difference at.  
#' @return Returns the difference of percentiles that a specific value takes in 2 different distributions, as calculated using group1 - group2
#' @importFrom spatstat ewcdf
#' @export
#' @examples rankDifference(group1, group2, 500)
#' rankDifference(data1990[, c(2, 3)], data1990[, c(2, 7)], 10000)
rankDifference = function(group1, group2, value) {
  group1 = as.data.frame(group1)
  group2 = as.data.frame(group2)
  dist1 = ewcdf(group1[,1], weights = group1[,2]/sum(group1[,2]))
  dist2 = ewcdf(group2[,1], weights = group2[,2]/sum(group2[,2]))
  quant1 = dist1(value)
  quant2 = dist2(value)
  return(quant1 - quant2)
}