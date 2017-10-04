#' Find differences in group improvement over time
#'
#'@param group1.1 is meant to be a dataframe with 2 columns. The columns should be the toxicity in a location and the count of the population of interest in 
#' that location. Column names are not important, but columns must be ordered with concentration in the first column and population counts in the second column.
#' 1.1 should be the first group at the first time.
#' @param group1.2 should be the first group at the second time period.
#' @param group2.1 should be the second group at the first time period.
#' @param group2.2 should be the second group at the first time period. 
#' @param type should be either "rank" or "value", and defines the type of difference calculation being done. 
#' @param value is the level of toxicity you would like to find the rank difference at, or the rank you would like to find the toxicity difference at. 
#' @return Returns the difference of the changes between the two groups. Returns a positive value difference if group 1 improved more than group 2 over the interest period.
#' Returns a negative rank difference if group 1 improved more than group 2 in the defined time period.
#' @examples timeDifference(0.5, t1990[, c("lconc", "hispanic")], t2010[, c("lconc", "hispanic")], t1990[, c("lconc", "white")], t2010[c("lconc", "white")], "value") 
timeDifference = function(value, group1.1, group1.2, group2.1, group2.2, type) {
  if (type == "rank") {
    group1 = rankDifference(group1.1, group1.2, value) #returns - if improved (higher rank = better, so if group1.2 > group1.1 = improved = - return)
    group2 = rankDifference(group2.1, group2.2, value)
    return(group1 - group2) #if group 1 more negative = move improved. So negative return means group 1 is better
  }
  else if (type == "value") {
    if (value > 1 || value < 0) {
      print("For value calcualtions, the value entered should be between 0 and 1.")
      return(NA)
    }
    group1 = valueDifference(group1.1, group1.2, value) #returns + if improved (lower conc = better, so if group1.2 < group1.1 = improved = + return)
    group2 = valueDifference(group2.1, group2.2, value)
    return(group1 - group2) #if group 1 more positive = move improved. So positive return means group 1 is better
  }
  else {
    print("Type must be either 'rank' or 'value'.")
    return(NA)
  }
}