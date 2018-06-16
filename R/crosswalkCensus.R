#' Crosswalk data from 1990/2000 census geographies to 2010 geographies
#'
#' @param crosswalk the crosswalk across the two geographies
#' @param data the dataset you want in 2010 geographies
#' @param mergex default = id00, the column to merge the crosswalk to the dataset
#' @param mergey default = id, the column that you would like to crosswalk to
#' @importFrom stringr str_pad
#' @importFrom plyr ddply
#' @export
#' @examples extrapolate(race_data[race_data$year == year, ], race_data[race_data$year == 1990, ], "black", n = 10)

crosswalkCensus = function(crosswalk, data, mergex = "id00", mergey = "id") {
  #merge the data and the crosswalk
  temp = merge(crosswalk, data, by.x = mergex, by.y = mergey)
  #multiply all the values by the weight of the crosswalk
  n = ncol(crosswalk) + 1
  temp[, n:ncol(temp)] = temp[, n:ncol(temp)] * as.numeric(temp$weight)
  #take all the crosswalked values
  temp = as.data.frame(cbind(temp[, "id10"], temp[, n:ncol(temp)]))
  #group together all the 2010 groups, and add up all the values
  t = temp %>%
    group_by(id10) %>% 
    summarise_all(sum)
  t = as.data.frame(t)
  #give the columns their original names and output
  names(t) = c("id", names(t)[2:ncol(t)])
  return(t)
}