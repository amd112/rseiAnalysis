#' Aggregate Geography to Higher Census Level
#'
#' This function allows you to aggregate the RSEI toxicity data from block level to any higher level. The function can only aggregate up to coarser geographies, and will 
#' @param dataframe This is the dataframe that contains your data. Each entry should be already be in census geography, and should contain the columns 'block', 'concentration' and 'area'. Columns must be named as such.
#' @param level This defines how high you would like to aggregate your data. Options are: 'state', 'county', 'tract', or 'block group'.
#' @importFrom stringr str_pad
#' @importFrom plyr ddply
#' @export
#' @examples aggregateGeography(data, 'state')
#' 
aggregateGeography = function(dataframe, level) {
  if (dim(dataframe)[2] < 3) {
    print("Your dataframe has less than 3 columns. Please try again, include 'block', 'concentration' and 'area'.")
    return(NA)
  }
  #define number of characters for chosen level
  if (!level %in% c("state", "county", "tract", "block group")) {
    print("Parameter 'level' must be: 'state', 'county', 'tract', or 'block group'.")
    return(NA)
  }
  if (level == 'state') {
    num_char = 2
  }
  else if (level == 'county') {
    num_char = 5
  }
  else if (level == "tract") {
    num_char = 11
  }
  else {
    num_char = 12
  }
  

  #check how many digits are given in the block column, and pad out to a census geography so that leading zeros are not lost.
  #not padding means losing states that start with 0. Tract 01-222-555555 drops 0 and becomes (incorrect) county 12-225
  avg_length = mean(nchar(dataframe$block))
  if (avg_length <= 2) {
    dataframe$block = str_pad(dataframe$block, 2, "left", "0")
  } else if (avg_length <= 5) {
    dataframe$block = str_pad(dataframe$block, 5, "left", "0")
  } else if (avg_length <= 11) {
    dataframe$block = str_pad(dataframe$block, 11, "left", "0")
  } else if (avg_length <= 12) {
    dataframe$block = str_pad(dataframe$block, 12, "left", "0")
  } else {
    dataframe$block = str_pad(dataframe$block, 15, "left", "0")
  }
  
  
  #after padding for the leading zeros, remove unnecessary digits at the end
  dataframe$block = substring(dataframe$block, 1, num_char)

  temp = ddply(dataframe, .(block), function(x) data.frame(concentration = weighted.mean(x$concentration, x$area), area = sum(x$area)))
  return(temp)
}