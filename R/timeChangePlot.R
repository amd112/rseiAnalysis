#' Create a plot showing the improvement of groups relataive to the total population
#'
#' @param data1 is a full dataframe with data on toxicity concentration and population counts. The dataframe must contain the variables: 'lconcentration' and the count of total population 'tot_pop'
#' @param data2 is the same format of data, but at a second time period, over which you are interested in the changes
#' @param values is a vector of strings refering to the columns you would like to see the relative changes in. All strings must match to a column in the dataframe. 
#' @param type should be either "rank" or "value". 
#' @param grain defines how fine grain you want the calulation to be. No input will default to 100 calculations
#' @return Returns a plot object, showing the differences in changes in the groups, that can be saved or directly displayed. Returns a positive 'value' difference if group 1 improved more than group 2 
#' over the interest period.Returns a negative 'rank' difference if group 1 improved more than group 2 in the defined time period.
#' @examples timeChangePlot(t1990, t2010, c("hispanic", "white", "black"), "value", 30)
#' @export
timeChangePlot = function(data1, data2, values, type, grain = 20) {
  for (i in values) {
    if (!i %in% names(data1) || !i %in% names(data2)) {
      print("All passed strings must match to a column in data.")
      return(NA)
    }
  }
  if(type == "rank") {
    x_var = "value"
    x_lab = "log toxicity value"
    y_var = "relative rank/percentile change"
  } else if (type == "value") {
    x_var = "percentile"
    x_lab = "toxicity percentile"
    y_var = "relative toxicity change"
  } else {
    print("Variable 'type' must be either 'rank' or 'value.")
    return(NA)
  }
  #----------------------
  i = values[1]
  df = getTimeChangeData(data1[, c("lconcentration", i)], data2[, c("lconcentration", i)], data1[, c("lconcentration", "tot_pop")], data2[, c("lconcentration", "tot_pop")], type, grain)
  for (i in values[-1]) {
    temp = getTimeChangeData(data1[, c("lconcentration", i)], data2[, c("lconcentration", i)], data1[, c("lconcentration", "tot_pop")], data2[, c("lconcentration", "tot_pop")], type, grain)
    df = merge(df, temp, by = x_var, all = TRUE)
  }
  names(df) = c(x_var, values)
  g = ggplot(data = df) + theme(legend.position = c(0.15, 0.9)) + labs(color='Legend') + geom_hline(yintercept = 0, color = "grey65") + 
    labs(title = "Group distributional changes in log toxicity as compared to population", y = y_var, x = x_lab)
  for (i in values) {
    g = g + geom_line(aes_q(x = as.name(x_var), y = as.name(i), color = i))
  }
  return(g)
}