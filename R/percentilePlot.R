#' Create a plot of the cdf's of the log toxicity of various groups.
#'
#' @param data is a list of dataframes. The list must be at least 1 long. Each of the dataframes must contain a column "concentration", and 
#' will be the data for various groups at different times. 
#' @param value is a list of the columns that you would like plotted. All dataframes must contain all values included here. For example, 
#' if you want to plot the hispanic distributions in 1990 and 2000 -> percentilePlot(c(t1990, t2000), c("hispanic")). Comparitively, if 
#' you want to plot hispanic in 1990, and asian in 2000, percentilePlot(c(t1990, t2000), c("hispanic", "asian")) will plot both groups in 
#' both years, and throw an error if any of those columns don't exist.
#' @param log if log == "l", then it will plot the logged toxicity, if log == "", it will plot normal toxicity
#' @import ggplot2
#' @examples percentilePlot(list(toxic_1990), c("hispanic", "white"))
percentilePlot = function(data, value, log = "l", title = NA) {
  for (i in 1:length(data)) {
    cur = data[i][[1]]
    if (!"concentration" %in% names(cur)) {
      print(paste0("No 'concentration' column in the ", i, "st/nd/th dataframe."))
      return(NA)
    }
    cur = cur[, c(value, "concentration")]
    data[i][[1]] = getPercentileData(data.frame(cur))
  }
  if (is.na(title)) {
    title = "CDF of concentration of toxic chemicals"
  }
  
  g = ggplot() + labs(title = title, y = "percentile", x = "log toxicity concentration") +
                  theme(legend.position = c(0.2, 0.8)) + labs(color='Legend')
  if (log == "l") {
    g = g + xlim(-5, 15)
  }
  
  for (i in 1:length(data)) {
    cur = data.frame(data[i][[1]])
    for (j in value) {
      cur_sub = cur[, c(paste0(log, "concentration"), j)]
      g = g + geom_line(data = cur_sub, aes_q(x = as.name(paste0(log, "concentration")), y = as.name(j), color = j))
    }
  }
  g
}
