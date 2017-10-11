#' Transform data from count of people in group to percentile of toxicity in group
#'
#' @param data is the dataset to transform. The data must contain a column named 'concentration' which the percentile rankings will be
#' based off.
#' @return Returns the same dataset, with the same column names, but adjusted so the columns are the corresponding percentile, rather than a count.
#' @examples getPercentileData(toxic_1990)
#' @export
getPercentileData = function(data) {
  if (!"concentration" %in% names(data)){
    print("Data must have 'concentration' column.")
  }
  data = data[order(data$concentration), ]
  columns = names(data)
  for (i in columns) {
    if (i == "concentration" || i == "block" || i == "lconcentration") {
      next
    }
    data[, i] = with(data, cumsum(data[, i]) / sum(data[, i]))
  }
  data$lconcentration = log(data$concentration)
  return (data)
}