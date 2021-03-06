% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getPercentileData.R
\name{getPercentileData}
\alias{getPercentileData}
\title{Transform data from count of people in group to percentile of toxicity in group}
\usage{
getPercentileData(data)
}
\arguments{
\item{data}{is the dataset to transform. The data must contain a column named 'concentration' which the percentile rankings will be
based off.}
}
\value{
Returns the same dataset, with the same column names, but adjusted so the columns are the corresponding percentile, rather than a count.
}
\description{
Transform data from count of people in group to percentile of toxicity in group
}
\examples{
getPercentileData(toxic_1990)
}
