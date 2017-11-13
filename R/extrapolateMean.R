#' Create non-parametric estimate of what distributions would have been, given percentile placement stayed the same
#'
#' This function allows you to estimate the mean toxicity of a group, given their placement in the overall distribution had stayed the same over time.  
#' @param year This is the dataframe that contains your data. Each entry should be already be in census geography, and should contain the column 'concentration'.
#' @param old This is the dataframe for the original year you'd like to extrapolate from. Must also contain "concentration".
#' @param col This is a string with the name of the column that you'd like to extrapolate. Must have same name in both df's
#' @param log Optional, parameter to specify that you'd like to simulate log toxicity. Set to any string for log. 
#' @param n Optional, default set to 10000, that specifies how many times you draw from the toxicities. 
#' @importFrom stringr str_pad
#' @importFrom plyr ddply
#' @export
#' @examples nonParametricExtrapolate(t2010, t1990)
extrapolateMean = function(year, old, col, log = NA, n = 10000) {  
  #sampling white and black individuals with probability equal to their allocations in time = t
  old = old[complete.cases(old), ]
  year = year[complete.cases(year), ]
  tox = "concentration"
  if (!is.na(log)){
    tox = "lconcentration"
    year$lconcentration = log(year$concentration)
    old$lconcentration = log(old$concentration)
  }
  samp = sample(old[, tox], n, replace = TRUE, prob = old[, col])
  density_old = ewcdf(old[, tox], weights = (old$pop)/sum(old$pop, na.rm = TRUE))
  perc = density_old(samp)
  #sampling the concentration value for the drawn percentile at time = t
  density_cur = ewcdf(year[, tox], weights = (year$pop)/sum(year$pop, na.rm = TRUE))
  est = quantile(density_cur, perc)
  return(est)
}