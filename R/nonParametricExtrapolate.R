#' Create non-parametric estimate of what distributions would have been, given percentile placement stayed the same
#'
#' This function allows you to get an estimate of the toxicity distribution for a given group, given their placement in the overall distribution had stayed the same over time.  
#' @param dataframe This is the dataframe that contains your data. Each entry should be already be in census geography, and should contain the column 'block', 'black' and 'white'. Columns must be named as such.
#' @param old This is the dataframe for the original year you'd like to extrapolate from.
#' @importFrom stringr str_pad
#' @importFrom plyr ddply
#' @export
#' @examples nonParametricExtrapolate(t2010, t1990)
nonParametricExtrapolate = function(year, old) {  
  #sampling white and black individuals with probability equal to their allocations in time = t
  year$lconcentration = log(year$concentration)
  old$lconcentration = log(old$concentration)
  w_pop = sum(year$white, na.rm = TRUE)
  n_pop = w_pop + sum(year$black, na.rm = TRUE)
  sample = rbinom(n_pop/10000, 1, w_pop/n_pop) #so 1 represents white
  #drawing a concentration for each of the sampled individuals, now we have two collections of concentrations at t = 0
  w_samp = sample(old$lconcentration, sum(sample == 1), replace = TRUE, prob = old$white)
  b_samp = sample(old$lconcentration, sum(sample == 0), replace = TRUE, prob = old$black)
  #finding the percentile in the overall distribution of each concentration at t = 0
  density_0 = ewcdf(old$lconcentration, weights = old$pop/sum(old$pop))
  w_perc = density_0(w_samp)
  b_perc = density_0(b_samp)
  #sampling the concentration value for the drawn percentile at time = t
  density_cur = ewcdf(year$lconcentration, weights = (year$pop+1)/sum(year$pop, na.rm = TRUE))
  w_est = quantile(density_cur, w_perc)
  b_est = quantile(density_cur, b_perc)
  w_tru = sample(year$lconcentration, sum(sample == 1), replace = TRUE, prob = year$white)
  b_tru = sample(year$lconcentration, sum(sample == 0), replace = TRUE, prob = year$black)
  #plotting the true time = t distribution, as well as the modeled time = t distribution
  ggplot() + geom_density(aes(w_tru), color = "orange", fill = "orange", alpha = 0.2) + geom_density(aes(b_tru), color = "cornflowerblue", fill = "cornflowerblue", alpha = 0.2) + xlim(-2, 14) + geom_density(aes(w_est), color = "red") + geom_density(aes(b_est), color = "blue")
  return(list(w_est, b_est, w_tru, b_tru))
}