#' Create non-parametric estimate of what distributions would have been, given percentile placement stayed the same
#'
#' This function allows you to get an estimate of the toxicity distribution for a given group, given their placement in the overall distribution had stayed the same over time.  
#' @param dataframe This is the dataframe that contains your data. Each entry should be already be in census geography, and should contain the column 'block', 'black' and 'white'. Columns must be named as such.
#' @param old This is the dataframe for the original year you'd like to extrapolate from.
#' @importFrom stringr str_pad
#' @importFrom plyr ddply
#' @export
#' @examples nonParametricExtrapolate(t2010, t1990)
#' 
nonParametricExtrapolate = function(year, old) {  
  #sampling white and black individuals with probability equal to their allocations in time = t
  n_pop = sum(year$tot_pop)
  w_pop = sum(year$white)
  sample = rbinom(n_pop/10000, 1, w_pop/n_pop) #so 1 represents white
  #drawing a concentration for each of the sampled individuals, now we have two collections of concentrations at t = 0
  w_samp = sample(old$lconcentration, sum(sample == 1), replace = TRUE, prob = old$white)
  b_samp = sample(old$lconcentration, sum(sample == 0), replace = TRUE, prob = old$black)
  #finding the percentile in the overall distribution of each concentration at t = 0
  density_0 = ewcdf(old$lconcentration, weights = old$tot_pop/sum(old$tot_pop))
  w_perc = density_0(w_samp)
  b_perc = density_0(b_samp)
  boxplot(w_perc, b_perc) #plotting the percentiles for each group
  #sampling the concentration value for the drawn percentile at time = t
  density_cur = ewcdf(year$lconcentration, weights = year$tot_pop/sum(year$tot_pop))
  w_est = quantile(density_cur, w_perc)
  b_est = quantile(density_cur, b_perc)
  w_tru = sample(year$lconcentration, sum(sample == 1), replace = TRUE, prob = year$white)
  b_tru = sample(year$lconcentration, sum(sample == 0), replace = TRUE, prob = year$black)
  #plotting the true time = t distribution, as well as the modeled time = t distribution
  ggplot() + stat_ecdf(aes(w_est), color = "red") + stat_ecdf(aes(b_est), color = "blue") + stat_ecdf(aes(w_tru), color = "orange") + stat_ecdf(aes(b_tru), color = "green") + xlim(-4, 14) 
  ggplot() + geom_density(aes(w_tru), color = "orange", fill = "orange", alpha = 0.2) + geom_density(aes(b_tru), color = "green", fill = "green", alpha = 0.2) + xlim(-2, 14) + geom_density(aes(w_est), color = "red") + geom_density(aes(b_est), color = "blue")
  return(c(w_est, b_est, w_tru, b_tru))
}