adjust_slope_criteria = function (sample_num_for_slopes, samples_per_2s, num_slope,
                                  slope_crit, slope_crit_min, slope_crit_max) {
  if (sample_num_for_slopes %% samples_per_2s == 0){
    if (num_slope == 0){
      slope_crit = slope_crit - slope_crit %/% 16
      return(slope_crit)
    }
  }else {
    return(slope_crit)
  }

}
