#' Internal function which increases or decreases the slope criteria acting as a
#' threshold every two seconds since the end of the previous qrs complex detected
#' when either to many or none slope changes are detected.
#'
#' @param samples_since_qrs_start samples since the previous qrs complex was detected.
#' @param samples_per_2s constant value of the number of samples contained in two seconds.
#' @param num_slope number of slope changes detected since the previous qrs complex was identified.
#' @param slope_crit threshold used agaist which to compare the filter of the signal.
#' @param slope_crit_min constant value of the minimum possible value for the slope_crit
#' @param slope_crit_max constant value of the maximum possible value for the slope_crit
#'
#' @return New value for the slope criteria

adjust_slope_criteria = function (samples_since_qrs_start, samples_per_2s, num_slope,
                                  slope_crit, slope_crit_min, slope_crit_max) {

  if (samples_since_qrs_start %% samples_per_2s == 0){
    if (num_slope == 0){
      slope_crit = max (slope_crit_min, slope_crit - slope_crit %/% 16)

    } else if (num_slope >=5){
      slope_crit = min (slope_crit_max, slope_crit + slope_crit %/% 16)
    }
  }
  return(slope_crit)
}
