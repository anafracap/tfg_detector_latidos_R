adjust_slope_criteria = function(variables, constants) {
  if (variables$sample_for_slopes %% constants$samples_per_2s == 0){
    if (variables$num_slope == 0){
      variables$slope_crit = variables$slope_crit - variables$slope_crit %/% 16
      return(variables$slope_crit)
    }
  }else {
    return(variables$slope_crit)
  }

}
