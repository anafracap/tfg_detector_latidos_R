initialize_variables_for_detection = function(threshold, from_sample, slope_crit_max) {
  return (list(
    slope_crit = slope_crit_max,

    now = from_sample,
    next_minute = from_sample + 60,
    minutes = 0,
    t_values_buffer = numeric(10),  # Buffer for signal values

    filter = 0,
    nslope = 0,
    maxslope = 0,
    time = 0,
    # Prepare to store annotations
    annotations = data.frame(time = numeric(0), type = character(0))
  ))
}
