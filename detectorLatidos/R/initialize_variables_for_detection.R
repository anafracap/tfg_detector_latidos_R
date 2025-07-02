initialize_variables_for_detection = function(threshold, from_sample, slope_crit_max) {
  return (list(
    slope_crit = slope_crit_max,

    current_sample_number = from_sample,
    next_minute = from_sample + 60,
    minutes = 0,
    t_values = numeric(10),  # Buffer for signal values

    filter = NULL,
    num_slope = 0,
    sign = NULL,
    max_slope_detected = 0,
    samples_since_qrs_start = 0,
    first_sample_of_qrs_complex = NULL,
    detection_window_countdown = NULL,
    # Prepare to store annotations
    annotations = data.frame(sample_number = numeric(0), type = character(0))
  ))
}
