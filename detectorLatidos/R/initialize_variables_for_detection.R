#' Internal function which initializes the variables uses throught the qrs detection program.
#'
#' @param threshold minimum value to be achieved by the filtered signal to detect a slope change
#' @param from_sample initial sample to be analysed.
#' @param slope_crit_max maximum threshold value
#'
#' @return list with the different values used: slope_crit, current_sample_number, t_values, filter, num_slope, sign, max_slope_detected, samples_since_qrs_start, first_sample_of_qrs_complex, detection_window_countdown, annotations.
#'
initialize_variables_for_detection = function(threshold, from_sample, slope_crit_max) {
  return (list(
    slope_crit = slope_crit_max,

    current_sample_number = from_sample,
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
