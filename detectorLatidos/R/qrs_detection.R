#' QRS detection on a single-channel ECG signal.
#'
#' @param signal_data dataframe with the single-channel signal to be analyzed.
#' @param sampling_rate samples per second recorded.
#' @param threshold threshold to be applied to the filterd signal in order to consider it beign a slope change
#' @param from_sample start of the qrs detection in samples.
#' @param to_sample end of the qrs detection in samples.
#'
#' @return Dataframe with the annotated heartbeats. The reference is in sample numbers
qrs_detection = function(signal_data, sampling_rate = 360, threshold = 200, from_sample = 0, to_sample = Inf) {

  constants = list (
    slope_crit_min = threshold,
    slope_crit_max = threshold * 10,
    sampling_rate = sampling_rate,

    # Timing constants (adjustable for pediatric/small mammal ECGs) transforms milliseconds to samples
    samples_per_160ms = as.integer(round(0.16 * sampling_rate)),
    samples_per_200ms = as.integer(round(0.2 * sampling_rate)),
    samples_per_2s = as.integer(round(2 * sampling_rate))
  )

  variables = initialize_variables_for_detection(threshold, from_sample, constants$slope_crit_max)

  signal_data_subset = signal_data[ (from_sample + 1) : length(signal_data)]

  # Process the signal
  for (v in signal_data_subset) {
    variables$t_values[1] = v
    variables$filter = sum( c(1,4,6,4,1,-1,-4,-6,-4,-1) * variables$t_values )


    variables$slope_crit = adjust_slope_criteria(
      samples_since_qrs_start = variables$samples_since_qrs_start,
      samples_per_2s = constants$samples_per_2s,
      num_slope = variables$num_slope,
      slope_crit = variables$slope_crit,
      slope_crit_min = constants$slope_crit_min,
      slope_crit_max = constants$slope_crit_max
    )

    detection_results = slope_detection(constants = constants,
                                        num_slope = variables$num_slope,
                                        filter = variables$filter,
                                        slope_crit = variables$slope_crit,
                                        detection_window_countdown = variables$detection_window_countdown,
                                        sign = variables$sign,
                                        samples_since_qrs_start = variables$samples_since_qrs_start,
                                        first_sample_of_qrs_complex = variables$first_sample_of_qrs_complex,
                                        max_slope_detected = variables$max_slope_detected,
                                        annotations = variables$annotations,
                                        current_sample_number = variables$current_sample_number)

    variables$num_slope = detection_results$num_slope
    variables$filter = detection_results$filter
    variables$slope_crit = detection_results$slope_crit
    variables$detection_window_countdown = detection_results$detection_window_countdown
    variables$sign = detection_results$sign
    variables$first_sample_of_qrs_complex = detection_results$first_sample_of_qrs_complex
    variables$samples_since_qrs_start = detection_results$samples_since_qrs_start
    variables$max_slope_detected = detection_results$max_slope_detected
    variables$annotations = detection_results$annotations




    # Update the buffer and time
    variables$t_values = c(0, variables$t_values[1:9])
    variables$samples_since_qrs_start = variables$samples_since_qrs_start + 1
    variables$current_sample_number = variables$current_sample_number + 1

    if (variables$current_sample_number >= to_sample) {
      break
    }
  }

  return(variables$annotations)
}
