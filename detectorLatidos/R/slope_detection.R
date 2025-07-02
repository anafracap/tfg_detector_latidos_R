#' Internal function contaning the logic for the slope changes detection and the
#' annotation of the possible heartbeats.
#'
#' @param constants list containing the constants involved in the slope detection
#' @param num_slope number of slope changes detected
#' @param filter filtered signal values which to compare against the slope_crit or threshold
#' @param slope_crit threshold applied to the filtered signal.
#' @param detection_window_countdown time countdown in samples of the window available for the next qrs complex detection
#' @param sign sign of the current slope direction
#' @param samples_since_qrs_start samples since the previous qrs complex was detected.
#' @param first_sample_of_qrs_complex number of signals between the previous and current qrs complexes.
#' @param max_slope_detected maximum filtered value detected
#' @param annotations dataframe containing the annotations with the possible heartbeats and their clasiffication
#' @param current_sample_number number of the sample number since the start of the qrs detection, taking into account the original start sample number
#'
#' @return list with the variables modified in this function: num_slope, slope_crit, detection_window_countdown, sign, first_sample_of_qrs_complex, max_slope_detecte and annotations.
#'

slope_detection = function (constants, num_slope, filter, slope_crit,
                            detection_window_countdown, sign, samples_since_qrs_start,
                            first_sample_of_qrs_complex,
                            max_slope_detected, annotations, current_sample_number) {

  # First slope
  if (num_slope == 0 && abs(filter) > slope_crit) {
    num_slope = 1
    detection_window_countdown = constants$samples_per_160ms
    sign = ifelse(filter > 0, 1, -1)
    first_sample_of_qrs_complex = samples_since_qrs_start
  }

  if (num_slope != 0) {

    if (filter * sign < -slope_crit) {

      sign = -sign
      num_slope = num_slope + 1
      detection_window_countdown = ifelse(num_slope > 4, constants$samples_per_200ms, constants$samples_per_160ms)
    } else if (filter * sign > slope_crit && abs(filter) > max_slope_detected) {
      max_slope_detected = abs(filter)
    }

    if (detection_window_countdown < 0) {
      if (2 <= num_slope && num_slope <= 4) {
        slope_crit = slope_crit + ((max_slope_detected %/% 4) - slope_crit) %/% 8

        slope_crit = min (max (slope_crit, constants$slope_crit_min), constants$slope_crit_max)

        annotations = rbind(annotations, data.frame(sample_number = current_sample_number - (samples_since_qrs_start - first_sample_of_qrs_complex) , type = "NORMAL"))

        samples_since_qrs_start = 0
      } else if (num_slope >= 5) {
        annotations = rbind(annotations, data.frame(sample_number = current_sample_number - (samples_since_qrs_start - first_sample_of_qrs_complex) , type = "ARFCT"))

      }
      num_slope = 0
    }
    detection_window_countdown = detection_window_countdown - 1
  }

  return(list(num_slope = num_slope,
              slope_crit = slope_crit,
              detection_window_countdown = detection_window_countdown,
              sign = sign,
              first_sample_of_qrs_complex = first_sample_of_qrs_complex,
              samples_since_qrs_start = samples_since_qrs_start,
              max_slope_detected = max_slope_detected,
              annotations = annotations))
}

#num_slope, filter, slope_crit, detection_window_countdown, samples160, sign, first_sample_of_qrs_complex, max_slope_detected, samples200, scmin, scmax, annotations
#num_slope, filter, slope_crit, detection_window_countdown, sign, first_sample_of_qrs_complex, max_slope_detected, annotations
