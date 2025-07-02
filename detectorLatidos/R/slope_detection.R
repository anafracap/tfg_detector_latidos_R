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

  return(list(num_slope = num_slope, slope_crit = slope_crit,
         detection_window_countdown = detection_window_countdown, sign = sign,
         first_sample_of_qrs_complex = first_sample_of_qrs_complex,
         samples_since_qrs_start = samples_since_qrs_start,
         max_slope_detected = max_slope_detected, annotations = annotations))
}

#num_slope, filter, slope_crit, detection_window_countdown, samples160, sign, first_sample_of_qrs_complex, max_slope_detected, samples200, scmin, scmax, annotations
#num_slope, filter, slope_crit, detection_window_countdown, sign, first_sample_of_qrs_complex, max_slope_detected, annotations
