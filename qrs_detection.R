# QRS detection on a single-channel ECG signal
qrs_detection = function(signal_data, threshold = 500, from_time = 0, to_time = Inf, output = "record_annotations.csv") {
  spm = 60
  next_minute = from_time + spm
  scmin = threshold
  scmax = 10 * scmin
  slopecrit = scmax
  
  # Timing constants (adjustable for pediatric/small mammal ECGs)
  ms160 = 0.00016
  ms200 = 0.0002
  s2 = 2
  
  # Initialize variables
  nslope = 0
  maxslope = 0
  time = 0
  minutes = 0
  now = from_time
  t_values = numeric(10)   # Buffer for signal values
  
  # Prepare to store annotations
  annotations = data.frame(time = numeric(0), type = character(0))
  sample_freq = 360
  
  # Process the signal
  for (v in signal_data) {
    t_values[1] = v
    filter = t_values[1] + 4 * t_values[2] + 6 * t_values[3] + 4 * t_values[4] + t_values[5] -
      t_values[6] - 4 * t_values[7] - 6 * t_values[8] - 4 * t_values[9] - t_values[10]
    
    # Adjust criteria
    if (time %% s2 == 0) {
      if (nslope == 0) {
        slopecrit = slopecrit - (slopecrit %/% 16)
        if (slopecrit < scmin) slopecrit = scmin
      } else if (nslope >= 5) {
        slopecrit = slopecrit + (slopecrit %/% 16)
        if (slopecrit > scmax) slopecrit = scmax
      }
    }
    
    # First slope
    if (nslope == 0 && abs(filter) > slopecrit) {
      nslope = 1
      maxtime = ms160
      sign = ifelse(filter > 0, 1, -1)
      qtime = time
    }
    
    if (nslope != 0) {
      if (filter * sign < -slopecrit) {
        sign = -sign
        nslope = nslope + 1
        maxtime = ifelse(nslope > 4, ms200, ms160)
      } else if (filter * sign > slopecrit && abs(filter) > maxslope) {
        maxslope = abs(filter)
      }
      
      if (maxtime < 0) {
        if (2 <= nslope && nslope <= 4) {
          slopecrit = slopecrit + ((maxslope %/% 4) - slopecrit) %/% 8
          if (slopecrit < scmin) {
            slopecrit = scmin
          } else if (slopecrit > scmax) {
            slopecrit = scmax
          }
          annotations = rbind(annotations, data.frame(time = now - (time - qtime) - 4, type = "NORMAL"))
          time = 0
        } else if (nslope >= 5) {
          annotations = rbind(annotations, data.frame(time = now - (time - qtime) - 4, type = "ARFCT"))
        }
        nslope = 0
      }
      maxtime = maxtime - 1
    }
    
    # Update the buffer and time
    t_values = c(0, t_values[1:9])
    time = time + 1
    now = now + 1
    
    if (now >= next_minute) {
      next_minute = next_minute + spm
      
      # cat(".", append = TRUE)  # Print a dot to indicate progress
      # flush.console()          # Ensure the output is flushed to the console
      
      # Increment minutes and check if it has reached 60
      minutes = minutes + 1
      if (minutes >= 60) {
        cat(" ----", now, "\n")  # Print the formatted time
        minutes = 0  # Reset minutes
      }
    }
    if (now >= to_time) {
      break
    }
  }
  
  # Write the annotations to a file
  write_annotations(annotations, output)
  return(annotations)
}
