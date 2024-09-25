read_signal = function(file, signal_col = 1, has_head = FALSE) {
  ext = tools::file_ext(file)  # Get the file extension
  if (ext == "csv") {
    data = read.csv(file, header = has_head)
    signal_data = data[, signal_col]
  } else if (ext == "dat") { # ----> Doesn't read data correctly
    # For .dat files, assuming binary data and little-endian 16-bit integers
    # Header details 100.hea
    sampling_rate = 360
    nsamples = 650000
    nsignals = 2
    
    # ADC gain and baseline for each signal
    adc_gain <- c(212, 212)
    baseline <- c(200, 200)

    con = file(file, "rb")
    ecg_raw = readBin(con, what = integer(), size = 2, signed = TRUE, endian = "little", n = nsamples * nsignals)
    close(con)
    
    # Reshape the data into two columns, since there are two signals (MLII and V5)
    ecg_matrix = matrix(ecg_raw, ncol = nsignals, byrow = TRUE)
    signal_data = ecg_matrix[, 1]
  } else {
    stop("Unsupported file type: ", ext)
  }
  return(signal_data)
}