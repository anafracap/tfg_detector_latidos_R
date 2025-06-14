read_signal = function(file, signal_col = 1, has_head = FALSE, header_dat = NULL) {
  ext = tools::file_ext(file)  # Get the file extension
  if (ext == "csv") {
    data = read.csv(file, header = has_head)
    signal_data = data[, c(2,3)] #first column is sample numer

    colnames(signal_data) <- c("signal_1", "signal_2")

    return(signal_data)

  } else if (ext == "dat") {
    sampling_rate = header_dat$general$sampling_rate
    num_samples = header_dat$general$num_samples

    if (header_dat$general$num_signals == 2){
      signal_data = read_bin_wfdb_212_sig_2(file = file, num_samples = num_samples)
    }

    return(signal_data)

  } else {
    stop("Unsupported file type: ", ext)
  }
}
