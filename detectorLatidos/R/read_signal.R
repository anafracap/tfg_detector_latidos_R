#' Reads a file containing ECG signal data
#'
#' @param file full file name, including extension.
#' @param signal_col sets the index of the column where the signal data starts (starting on 1)
#' @param has_head value for csv files, defines whether the file has a header row
#' @param header_dat  list containig two lists within with information on the header for dat files
#'
#' @return signal data for 2 signals, returned as a dataframe
#'
#' @export
read_signal = function(file, signal_col = 1, has_head = FALSE, header_dat = NULL, verbose = FALSE) {
  ext = tools::file_ext(file)  # Get the file extension

  if(verbose){
    cat("Trying to read a(n) ", ext, " file \n")
  }

  if (ext == "csv") {
    data = read.csv(file, header = has_head)

    signal_data = data[, c(signal_col, signal_col + 1)] #first column is sample number
    colnames(signal_data) <- c("signal_1", "signal_2")

    if(verbose){
      cat("Read ", nrow(signal_data), " samples. \n")
    }

    return(signal_data)

  } else if (ext == "dat") {
    sampling_rate = header_dat$general$sampling_rate
    num_samples = header_dat$general$num_samples

    if (header_dat$general$num_signals == 2){
      signal_data = read_bin_wfdb_212_sig_2(file = file, num_samples = num_samples)
    } else {
      stop("Unsupported number of signals contained in wfdb file: ", header_dat$general$num_signals)
    }

    if(verbose){
      cat("Read ", nrow(signal_data), " samples. \n")
    }

    return(signal_data)

  } else {
    stop("Unsupported file type: ", ext)
  }
}
