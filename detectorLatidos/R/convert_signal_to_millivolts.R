convert_signal_to_millivolts = function(signal_data, adc_gain, baseline, sampling_rate){

  num_samples = nrow(signal_data)

  converted_list = list()

  for (s in 1:ncol(signal_data)) {
    signal_name = colnames(signal_data)[s]
    converted_list[[signal_name]] = (signal_data[[s]] - baseline[s]) / adc_gain[s]

  }

  time = seq(0, length.out = num_samples) / sampling_rate

  converted_list[['time']] = time

  converted_signals = as.data.frame(converted_list)

  return(converted_signals)
}
