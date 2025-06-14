read_header_wfdb = function (hea_file){
  header_lines = readLines(hea_file)

  general_information = strsplit(header_lines[1], " ")[[1]]
  num_signals = as.integer(general_information[2])

  file_name = character(num_signals)
  format_code = integer(num_signals)
  adc_gain = numeric(num_signals)
  bit_resolution = integer(num_signals)
  baseline = integer(num_signals)
  first_sample = integer(num_signals)
  module_checksum = integer(num_signals)
  block_size = integer(num_signals)
  lead_name = character(num_signals)

  for(s in 1:num_signals){
    information = strsplit(header_lines[1 + s], " ")[[1]]
    file_name[s] = information[1]
    format_code[s] = as.integer(information[2])
    adc_gain[s] = as.numeric(information[3])
    bit_resolution[s] = as.integer(information[4])
    baseline[s] = as.integer(information[5])
    first_sample[s] = as.integer(information[6])
    module_checksum[s] = as.integer(information[7])
    block_size[s] = as.integer(information[8])
    lead_name[s] = information[9] # MLII or V5
  }

  general = list(
    record_name = general_information[1],
    num_signals = as.integer(general_information[2]),
    sampling_rate = as.integer(general_information[3]),
    num_samples = as.integer(general_information[4])
  )

  per_sample = list(
    file_name = file_name,
    format_code = format_code,
    adc_gain = adc_gain,
    bit_resolution = bit_resolution,
    baseline = baseline,
    first_sample = first_sample,
    module_checksum = module_checksum,
    block_size = block_size,
    lead_name = lead_name
  )

  return(list(general = general, per_sample = per_sample))

}
