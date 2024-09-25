# Read binary .dat file with format 212, containing 2 alternating
# signal samples. 
read_bin_wfdb_212_sig_2 = function(file, num_samples){
  bytes_per_signal = num_samples * 1.5
  num_signals = 2
  total_bytes = bytes_per_signal * num_signals
  
  raw_bytes = readBin(file, what = "raw", endian = "little", n = ceiling(total_bytes))
  
  # Initialize vectors to store the signals
  signal_1 = numeric(num_samples)
  signal_2 = numeric(num_samples)
  
  for(i in seq(1, length(raw_bytes) - 2, by = 3)){
    byte_1 = as.integer(raw_bytes[i])
    byte_2 = as.integer(raw_bytes[i + 1])
    byte_3 = as.integer(raw_bytes[i + 2])
    
    second_half_s1 = bitwShiftL(byte_2 %% 16, 8)
    sample_1 = bitwOr(second_half_s1, byte_1)
    
    if (sample_1 >= 2048) {
      sample_1 = sample_1 - 4096  # Convert to signed
    }
    
    sample_index = (i + 2) %/% 3
    signal_1[sample_index] = sample_1
    
    second_half_s2 = bitwShiftL(byte_2 %/% 16, 8)
    sample_2 = bitwOr(second_half_s2, byte_3)
    
    if (sample_2 >= 2048) {
      sample_2 = sample_2 - 4096  # Convert to signed
    }
    
    signal_2[sample_index] = sample_2
  }
 
  return(list(signal_1, signal_2))
}