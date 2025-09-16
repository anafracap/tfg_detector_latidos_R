directory_example = "data/example"

record = "101"
dat_file = paste0(directory_example, '/', record, ".dat")
hea_file = paste0(directory_example, '/', record, ".hea")
output =  paste0(directory_example, '/results/', record, ".csv")

header_information = read_header_wfdb (hea_file)

signal_data = read_signal(dat_file, signal_col = 2, has_head = TRUE, header_dat = header_information)

result = qrs_detection(signal_data[["signal_1"]], threshold = 180, from_sample = 0,
                     to_sample = Inf)

output_csv(result, output)
