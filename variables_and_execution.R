directory_mitdb = "data/mitdb"
directory_kaggle = "data/kaggle"
record = "100"
directory_pruebas = "data/pruebas"

dat_file = paste0(directory_mitdb, '/', record, ".dat")
csv_file = paste0(directory_kaggle, '/', record, ".csv")
hea_file = paste0(directory_mitdb, '/', record, ".hea")
atr_file_pruebas = paste0(directory_pruebas, '/', record, ".atr")
output =  paste0(directory_kaggle, '/results/', record, ".csv")

signal_data = read_signal(csv_file, signal_col = 2, has_head = TRUE)

result = qrs_detection(signal_data, threshold = 500, from_time = 0, 
                       to_time = Inf, output = output)

write.table(result, file = atr_file_pruebas, row.names = FALSE, col.names = FALSE, 
            sep = " ", quote = FALSE)
print(result)