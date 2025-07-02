#' Generates a .csv file with the annotations.
#'
#'
output_csv = function(annotations, file = "./output") {
  write.csv(annotations, file, row.names = FALSE)
}
