#' Generates a .csv file with the annotations.
#' @param annotations dataframe containing the detected heartbeats and their annotations
#' @param file full file path and name.
#'
#' @export
output_csv = function(annotations, file = "./output.csv") {
  write.csv(annotations, file, row.names = FALSE)
}
