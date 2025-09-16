test_that("Wrong format inputted", {
  df = data.frame(c("a", "b", "c"))
  expect_error(qrs_detection(df, threshold = 80, from_sample = 0,
                to_sample = Inf), "Error: signal_data dataframe must contain numeric values." )
})
