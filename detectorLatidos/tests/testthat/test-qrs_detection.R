test_that("Wrong format inputted in dataframe", {
  df = data.frame(c("a", "b", "c"))
  expect_error(qrs_detection(df, threshold = 80, from_sample = 0,
                to_sample = Inf), "Error: signal_data dataframe must contain numeric values." )
})

test_that("Wrong format inputted in list", {
  list = list("a", "b", "c")
  expect_error(qrs_detection(list, threshold = 80, from_sample = 0,
                             to_sample = Inf), "Error: signal_data list must contain numeric values." )
})

test_that("Wrong format inputted in other variables", {
  df = data.frame(c(1, 2, 3))
  expect_error(qrs_detection(df, sampling_rate = "360", threshold = 80, from_sample = 0,
                             to_sample = Inf), "Error: 'sampling_rate' must be numeric." )
  expect_error(qrs_detection(df, sampling_rate = 360, threshold = "80", from_sample = 0,
                             to_sample = Inf), "Error: 'threshold' must be numeric." )
  expect_error(qrs_detection(df, sampling_rate = 360, threshold = 80, from_sample = "0",
                             to_sample = Inf), "Error: 'from_sample' must be numeric." )
  expect_error(qrs_detection(df, sampling_rate = 360, threshold = 80, from_sample = 0,
                             to_sample = "Inf"), "Error: 'to_sample' must be numeric." )
})


