constants = list (
  slope_crit_min = 500,
  slope_crit_max = 500 * 10,
  sampling_rate = 360,
  samples_per_160ms = 0.16 * 360,
  samples_per_200ms = 0.2 * 360,
  samples_per_2s = 2 * 360
)

test_that("Doesn't change the criteria if it's still within 2 seconds", {
  slope_crit = constants$slope_crit_max
  num_slope = 0
  max_slope_detected = 0
  samples_since_qrs_start = 1

  expect_equal(
    adjust_slope_criteria(samples_since_qrs_start = samples_since_qrs_start,
                          samples_per_2s = constants$samples_per_2s,
                          num_slope = num_slope,
                          slope_crit = slope_crit,
                          slope_crit_min = constants$slope_crit_min,
                          slope_crit_max = constants$slope_crit_max) ,
    slope_crit)
})

test_that("If 2 seconds passed, and no slope change detected", {
  slope_crit = constants$slope_crit_max
  num_slope = 0
  samples_since_qrs_start = 2*constants$sampling_rate

  expected = slope_crit - slope_crit %/% 16

  expect_equal(
    adjust_slope_criteria(
      samples_since_qrs_start = samples_since_qrs_start,
      samples_per_2s = constants$samples_per_2s,
      num_slope = num_slope,
      slope_crit = slope_crit,
      slope_crit_min = constants$slope_crit_min,
      slope_crit_max = constants$slope_crit_ma),
    expected)
})

test_that("If 2 seconds passed, and no slope change detected, but criteria under lower bounds", {
  slope_crit = constants$slope_crit_min - 10
  num_slope = 0
  samples_since_qrs_start = 2*constants$sampling_rate

  expected = constants$slope_crit_min

  expect_equal(
    adjust_slope_criteria(
      samples_since_qrs_start = samples_since_qrs_start,
      samples_per_2s = constants$samples_per_2s,
      num_slope = num_slope,
      slope_crit = slope_crit,
      slope_crit_min = constants$slope_crit_min,
      slope_crit_max = constants$slope_crit_ma),
    expected)
})

test_that("If 2 seconds passed, and too many slope changes detected", {
  slope_crit = 600
  num_slope = 5
  samples_since_qrs_start = 2*constants$sampling_rate

  expected = slope_crit + slope_crit %/% 16

  expect_equal(
    adjust_slope_criteria(
      samples_since_qrs_start = samples_since_qrs_start,
      samples_per_2s = constants$samples_per_2s,
      num_slope = num_slope,
      slope_crit = slope_crit,
      slope_crit_min = constants$slope_crit_min,
      slope_crit_max = constants$slope_crit_ma),
    expected)
})

test_that("If 2 seconds passed, and too many slope changes detected, but criteria over upper bounds", {
  slope_crit = constants$slope_crit_max + 10
  num_slope = 6
  samples_since_qrs_start = 2*constants$sampling_rate

  expected = constants$slope_crit_max

  expect_equal(
    adjust_slope_criteria(
      samples_since_qrs_start = samples_since_qrs_start,
      samples_per_2s = constants$samples_per_2s,
      num_slope = num_slope,
      slope_crit = slope_crit,
      slope_crit_min = constants$slope_crit_min,
      slope_crit_max = constants$slope_crit_ma),
    expected)
})
