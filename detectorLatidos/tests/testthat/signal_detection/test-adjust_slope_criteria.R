constants = list (
  slope_crit_min = 500,
  slope_crit_max = 500 * 10,
  sampling_rate = 360,
  samples_per_160ms = 0.16 * 360,
  samples_per_200ms = 0.2 * 360,
  samples_per_2s = 2 * 360
)

test_that("Doesn't change the criteria if it's still within 2 seconds", {
  variables = list(
    slope_crit = constants$slope_crit_max,

    current_sample_number = 1,
    t_values_buffer = numeric(10),  # Buffer for signal values

    filter = 0,
    num_slope = 0,
    max_slope = 0,
    sample_for_slopes = 1,
    # Prepare to store annotations
    annotations = data.frame(sample = numeric(0), type = character(0))
  )
  expect_equal(adjust_slope_criteria(variables, constants), variables$slope_crit)
})

test_that("If 2 seconds passed, and no slope change detected", {
  variables = list(
    slope_crit = constants$slope_crit_max,

    current_sample_number = 2 * constants$sampling_rate,
    t_values_buffer = numeric(10),  # Buffer for signal values

    filter = 0,
    num_slope = 0,
    max_slope = 0,
    sample_for_slopes = 2*constants$sampling_rate,
    # Prepare to store annotations
    annotations = data.frame(sample = numeric(0), type = character(0))
  )
  expected = variables$slope_crit - variables$slope_crit %/% 16

  expect_equal(adjust_slope_criteria(variables, constants), expected)
})
