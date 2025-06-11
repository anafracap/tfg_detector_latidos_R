test_that("Doesn't change the criteria if it's still within 2 seconds", {
  constants = list (
    slope_crit_min = 500,
    slope_crit_max = 500 * 10,
    sampling_rate = 360,
    samples_per_160ms = 0.16 * 360,
    samples_per_200ms = 0.2 * 360,
    samples_per_2s = 2 * 360
  )
  variables = list(
    slope_crit = constants$slope_crit_max,

    now = 0,
    next_minute = 0 + 60,
    minutes = 0,
    t_values_buffer = numeric(10),  # Buffer for signal values

    filter = 0,
    nslope = 0,
    maxslope = 0,
    time = 0,
    # Prepare to store annotations
    annotations = data.frame(time = numeric(0), type = character(0))
  )
  expect_equal(adjust_slope_criteria(variables, constants), variables$slope_crit)
})
