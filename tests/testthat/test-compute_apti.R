test_that("compute_apti works with sample_data", {
  data(sample_data)

  result <- comp_apti(
    A = sample_data$A,
    TC = sample_data$TC,
    P = sample_data$P,
    R = sample_data$R
  )

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 6)  # A, TC, P, R, APTI, Category
  expect_equal(nrow(result), 15)
  expect_true(all(result$APTI > 0))
  expect_true(all(result$Category %in% c("Sensitive", "Intermediate", "Tolerant")))
})
