test_that("multiplication works", {
  expect_equal(format_scores(c("1", "2", "3"),
                             cut_method = "bins"), c("1", "2", "3"))
  expect_warning(format_scores(c("1", "2", "3"),
                               cut_method = "bins"),
                 regexp = "Non-numeric score vector specified, all unique levels will be used.")

  expect_equal(format_scores(1:4,
                             cut_method = "bins", method_args = 2),
               factor(x = c("1-2", "1-2", "3-4", "3-4"),
                      ordered = TRUE))

  expect_equal(format_scores(c(-5, 1:4, -5, NA),
                             cut_method = "bins", method_args = 2,
                             exceptions = c(-5, NA)),
               factor(x = c("-5", "1-2", "1-2", "3-4", "3-4", "-5", NA),
                      ordered = TRUE, exclude = NULL))

  expect_equal(format_scores(c(1:10, 5),
                             cut_method = "percentiles",
                             method_args = c(0, 0.25, 0.75)),
               factor(x = c("1-4", "1-4", "1-4", "1-4",
                            "5-8", "5-8", "5-8", "5-8",
                            "9-10", "9-10", "5-8"),
                      ordered = TRUE))

  expect_equal(format_scores(c(1:10, 5.5),
                             cut_method = "percentiles",
                             method_args = c(0, 0.25, 0.75)),
               factor(x = c("1-3.5", "1-3.5", "1-3.5",
                            "3.6-7.5", "3.6-7.5", "3.6-7.5", "3.6-7.5",
                            "7.6-10", "7.6-10", "7.6-10", "3.6-7.5"),
                      ordered = TRUE))

  expect_equal(format_scores(1:10,
                             cut_method = "breaks",
                             method_args = c(2, 6, 8)),
               factor(x = c("1-2", "1-2",
                            "3-6", "3-6", "3-6", "3-6",
                            "7-8", "7-8", "9-10", "9-10"),
                      ordered = TRUE))

  expect_equal(format_scores(1:10,
                             cut_method = "breaks",
                             exceptions = 3,
                             method_args = c(2, 6, 8)),
               factor(x = c("1-2", "1-2", "3",
                            "3-6", "3-6", "3-6",
                            "3", "3", "9-10", "9-10"),
                      levels = c("3", "1-2", "3-6", "9-10"),
                      ordered = TRUE))

  expect_error(format_scores(1:10,
                             cut_method = "breaks",
                             exceptions = 2,
                             method_args = c(2, 6, 8)),
               regexp = c("Exception values present in breaks."))
})
