test_that("Test Formula Interface", {
  expect_equal(prep_formula(scr1 + scr2 ~ scr3 | perf),
               list(LHS = c("scr1", "scr2"),
                    RHS_L = c("scr3"),
                    RHS_R = c("perf"),
                    RHS_R_bv = NULL))

  expect_equal(prep_formula(scr1 + scr2 ~ scr3),
               list(LHS = c("scr1", "scr2"),
                    RHS_L = c("scr3"),
                    RHS_R = NULL,
                    RHS_R_bv = NULL))

  expect_equal(prep_formula(scr1 + scr2 ~ scr3 | !perf),
               list(LHS = c("scr1", "scr2"),
                    RHS_L = c("scr3"),
                    RHS_R = NULL,
                    RHS_R_bv = "perf"))

  expect_equal(prep_formula(scr1 + scr2 ~ scr3 | perf + !b2),
               list(LHS = c("scr1", "scr2"),
                    RHS_L = c("scr3"),
                    RHS_R = c("perf"),
                    RHS_R_bv = "b2"))

  expect_error(prep_formula(scr1 | scr2 ~ scr3 + scr4| perf + !b2),
               regexp = c("Invalid character \\w*"))

  expect_error(prep_formula(scr2 ~ scr3 + scr3 | scr4 | perf + !b2),
               regexp = c("Right hand side of formula \\w*"))
})
