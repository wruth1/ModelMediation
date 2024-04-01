

basic_output = get_med_effs(0,0,0)

test_that("Output of get_med_effs is named correctly", {
  expect_equal(names(basic_output), c("de", "ie", "te"))
})

test_that("Degenerate mediation effects are computed correctly", {
  expect_equal(as.numeric(basic_output), c(1,1,1))
})


some_output = get_med_effs(log(2), log(2), 2)

test_that("More complicated mediation effects are computed correctly",{
  expect_equal(as.numeric(some_output), c(2, 4, 8))
})


neg_output = get_med_effs(-log(2), -log(2), 2)

test_that("Negative regression coefficients are handled correctly",{
  expect_equal(as.numeric(neg_output), c(1/2, 1/4, 1/8))
})
