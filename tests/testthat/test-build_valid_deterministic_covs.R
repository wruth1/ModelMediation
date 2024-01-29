
K=3
N=20
data = build_valid_deterministic_covs(K, N)

test_that("Number of groups is correct",{
  expect_length(data, K)
})



test_that("Correct number of obs in each group",{
  expect_equal(nrow(data[[1]]), N)
})


test_that("Same number of obs in each group",{
  all_lengths = purrr::map_int(data, nrow)

  expect_equal(var(all_lengths), 0)
})
