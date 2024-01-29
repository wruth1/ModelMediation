test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


K = 3 # Number of groups
q = 4 # Number of covariates with random effects in each group
SD_mat = matrix(stats::rnorm(q^2), nrow = q, ncol = q) # SD matrix of random effects (i.e. Cholesky factor of its covariance matrix)

all_REs = make_REs(K, SD_mat)

test_that("Dimensions of random effects are correct",{
  expect_equal(length(all_REs), K)

  expect_equal(length(all_REs[[1]]), q)

  all_lengths = purrr::map_int(all_REs, length)
  expect_equal(var(all_lengths), 0)
})

test_that("Simulated random effects have correct object type",{
  all_REs_type = typeof(unlist(all_REs))

  expect_equal(all_REs_type, "double")
})
