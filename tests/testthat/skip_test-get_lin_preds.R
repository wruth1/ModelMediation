
K=3
n=20
p=3
q=2

set.seed(1)

data_by_K = make_dataset(K, n, p, q)
beta = c(1,2,3)
u_by_K = list(c(1,1), c(2,2), c(3,3))

lin_preds_by_K = get_lin_preds(data_by_K, beta, u_by_K)


test_that("Dimensions of linear predictor list are correct",{
  expect_equal(length(lin_preds_by_K), K)

  all_lengths = purrr::map_dbl(lin_preds_by_K, length)
  expect_true(all(all_lengths == n))
})


test_that("Linear predictors vary across individuals within the same group",{
  all_vars = purrr::map_dbl(lin_preds_by_K, var)
  expect_true(all(all_vars > 0))
})

test_that("Linear predictor vectors vary across groups",{
  all_comparisons = c()
  for(i in 1:(K-1)){
    for(j in (i+1):K){
      A = lin_preds_by_K[[i]]
      B = lin_preds_by_K[[j]]
      all_comparisons = c(all_comparisons, identical(A, B))
    }
  }

  expect_false(any(all_comparisons))
})

