test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


data = data.frame(X1 = c(1,0), X2 = c(0,1))

# No intercept
beta1 = c(1,2)
lin_pred1 = lin_pred_contrib(data, beta1, add_intercept = F)

test_that("Size is correct. No intercept.",{
  expect_equal(length(lin_pred1), 2)
})

test_that("Value is correct. No intercept.",{
  expect_equal(lin_pred1, c(1,2))
})



# With intercept
beta2 = c(1,2,3)
lin_pred2 = lin_pred_contrib(data, beta2, add_intercept = T)


test_that("Size is correct. With intercept.",{
  expect_equal(length(lin_pred2), 2)
})

test_that("Value is correct. With intercept.",{
  expect_equal(lin_pred2, c(3, 4))
})



# Incompatible sizes
test_that("Error occurs with non-conformable arguments.",{
  expect_error(lin_pred_contrib(data, beta1, add_intercept = T))
  expect_error(lin_pred_contrib(data, beta2, add_intercept = F))
})
