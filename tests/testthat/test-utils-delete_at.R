

A = 1 + 2*(0:3)

test_that("Simple removal works correctly",{
  B = delete_at(A, 1)
  expect_equal(B, c(3, 5, 7))

  C = delete_at(A, c(2,3))
  expect_equal(C, c(1, 7))
})

test_that("Order of indices to delete doesn't matter",{
  some_inds = c(3,4)

  D = delete_at(A, some_inds)
  E = delete_at(A, rev(some_inds))

  expect_equal(D, E)
})


test_that("Deletion works on lists",{
  A1 = list(1, 3, 5, 7)

  B1 = delete_at(A1, 1)
  expect_equal(B1, list(3, 5, 7))

  C1 = delete_at(A1, c(2,3))
  expect_equal(C1, list(1, 7))
})


test_that("Deleting outside the bounds of X throws an error",{
  expect_error(delete_at(A, 17))
})
