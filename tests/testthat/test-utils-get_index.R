

A = c(1,2,3)
B = c("a", "b")
C = 1:1000
all_vecs = list(A, B, C)


test_that("Simple extraction works",{
  check = get_index(all_vecs, 2)

  expect_equal(check, c("2", "b", "2"))
})


test_that("Passing too large of an index gives an error",{
  expect_error(get_index(all_vecs, 4))
})
