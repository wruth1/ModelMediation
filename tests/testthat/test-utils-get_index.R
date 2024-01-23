

A = c(1,2,3)
B = c("a", "b")
C = 1:1000
all_vecs = list(A, B, C)


test_that("Simple extraction works",{
  check = get_index_vec(all_vecs, 2)

  expect_equal(check, c("2", "b", "2"))
})





A1 = list(1, 2, 3)
B1 = list("a", "b")
all_lists = list(A1, B1)

test_that("Extraction from lists returns list of contents (instead of list of length 1 lists containing those contents)",{
  check = get_index_list(all_lists, 2)

  expect_equal(check, list(2, "b"))
})








test_that("Passing too large of an index gives an error",{
  expect_error(get_index_vec(all_vecs, 4))
  expect_error(get_index_list(all_lists, 4))
})

