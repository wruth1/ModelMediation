

a_matrix = matrix(c(1,0,0,1), nrow = 2)
another_matrix = matrix(c(0,1,1,0), nrow = 2)
some_matrices = list(a_matrix, another_matrix)



# list_2_tibble_unnamed() ----
X_stack = list_2_tibble_unnamed(some_matrices)

test_that("Stack is a tibble",{
  expect_true(tibble::is_tibble(X_stack))
})

test_that("Dimensions of stacked objects are correct", {
  expect_equal(nrow(X_stack), 4)
  expect_equal(ncol(X_stack), 2)
})

test_that("Nothing was added or removed when stacking",{
  contents_before = table(unlist(some_matrices))
  contents_after = table(unlist(X_stack))

  expect_equal(contents_before, contents_after)
})






# list_2_tibble_make_labels() ----
supplied_names = list_2_tibble_make_labels(some_matrices, c("G1", "G2"))
automatic_labels = list_2_tibble_make_labels(some_matrices)
expected_labels = c("G1", "G1", "G2", "G2")
supplied_labels = list_2_tibble_make_labels(some_matrices, expected_labels)

test_that("Correct number of labels is created",{
  expect_equal(length(supplied_names), 4)
  expect_equal(length(automatic_labels), 4)
  expect_equal(length(supplied_labels), 4)
})

test_that("Labels are created correctly",{
  expect_equal(supplied_names, expected_labels)
  expect_equal(automatic_labels, expected_labels)
  expect_equal(supplied_labels, expected_labels)
})

test_that("Supplying names differs from automatic labels",{
  new_supplied_labels = list_2_tibble_make_labels(some_matrices, c("Alice", "Bob"))

  # Structure is complicated here to accommodate all.equal sometimes returning a string
  expect_false(isTRUE(all.equal(new_supplied_labels, automatic_labels)))
})





# list_2_tibble() ----
X_tibble = list_2_tibble(some_matrices)

test_that("Stack is a tibble",{
  expect_true(tibble::is_tibble(X_tibble))
})

test_that("Dimensions of stack are correct",{
  expect_equal(nrow(X_tibble), 4)
  expect_equal(ncol(X_tibble), 3)
})

test_that("Created tibble has a column of group labels",{
  expect_false(is.null(X_tibble["group"]))
})

test_that("Group labels column has correct number of each type of label",{
  labels = X_tibble["group"]
  label_counts = as.numeric(table(labels))

  group_sizes = purrr::map_int(some_matrices, nrow)

  expect_equal(label_counts, group_sizes)
})
