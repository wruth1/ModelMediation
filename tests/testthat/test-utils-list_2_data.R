

data = as.data.frame(matrix(c(1,0,0,1), nrow = 2))
more_data = as.data.frame(matrix(c(0,1,1,0), nrow = 2))
data_list = list(data, more_data)






# list_2_data_make_labels() ----
supplied_names = list_2_data_make_labels(data_list, c("G1", "G2"))
automatic_labels = list_2_data_make_labels(data_list)
expected_labels = c("G1", "G1", "G2", "G2")
supplied_labels = list_2_data_make_labels(data_list, expected_labels)

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
  new_supplied_labels = list_2_data_make_labels(data_list, c("Alice", "Bob"))

  # Structure is complicated here to accommodate all.equal sometimes returning a string
  expect_false(isTRUE(all.equal(new_supplied_labels, automatic_labels)))
})





# list_2_data() ----
X_data = list_2_data(data_list)


test_that("Dimensions of stack are correct",{
  expect_equal(nrow(X_data), 4)
  expect_equal(ncol(X_data), 3)
})

test_that("Created data.frame has a column of group labels",{
  expect_false(is.null(X_data["group"]))
})

test_that("Group labels column has correct number of each type of label",{
  labels = X_data["group"]
  label_counts = as.numeric(table(labels))

  group_sizes = purrr::map_int(data_list, nrow)

  expect_equal(label_counts, group_sizes)
})

