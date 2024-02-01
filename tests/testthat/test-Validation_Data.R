test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})



n = 20
K = 3

all_reg_pars = make_all_reg_pars()


data_list = make_validation_data(n, K, all_reg_pars, output_list = TRUE)
data_frame = make_validation_data(n, K, all_reg_pars, output_list = FALSE)


test_that("Size of dataset list is correct",{
  expect_equal(length(data_list), K)

  one_group = data_list[[1]]
  expect_equal(nrow(one_group), n)
  expect_equal(ncol(one_group), 5)
})

test_that("Size of data frame is correct",{
  expect_equal(nrow(data_frame), K*n)
  expect_equal(ncol(data_frame), 6)
})
