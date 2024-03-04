


B = 2
n = 20
K = 3
all_reg_pars = make_all_reg_pars()
data = make_validation_data(n, K, all_reg_pars)






# Creation of a formal data frame ----
## See `Fit_Regression_Models.R` for what makes a data frame "formal"


data_new = data %>%
  rbind(data.frame(Y = 1, M=1, X=1, C1=1, C2=1, group="group_4"), .) %>%
  dplyr::rename(Y_alt = "Y")


data_renamed = rename_all_vars(data_new, "Y_alt", "M", "X", "group")

test_that("Outcome has successfully been renamed", {
  expect_true("Y" %in% colnames(data_renamed))
  expect_false("Y_alt" %in% colnames(data_renamed))
})


data_formal = recode_groups(data_renamed)

test_that("Group labels have successfully been recoded", {
  expect_equal(as.character(sort(unique(data_formal$group))), paste0("G", 1:4))
})


data_formal_sorted = dplyr::arrange(data_formal, group)

data_formal_direct = make_data_formal(data_new, "Y_alt", "M", "X", "group")

test_that("Directly constructing the formal data frame works", {
  expect_equal(data_formal_sorted, data_formal_direct)
})
