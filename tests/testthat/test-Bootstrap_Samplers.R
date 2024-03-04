

n = 20
K = 3
all_reg_pars = make_all_reg_pars()
data = make_validation_data(n, K, all_reg_pars)

data_npar = one_non_parametric_sample(data)


mod_Y = fit_mod_Y(data)
mod_M = fit_mod_M(data)

data_par = one_parametric_resample(mod_Y, mod_M)



test_that("Size of non-parametric sample is correct",{
  expect_equal(nrow(data_npar), nrow(data))
  expect_equal(ncol(data_npar), ncol(data))
})

test_that("Size of parametric sample is correct",{
  expect_equal(nrow(data_par), nrow(data))
  expect_equal(ncol(data_par), ncol(data))
})

test_that("Grouping variable matches original data",{
  expect_equal(data_npar$group, data$group)
  expect_equal(as.character(data_par$group), data$group)
})

test_that("Every element of non-parametric sample is contained in original dataset (in correct group)",{
  data_list = split(data, data$group)
  data_npar_list = split(data_npar, data_npar$group)

  for(j in seq_along(data_list)){
    expect_true(is_DF1_subset_DF2(data_npar_list[[j]], data_list[[j]])) ######################################!!!!!!!!!!!!!! Start Here
  }
})



# Test bootstrap index generator for semi-par bootstrap ----
boot_inds = get_boot_inds(data)

group_match_check = purrr::map_lgl(seq_len(nrow(data)), \(i){
  ref_group = data$group[i]
  boot_group = data$group[boot_inds[i]]

  return(ref_group == boot_group)
})

test_that("Bootstrap indices respect group membership",{
  expect_true(all(group_match_check))
})

