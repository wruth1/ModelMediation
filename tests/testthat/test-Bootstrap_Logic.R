

B = 2
n = 20
K = 3
all_reg_pars = make_all_reg_pars()
data = make_validation_data(n, K, all_reg_pars)

mod_Y = fit_mod_Y(data)
mod_M = fit_mod_M(data)

par_output = run_bootstrap(B, mod_Y = mod_Y, mod_M = mod_M, parametric = TRUE, .parallel = FALSE, .verbose = FALSE)

npar_output = run_bootstrap(B, data = data, parametric = FALSE, .parallel = FALSE, .verbose = FALSE)


test_that("Dimensions are correct for parametric bootstrap.",{
  expect_equal(ncol(par_output), 5)
  expect_equal(nrow(par_output), B * (K+1))
})


test_that("Dimensions are correct for non-parametric bootstrap.",{
  expect_equal(ncol(npar_output), 5)
  expect_equal(nrow(npar_output), B * (K+1))
})



test_that("Group variable is defined correctly.",{
  par_group = par_output$group
  npar_group = npar_output$group

  group_names = c("fixed", paste0("G", 1:K))
  group_labels = rep(group_names, times = B)

  expect_equal(par_group, group_labels)
  expect_equal(npar_group, group_labels)
})


test_that("Bootstrap iteration label is defined correctly.",{
  par_boot = par_output$b
  npar_boot = npar_output$b


  boot_labels = rep(1:B, each = (K+1))

  expect_equal(par_boot, boot_labels)
  expect_equal(npar_boot, boot_labels)
})
