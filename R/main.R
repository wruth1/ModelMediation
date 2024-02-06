
# A sample analysis ----
an_analysis <- function(){
  n = 20
  K = 3
  all_reg_pars = make_all_reg_pars()
  data = make_validation_data(n, K, all_reg_pars)

  run_analysis(data)
}


run_analysis <- function(data, B, .parallel = FALSE){
  mod_Y = fit_mod_Y(data)
  mod_M = fit_mod_M(data)

  boot_results_par = run_bootstrap(B, mod_Y = mod_Y, mod_M = mod_M, parametric = TRUE, .parallel = .parallel)
  boot_results_npar = run_bootstrap(B, data = data, parametric = FALSE, .parallel = .parallel)

  boot_CIs_par = get_boot_CIs(boot_results_par, mod_Y=mod_Y, mod_M=mod_M)
  boot_CIs_npar = get_boot_CIs(boot_results_npar, mod_Y=mod_Y, mod_M=mod_M)

  boot_CIs_par$boot_type = "par"
  boot_CIs_npar$boot_type = "npar"
  all_boot_CIs = rbind(boot_CIs_par, boot_CIs_npar)

  output = list(CIs = all_boot_CIs, boot_results_par = boot_results_par, boot_results_npar = boot_results_npar)
  return(all_boot_CIs)
}


# tic()
#
# boot_output = run_analysis(data, 1000)
#
# toc()
#
# X = c(20, 40, 60)
# Y = c(37.84, 71.65, 102.22)
# Z = data.frame(X=X, Y=Y)
#
# fit = lm(Y ~ X, data = Z)
# summary(fit)
# plot(X, Y, xlim = c(0, 70))
# abline(fit)
# predict(fit, newdata=data.frame(X=1000))
#
