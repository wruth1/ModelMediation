
# A sample analysis ----
an_analysis <- function(){
  n = 100
  K = 3
  all_reg_pars = make_all_reg_pars()
  data = make_validation_data(n, K, all_reg_pars)

  run_analysis(data)
}


#' Generate bootstrap samples, compute estimates, and construct CIs
#'
#' @param data Observed dataset.
#' @param B Number of bootstrap replicates.
#' @param .parallel Should bootstrapping be performed in parallel?
#' @param .verbose Should progress bars be produced for the two bootstrap analyses?
#'
#' @return A data frame containing confidence intervals for each mediation effect, in each group, for all flavours of bootstrap and bootstrap CI.
#' @export
#'
#' @examples
#' 1+1
run_analysis <- function(data, B, .parallel = FALSE, .verbose = FALSE){
  mod_Y = fit_mod_Y(data)
  mod_M = fit_mod_M(data)

  boot_results_par = run_bootstrap(B, mod_Y = mod_Y, mod_M = mod_M, boot_type = "par", .parallel = .parallel, .verbose = .verbose)
  boot_results_spar = run_bootstrap(B, mod_Y = mod_Y, mod_M = mod_M, boot_type = "spar", .parallel = .parallel, .verbose = .verbose)
  boot_results_npar = run_bootstrap(B, data = data, boot_type = "npar", .parallel = .parallel, .verbose = .verbose)

  boot_CIs_par = get_boot_CIs(boot_results_par, mod_Y=mod_Y, mod_M=mod_M)
  boot_CIs_spar = get_boot_CIs(boot_results_spar, mod_Y=mod_Y, mod_M=mod_M)
  boot_CIs_npar = get_boot_CIs(boot_results_npar, mod_Y=mod_Y, mod_M=mod_M)

  boot_CIs_par$boot_type = "par"
  boot_CIs_spar$boot_type = "spar"
  boot_CIs_npar$boot_type = "npar"
  all_boot_CIs = rbind(boot_CIs_par, boot_CIs_spar, boot_CIs_npar)

  output = list(CIs = all_boot_CIs, boot_results_par = boot_results_par, boot_results_spar = boot_results_spar, boot_results_npar = boot_results_npar)
  return(all_boot_CIs)
}

#
# tic()
#
# boot_output = run_analysis(data, 200)
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

