

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

  print("Running parametric bootstrap")
  boot_results_par = run_bootstrap(B, mod_Y = mod_Y, mod_M = mod_M, boot_type = "par", .parallel = .parallel, .verbose = .verbose)
  cat("\n")

  print("Running semi-parametric bootstrap")
  boot_results_spar = run_bootstrap(B, mod_Y = mod_Y, mod_M = mod_M, boot_type = "spar", .parallel = .parallel, .verbose = .verbose)
  cat("\n")

  print("Running non-parametric bootstrap")
  boot_results_npar = run_bootstrap(B, data = data, boot_type = "npar", .parallel = .parallel, .verbose = .verbose)
  cat("\n")

  boot_CIs_par = get_boot_CIs(boot_results_par, mod_Y=mod_Y, mod_M=mod_M)
  boot_CIs_spar = get_boot_CIs(boot_results_spar, mod_Y=mod_Y, mod_M=mod_M)
  boot_CIs_npar = get_boot_CIs(boot_results_npar, mod_Y=mod_Y, mod_M=mod_M)

  boot_CIs_par$boot_type = "par"
  boot_CIs_spar$boot_type = "spar"
  boot_CIs_npar$boot_type = "npar"
  all_boot_CIs = rbind(boot_CIs_par, boot_CIs_spar, boot_CIs_npar)

  # output = list(CIs = all_boot_CIs, boot_results_par = boot_results_par, boot_results_spar = boot_results_spar, boot_results_npar = boot_results_npar)
  return(all_boot_CIs)
}


#' Generate bootstrap samples, compute estimates, and construct CIs. Run in parallel using `pbapply`
#'
#' @param data Observed dataset.
#' @param B Number of bootstrap replicates.
#' @param cl A cluster object
#' @param .verbose Should progress bars be produced for the two bootstrap analyses?
#'
#' @return A data frame containing confidence intervals for each mediation effect, in each group, for all flavours of bootstrap and bootstrap CI.
#' @export
#'
#' @examples
#' 1+1
run_analysis_parallel <- function(data, B, cl, .verbose = FALSE){
  mod_Y = fit_mod_Y(data)
  mod_M = fit_mod_M(data)

  print("Running parametric bootstrap")
  boot_results_par = run_bootstrap_parallel(B, cl, mod_Y = mod_Y, mod_M = mod_M, boot_type = "par", .verbose = .verbose)
  cat("\n")

  print("Running semi-parametric bootstrap")
  boot_results_spar = run_bootstrap_parallel(B, cl, mod_Y = mod_Y, mod_M = mod_M, boot_type = "spar", .verbose = .verbose)
  cat("\n")

  print("Running non-parametric bootstrap")
  boot_results_npar = run_bootstrap_parallel(B, cl, data = data, boot_type = "npar", .verbose = .verbose)
  cat("\n")

  boot_CIs_par = get_boot_CIs(boot_results_par, mod_Y=mod_Y, mod_M=mod_M)
  boot_CIs_spar = get_boot_CIs(boot_results_spar, mod_Y=mod_Y, mod_M=mod_M)
  boot_CIs_npar = get_boot_CIs(boot_results_npar, mod_Y=mod_Y, mod_M=mod_M)

  boot_CIs_par$boot_type = "par"
  boot_CIs_spar$boot_type = "spar"
  boot_CIs_npar$boot_type = "npar"
  all_boot_CIs = rbind(boot_CIs_par, boot_CIs_spar, boot_CIs_npar)

  # output = list(CIs = all_boot_CIs, boot_results_par = boot_results_par, boot_results_spar = boot_results_spar, boot_results_npar = boot_results_npar)
  return(all_boot_CIs)
}







# Run all three flavours of bootstrap. Return mediation effects, not CIs
run_analysis_boot_dist <- function(data, B, cl, .verbose = FALSE){
  mod_Y = fit_mod_Y(data)
  mod_M = fit_mod_M(data)

  print("Running parametric bootstrap")
  boot_results_par = run_bootstrap_parallel(B, cl, mod_Y = mod_Y, mod_M = mod_M, boot_type = "par", .verbose = .verbose) %>%
    get_med_effs_DF() %>% dplyr::mutate(boot_type = "par")
  cat("\n")

  print("Running semi-parametric bootstrap")
  boot_results_spar = run_bootstrap_parallel(B, cl, mod_Y = mod_Y, mod_M = mod_M, boot_type = "spar", .verbose = .verbose) %>%
    get_med_effs_DF() %>% dplyr::mutate(boot_type = "spar")
  cat("\n")

  print("Running non-parametric bootstrap")
  boot_results_npar = run_bootstrap_parallel(B, cl, data = data, boot_type = "npar", .verbose = .verbose) %>%
    get_med_effs_DF() %>% dplyr::mutate(boot_type = "npar")
  cat("\n")


  all_boot_results = rbind(boot_results_par, boot_results_spar, boot_results_npar)

  return(all_boot_results)
}








#' Generate bootstrap samples, compute estimates, and construct CIs. Data must be formatted as a formal data frame.
#'
#' @param data_formal A formal data frame.
#' @param B Number of bootstrap replicates.
#' @param .parallel Should bootstrapping be performed in parallel?
#' @param .verbose Should progress bars be produced for the two bootstrap analyses?
#'
#' @return A data frame containing confidence intervals for each mediation effect, in each group, for all flavours of bootstrap and bootstrap CI.
#' @export
#'
#' @examples
#' 1+1
run_analysis_formal <- function(data_formal, B, .parallel = FALSE, .verbose = FALSE){
  mod_Y = fit_mod_Y_formal(data_formal)
  mod_M = fit_mod_M_formal(data_formal)

  boot_results_par = run_bootstrap(B, mod_Y = mod_Y, mod_M = mod_M, boot_type = "par", .parallel = .parallel, .verbose = .verbose)
  boot_results_spar = run_bootstrap(B, mod_Y = mod_Y, mod_M = mod_M, boot_type = "spar", .parallel = .parallel, .verbose = .verbose)
  boot_results_npar = run_bootstrap(B, data = data_formal, boot_type = "npar", .parallel = .parallel, .verbose = .verbose)

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



#' Generate and return a single bootstrap sample of each flavour. Data must be formatted as a formal data frame.
#'
#' @param data_formal Observed dataset, structured as a formal data frame
#' @param mod_Y,mod_M Pre-fit models for the outcome and mediator, respectively. If NULL, these models will be fit inside the function.
#' @param .parallel Should bootstrapping be performed in parallel?
#' @param .verbose Should progress bars be produced for the two bootstrap analyses?
#'
#' @return A list containing one bootstrap sample of each flavour (parametric, semiparametric and nonparametric, in that order). Each sample contains the three mediation effects, both in aggregate and for each group.
#' @export
#'
#' @examples
#' 1+1
run_analysis_one_bootstrap <- function(data_formal, mod_Y=NULL, mod_M=NULL, .parallel = FALSE, .verbose = FALSE){
  if(is.null(mod_Y)){
    mod_Y = fit_mod_Y_formal(data_formal)
  }
  if(is.null(mod_M)){
    mod_M = fit_mod_M_formal(data_formal)
  }

  # tictoc::tic()
  # boot_results_par = run_bootstrap(1, mod_Y = mod_Y, mod_M = mod_M, boot_type = "par", .parallel = .parallel, .verbose = .verbose)
  # tictoc::toc()
  #
  # tictoc::tic()
  # boot_results_spar = run_bootstrap(1, mod_Y = mod_Y, mod_M = mod_M, boot_type = "spar", .parallel = .parallel, .verbose = .verbose)
  # tictoc::toc()
  #
  # tictoc::tic()
  # boot_results_npar = run_bootstrap(1, data = data_formal, boot_type = "npar", .parallel = .parallel, .verbose = .verbose)
  # tictoc::toc()

  boot_results_par = run_bootstrap(1, mod_Y = mod_Y, mod_M = mod_M, boot_type = "par", .parallel = .parallel, .verbose = .verbose)
  boot_results_spar = run_bootstrap(1, mod_Y = mod_Y, mod_M = mod_M, boot_type = "spar", .parallel = .parallel, .verbose = .verbose)
  boot_results_npar = run_bootstrap(1, data = data_formal, boot_type = "npar", .parallel = .parallel, .verbose = .verbose)



  this_boot_results = list(par = boot_results_par,
                           spar = boot_results_spar,
                           npar = boot_results_npar)

  return(this_boot_results)
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

