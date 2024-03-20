

#' Run a complete bootstrap analysis with `B` bootstrap samples
#'
#' @param B Number of bootstrap samples.
#' @param data Original dataset. Optional for parametric (as long as `mod_Y` and `mod_M` are supplied), required for non-parametric, .
#' @param mod_Y Regression model for predicting `Y`. Recommended but optional for parametric (as long as `data` is supplied), not used for non-parametric.
#' @param mod_M Regression model for predicting `M`. Recommended but optional for parametric (as long as `data` is supplied), not used for non-parametric.
#' @param boot_type Indicates which type of bootstrap should be performed. Options include parametric (`par`), non-parametric (`npar`), and semi-parametric (`spar`).
#' @param .parallel Should the `B` analyses be performed in parallel?
#' @param .verbose Should a progress bar be printed?
#'
#' @return Standard output from a bootstrap analysis. Ready to be used for constructing confidence intervals.
#' @export
#'
#' @examples
#' B = 2
#' n = 20
#' K = 3
#' all_reg_pars = make_all_reg_pars()
#' data = make_validation_data(n, K, all_reg_pars)
#'
#' mod_Y = fit_mod_Y(data)
#' mod_M = fit_mod_M(data)
#'
#' run_bootstrap(B, mod_Y = mod_Y, mod_M = mod_M, boot_type = "par", .parallel = FALSE)
#'
#' run_bootstrap(B, data = data, boot_type = "npar", .parallel = FALSE)
run_bootstrap <- function(B, data = NULL, mod_Y = NULL, mod_M = NULL, boot_type = c("par", "npar", "spar"), .parallel = FALSE, .verbose = TRUE){
  check_bootstrap_inputs(data, mod_Y, mod_M, boot_type)

  if(.verbose){
    ### Initialize Progress Bar ----
    ### Note: DoSNOW_opts is only used if .parallel == T
    prog = utils::txtProgressBar(max = B, style = 3)
    prog_update = function(n) utils::setTxtProgressBar(prog, n)
    DoSNOW_opts = list(progress = prog_update)
  } else{
    DoSNOW_opts = list()
  }

  # Run B bootstrap analyses
  if(.parallel){
    ## Parallel ----
    all_boot_results = foreach::foreach(i = seq_len(B), .options.snow = DoSNOW_opts) %dopar% {

      this_boot_results = one_bootstrap(data, mod_Y, mod_M, boot_type)
      this_boot_results$b = i

      return(this_boot_results)
    }
  } else {
    ## Serial ----
    all_boot_results = foreach::foreach(i = seq_len(B)) %do% {

      this_boot_results = one_bootstrap(data, mod_Y, mod_M, boot_type)
      this_boot_results$b = i

      if(.verbose){
        prog_update(i)
      }

      return(this_boot_results)
    }
  }
  # Format output as a single data frame
  output = purrr::list_rbind(all_boot_results)

  return(output)
}

#' Run a complete bootstrap analysis with `B` bootstrap samples in parallel using `pbapply`
#'
#' @param B Number of bootstrap samples.
#' @param cl A cluster object
#' @param data Original dataset. Optional for parametric (as long as `mod_Y` and `mod_M` are supplied), required for non-parametric, .
#' @param mod_Y Regression model for predicting `Y`. Recommended but optional for parametric (as long as `data` is supplied), not used for non-parametric.
#' @param mod_M Regression model for predicting `M`. Recommended but optional for parametric (as long as `data` is supplied), not used for non-parametric.
#' @param boot_type Indicates which type of bootstrap should be performed. Options include parametric (`par`), non-parametric (`npar`), and semi-parametric (`spar`).
#' @param .verbose Should a progress bar be printed?
#'
#' @return Standard output from a bootstrap analysis. Ready to be used for constructing confidence intervals.
#' @export
#'
#' @examples
#' B = 2
#' n = 20
#' K = 3
#' all_reg_pars = make_all_reg_pars()
#' data = make_validation_data(n, K, all_reg_pars)
#'
#' mod_Y = fit_mod_Y(data)
#' mod_M = fit_mod_M(data)
#'
#' run_bootstrap(B, mod_Y = mod_Y, mod_M = mod_M, boot_type = "par", .parallel = FALSE)
#'
#' run_bootstrap(B, data = data, boot_type = "npar", .parallel = FALSE)
run_bootstrap_parallel <- function(B, cl, data = NULL, mod_Y = NULL, mod_M = NULL, boot_type = c("par", "npar", "spar"), .verbose = TRUE){
  check_bootstrap_inputs(data, mod_Y, mod_M, boot_type)

  # Run B bootstrap analyses ----

  all_boot_results = pblapply(seq_len(B), function(i){

    this_boot_results = one_bootstrap(data, mod_Y, mod_M, boot_type)
    this_boot_results$b = i

    return(this_boot_results)
  }, cl=cl)

  # Format output as a single data frame
  output = purrr::list_rbind(all_boot_results)

  return(output)
}




#' Perform a single bootstrap analysis
#'
#' @param data Original dataset. Optional for parametric (as long as `mod_Y` and `mod_M` are supplied), required for non-parametric, .
#' @param mod_Y Regression model for predicting `Y`. Recommended but optional for parametric (as long as `data` is supplied), not used for non-parametric.
#' @param mod_M Regression model for predicting `M`. Recommended but optional for parametric (as long as `data` is supplied), not used for non-parametric.
#' @param boot_type Indicates which type of bootstrap should be performed. Options include parametric (`par`), non-parametric (`npar`), and semi-parametric (`spar`).
#' @param .careful Should input be checked for whether enough information has been provided for the requested flavour of bootstrap? Recommended setting is `TRUE` if running interactively and `FALSE` if running inside another function which has already checked.
#'
#' @return One component of standard bootstrap output.
#' @export
#'
#' @examples
#' n = 20
#' K = 3
#' all_reg_pars = make_all_reg_pars()
#' data = make_validation_data(n, K, all_reg_pars)
#'
#' mod_Y = fit_mod_Y(data)
#' mod_M = fit_mod_M(data)
#'
#' \dontrun{one_bootstrap(mod_Y = mod_Y, mod_M = mod_M, boot_type = "par", .careful = TRUE)}
#'
#' one_bootstrap(data = data, boot_type = "npar", .careful = TRUE)
one_bootstrap <- function(data = NULL, mod_Y = NULL, mod_M = NULL, boot_type = c("par", "npar", "spar"), .careful = TRUE){
  if(.careful){
    check_bootstrap_inputs(data, mod_Y, mod_M, boot_type)
  }

  boot_successful = FALSE

  while(!boot_successful){
    tryCatch({
      boot_data = one_bootstrap_sample(data, mod_Y, mod_M, boot_type, .careful = FALSE)

      boot_coeffs = boot_samp_2_coeffs(boot_data)

      boot_successful = TRUE
    },
    error = function(e){ e}) #num_failed_boots <<- num_failed_boots + 1})
  }

  return(boot_coeffs)
}


#' Generate a single bootstrap sample
#'
#' @param data Original dataset. Optional for parametric (as long as `mod_Y` and `mod_M` are supplied), required for non-parametric, .
#' @param mod_Y Regression model for predicting `Y`. Recommended but optional for parametric (as long as `data` is supplied), not used for non-parametric.
#' @param mod_M Regression model for predicting `M`. Recommended but optional for parametric (as long as `data` is supplied), not used for non-parametric.
#' @param boot_type Indicates which type of bootstrap should be performed. Options include parametric (`par`), non-parametric (`npar`), and semi-parametric (`spar`).
#' @param .careful Should input be checked for whether enough information has been provided for the requested flavour of bootstrap? Recommended setting is `TRUE` if running interactively and `FALSE` if running inside another function which has already checked.
#'
#' @return A single bootstrap sample, formatted as a data.frame.
#' @export
#'
#' @examples
#' n = 20
#' K = 3
#' all_reg_pars = make_all_reg_pars()
#' data = make_validation_data(n, K, all_reg_pars)
#'
#' mod_Y = fit_mod_Y(data)
#' mod_M = fit_mod_M(data)
#'
#' one_bootstrap_sample(mod_Y = mod_Y, mod_M = mod_M, boot_type = "par", .careful = TRUE)
#'
#' one_bootstrap_sample(data = data, boot_type = "npar", .careful = TRUE)
one_bootstrap_sample <- function(data = NULL, mod_Y = NULL, mod_M = NULL, boot_type = c("par", "npar", "spar"), .careful = TRUE){
  if(.careful){
    check_bootstrap_inputs(data, mod_Y, mod_M, boot_type)
  }

  ## If models are required but not supplied, fit them ----
  if(boot_type == "par" || boot_type == "spar"){
    if(is.null(mod_Y)){
      mod_Y = fit_mod_Y_formal(data)
    }
    if(is.null(mod_M)){
      mod_M = fit_mod_M_formal(data)
    }
  }

  if(boot_type == "par"){
      return(one_parametric_sample(mod_Y, mod_M))
  } else if(boot_type == "spar"){
      return(one_semi_parametric_sample(mod_Y, mod_M))
  } else{
    return(one_non_parametric_sample(data))
  }
}

#' Check whether enough information has been provided to fit the requested flavour of bootstrap
#'
#' @param data Original dataset. Optional for parametric (as long as `mod_Y` and `mod_M` are supplied), required for non-parametric, .
#' @param mod_Y Regression model for predicting `Y`. Recommended but optional for parametric (as long as `data` is supplied), not used for non-parametric.
#' @param mod_M Regression model for predicting `M`. Recommended but optional for parametric (as long as `data` is supplied), not used for non-parametric.
#' @param boot_type Indicates which type of bootstrap should be performed. Options include parametric (`par`), non-parametric (`npar`), and semi-parametric (`spar`).
#'
#' @return `TRUE` if there is sufficient information available to fit the requested bootstrap. `FALSE` otherwise. See arguments for requirements
#' @export
#'
#' @examples
#' n = 20
#' K = 3
#' all_reg_pars = make_all_reg_pars()
#' data = make_validation_data(n, K, all_reg_pars)
#'
#' mod_Y = fit_mod_Y(data)
#' mod_M = fit_mod_M(data)
#'
#' # These settings are fine
#' check_bootstrap_inputs(mod_Y = mod_Y, mod_M = mod_M, boot_type = "par")
#' check_bootstrap_inputs(mod_Y = mod_Y, mod_M = mod_M, boot_type = "spar")
#' check_bootstrap_inputs(data = data, boot_type = "npar")
#'
#' # This setting is not supported
#' tryCatch({
#'     check_bootstrap_inputs(mod_Y = mod_Y, mod_M = mod_M, boot_type = "npar")
#'     print("No error found. This represents a failure of check_bootstrap_inputs()")
#'   },
#'   error = function(e) e
#' )
#'
#'
#' # These settings are okay, but inefficient
#' \dontrun{
#'  check_bootstrap_inputs(data = data, boot_type = "par")
#'  check_bootstrap_inputs(data = data, boot_type = "spar")
#' }
check_bootstrap_inputs <- function(data = NULL, mod_Y = NULL, mod_M = NULL, boot_type = c("par", "npar", "spar")){
  data_present = !is.null(data)
  models_present = !(is.null(mod_Y) || is.null(mod_M))

  if(!data_present && !models_present){
    stop("In bootstrap: One of either data or models must be provided.")
  } else if((boot_type == "npar") && !data_present){
    stop("In bootstrap: Data must be supplied when performing non-parametric bootstrap.")
  } else if((boot_type == "par") && !models_present){
    warning("In bootstrap: Models will be fit to data before performing parametric bootstrap. Consider supplying fitted models to improve efficiency.")
  } else if((boot_type == "spar") && !models_present){
    warning("In bootstrap: Models will be fit to data before performing semi-parametric bootstrap. Consider supplying fitted models to improve efficiency.")
  }
}



