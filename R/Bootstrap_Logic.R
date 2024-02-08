

#' Run a complete bootstrap analysis with `B` bootstrap samples
#'
#' @param B Number of bootstrap samples.
#' @param data Original dataset. Optional for parametric (as long as `mod_Y` and `mod_M` are supplied), required for non-parametric, .
#' @param mod_Y Regression model for predicting `Y`. Recommended but optional for parametric (as long as `data` is supplied), not used for non-parametric.
#' @param mod_M Regression model for predicting `M`. Recommended but optional for parametric (as long as `data` is supplied), not used for non-parametric.
#' @param parametric Indicates whether to perform parametric or non-parametric bootstrap.
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
#' run_bootstrap(B, mod_Y = mod_Y, mod_M = mod_M, parametric = TRUE, .parallel = FALSE)
#'
#' run_bootstrap(B, data = data, parametric = FALSE, .parallel = FALSE)
run_bootstrap <- function(B, data = NULL, mod_Y = NULL, mod_M = NULL, parametric = TRUE, .parallel = FALSE, .verbose = TRUE){
  check_bootstrap_inputs(data, mod_Y, mod_M, parametric)

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
      # set.seed(i * 1000)  # Let the user set.seed at a higher level

      this_boot_results = one_bootstrap(data, mod_Y, mod_M, parametric)
      this_boot_results$b = i

      return(this_boot_results)
    }
  } else {
    ## Serial ----
    all_boot_results = foreach::foreach(i = seq_len(B)) %do% {
      # set.seed(i * 1000)  # Let the user set.seed at a higher level

      this_boot_results = one_bootstrap(data, mod_Y, mod_M, parametric)
      this_boot_results$b = i

      if(.verbose){
        prog_update(i)
        cat("\n")
      }

      return(this_boot_results)
    }
  }
  # Format output as a single data frame
  output = purrr::list_rbind(all_boot_results)

  return(output)
}




#' Perform a single bootstrap analysis
#'
#' @param data Original dataset. Optional for parametric (as long as `mod_Y` and `mod_M` are supplied), required for non-parametric, .
#' @param mod_Y Regression model for predicting `Y`. Recommended but optional for parametric (as long as `data` is supplied), not used for non-parametric.
#' @param mod_M Regression model for predicting `M`. Recommended but optional for parametric (as long as `data` is supplied), not used for non-parametric.
#' @param parametric Indicates whether to perform parametric or non-parametric bootstrap.
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
#' \dontrun{one_bootstrap(mod_Y = mod_Y, mod_M = mod_M, parametric = TRUE, .careful = TRUE)}
#'
#' one_bootstrap(data = data, parametric = FALSE, .careful = TRUE)
one_bootstrap <- function(data = NULL, mod_Y = NULL, mod_M = NULL, parametric = TRUE, .careful = TRUE){
  if(.careful){
    check_bootstrap_inputs(data, mod_Y, mod_M, parametric)
  }

  boot_successful = FALSE

  while(!boot_successful){
    tryCatch({
      boot_data = one_bootstrap_sample(data, mod_Y, mod_M, parametric, .careful = FALSE)

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
#' @param parametric Indicates whether to perform parametric or non-parametric bootstrap.
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
#' one_bootstrap_sample(mod_Y = mod_Y, mod_M = mod_M, parametric = TRUE, .careful = TRUE)
#'
#' one_bootstrap_sample(data = data, parametric = FALSE, .careful = TRUE)
one_bootstrap_sample <- function(data = NULL, mod_Y = NULL, mod_M = NULL, parametric = TRUE, .careful = TRUE){
  if(.careful){
    check_bootstrap_inputs(data, mod_Y, mod_M, parametric)
  }

  if(parametric){
    models_present = !(is.null(mod_Y) || is.null(mod_M))

    if(models_present){
      return(one_parametric_resample(mod_Y, mod_M))
    } else{
      # If models are not supplied, fit them, then generate the bootstrap sample
      mod_Y = fit_mod_Y(data)
      mod_M = fit_mod_M(data)

      return(one_parametric_resample(mod_Y, mod_M))
    }
  } else{
    return(one_non_parametric_sample(data))
  }
}

#' Check whether enough information has been provided to fit the requested flavour of bootstrap
#'
#' @param data Original dataset. Optional for parametric (as long as `mod_Y` and `mod_M` are supplied), required for non-parametric, .
#' @param mod_Y Regression model for predicting `Y`. Recommended but optional for parametric (as long as `data` is supplied), not used for non-parametric.
#' @param mod_M Regression model for predicting `M`. Recommended but optional for parametric (as long as `data` is supplied), not used for non-parametric.
#' @param parametric Indicates whether to perform parametric or non-parametric bootstrap.
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
#' check_bootstrap_inputs(mod_Y = mod_Y, mod_M = mod_M, parametric = TRUE)
#' check_bootstrap_inputs(data = data, parametric = FALSE)
#'
#' # This setting is not supported
#' tryCatch({
#'     check_bootstrap_inputs(mod_Y = mod_Y, mod_M = mod_M, parametric = FALSE)
#'     print("No error found. This represents a failure of check_bootstrap_inputs()")
#'   },
#'   error = function(e) e
#' )
#'
#'
#' # This setting is okay, but probably inefficient
#' check_bootstrap_inputs(data = data, parametric = TRUE)
check_bootstrap_inputs <- function(data = NULL, mod_Y = NULL, mod_M = NULL, parametric = TRUE){
  data_present = !is.null(data)
  models_present = !(is.null(mod_Y) || is.null(mod_M))

  if(!data_present && !models_present){
    stop("In bootstrap: One of either data or models must be provided.")
  } else if(!parametric && !data_present){
    stop("In bootstrap: Data must be supplied when performing non-parametric bootstrap.")
  } else if(parametric && !models_present){
    warning("In bootstrap: Models will be fit to data before performing parametric bootstrap. Consider supplying fitted models to save efficiency.")
  }
}



