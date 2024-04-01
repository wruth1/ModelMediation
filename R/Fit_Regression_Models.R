# Functions which fit regression models to the observed data to predict Y and M.
# Ultimately, I would like to create constructor functions which provide a high-level interface for people to specify the names of their outcome, treatment, mediator and confounder variables (as in the multimediate package), as well as which covariates have fixed and random effects. For now though, I'm just going to write these fitting functions explicitly.
# For now, this file must be updated anytime the model must be changed. Ultimately, these changes will be implemented by calling the constructor with new arguments.






#' Fit a regression model to the observed data with outcome as response variable
#'
#' @param data The observed data. A data.frame.
#'
#' @return A GLMM fit using `glmer` from the `lme4` package.
#' @export
#'
#' @examples
#' n = 20
#' K = 3
#' all_reg_pars = make_all_reg_pars()
#' data = make_validation_data(n, K, all_reg_pars)
#'
#' fit_mod_Y(data)
fit_mod_Y <- function(data){
  suppressMessages(
    lme4::glmer(Y ~ M + X + C1 + C2 + (M + X | group), data = data, family = "binomial",
                control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
  )
}



fit_mod_Y_bad <- function(data){
  suppressMessages(
    lme4::glmer(Y ~ M + X + C1 + C2 + (M + X | group), data = data, family = "binomial")
  )
}



#' Fit a regression model to the observed data with mediator as response variable
#'
#' @param data The observed data. A data.frame.
#'
#' @return A GLMM fit using `glmer` from the `lme4` package.
#' @export
#'
#' @examples
#' n = 20
#' K = 3
#' all_reg_pars = make_all_reg_pars()
#' data = make_validation_data(n, K, all_reg_pars)
#'
#' fit_mod_M(data)
fit_mod_M <- function(data){
  suppressMessages(
    lme4::glmer(M ~ X + C1 + C2 + (X | group), data = data, family = "binomial",
                control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
  )
}




###############################
#### More general versions ####
###############################


# Caution: For now, I'm going to modify these functions to just fit the model I want to the observed data. Later I will need to change them back to address the formal data frame structure.


# In this section, I make strong assumptions about the structure of the data frame which is passed to the fitting functions. I will refer to any data frame which must meet these assumptions as data_formal. The requirements are as follows:
#   Must contain columns named Y, M, X and group
#   May also contain other columns. Each such extra column must be a confounder.      !!!! I'm not sure yet whether I want to require that these columns be named C1, C2, ...
#   Groups must be labelled G1, G2, ...


#' Fit regression model for mediator (`M`) using a formal dataset
#'
#' @details
#' I make strong assumptions about the structure of the data frame which is passed to this function. Such a structured data frame is referred to as `data_formal`. The requirements are as follows:
#'
#' - Must contain columns named Y, M, X and group
#' - May also contain other columns. Each such extra column must be a confounder.      !!!! I'm not sure yet whether I want to require that these columns be named C1, C2, ...
#' - Groups must be labelled G1, G2, ...
#' - Must be sorted by group label
#'
#' @param data_formal A formal data frame. See Details for requirements.
#'
#' @return A GLMM fit using `glmer` from the `lme4` package.
#' @export
#'
#' @examples
#' n = 20
#' K = 3
#' all_reg_pars = make_all_reg_pars()
#' data_formal = make_validation_data(n, K, all_reg_pars)
#'
#' fit_mod_M_formal(data_formal)
fit_mod_M_formal <- function(data_formal){
  suppressMessages(
    if("q8.pcis_medium" %in% colnames(data_formal)){
      lme4::glmer(M ~ . - Y - group + (X + q8.pcis_medium| group), data = data_formal, family = "binomial",
                  control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    } else{
      lme4::glmer(M ~ . - Y - group + (X | group), data = data_formal, family = "binomial",
                  control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    }
  )
}


#' Fit regression model for outcome (`Y`) using a formal dataset
#'
#' @details
#' I make strong assumptions about the structure of the data frame which is passed to this function. Such a structured data frame is referred to as `data_formal`. The requirements are as follows:
#'
#' - Must contain columns named Y, M, X and group
#' - May also contain other columns. Each such extra column must be a confounder.      !!!! I'm not sure yet whether I want to require that these columns be named C1, C2, ...
#' - Groups must be labelled G1, G2, ...
#' - Must be sorted by group label
#'
#' @param data_formal A formal data frame. See Details for requirements.
#'
#' @return A GLMM fit using `glmer` from the `lme4` package.
#' @export
#'
#' @examples
#' n = 20
#' K = 3
#' all_reg_pars = make_all_reg_pars()
#' data_formal = make_validation_data(n, K, all_reg_pars)
#'
#' fit_mod_Y_formal(data_formal)
fit_mod_Y_formal <- function(data_formal){
  suppressMessages(
    if("q8.pcis_medium" %in% colnames(data_formal)){
      lme4::glmer(Y ~ . - group + (M+ X + q8.pcis_medium | group), data = data_formal, family = "binomial",
                  control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    } else{
      lme4::glmer(Y ~ . - group + (M + X | group), data = data_formal, family = "binomial",
                  control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    }
  )
}


rename_one_var <- function(data, new_name, old_name){
  data = dplyr::rename(data, (!!new_name) := (!!old_name))
  return(data)
}

rename_all_vars <- function(data, Y_name = NULL, M_name = NULL, X_name = NULL, group_name = NULL){
  data_renamed = data

  if(!is.null(Y_name)){
    data_renamed = rename_one_var(data_renamed, "Y", Y_name)
  }

  if(!is.null(M_name)){
    data_renamed = rename_one_var(data_renamed, "M", M_name)
  }

  if(!is.null(X_name)){
    data_renamed = rename_one_var(data_renamed, "X", X_name)
  }

  if(!is.null(group_name)){
    data_renamed = rename_one_var(data_renamed, "group", group_name)
  }

  return(data_renamed)
}

recode_groups <- function(data_renamed){
  # data_renamed = rbind(data_renamed, data.frame(Y = 1, M=1, X=1, C1=1, C2=1, group="group_4"))    # This line is useful for testing

  old_levels = sort(unique(data_renamed$group))

  new_levels = paste0("G", 1:length(old_levels))

  data_formal = dplyr::mutate(data_renamed, group = forcats::fct_recode(group, !!!setNames(old_levels, new_levels)))
  return(data_formal)
}


#' Transform `data` into a formal data frame
#'
#' @details
#' I make strong assumptions about the structure of the data frame which is passed to this function. Such a structured data frame is referred to as `data_formal`. The requirements are as follows:
#'
#' - Must contain columns named Y, M, X and group
#' - May also contain other columns. Each such extra column must be a confounder.      !!!! I'm not sure yet whether I want to require that these columns be named C1, C2, ...
#' - Groups must be labelled G1, G2, ...
#' - Must be sorted by group label
#'
#' @param data A data frame containing the outcome, mediator, exposure and group variables. May also contain other columns, which are assumed to be confounders.
#' @param Y_name Name of the outcome variable in `data`. This column name will be converted to `Y`.
#' @param M_name Name of the mediator variable in `data`. This column name will be converted to `M`.
#' @param X_name Name of the exposure variable in `data`. This column name will be converted to `X`.
#' @param group_name Name of the group variable in `data`. This column name will be converted to `group`.
#'
#' @return A formal data frame. See Details.
#' @export
#'
#' @examples
#' B = 2
#' n = 20
#' K = 3
#' all_reg_pars = make_all_reg_pars()
#' data = make_validation_data(n, K, all_reg_pars)
#'
#' # Pollute the data
#' data_new = data %>%
#'  rbind(data.frame(Y = 1, M=1, X=1, C1=1, C2=1, group="group_4"), .) %>%  # Add a fourth group with an incompatible name
#'  dplyr::rename(Y_alt = "Y")                                              # Change the name of Y
#'
#'  make_data_formal(data_new, Y_name = "Y_alt", M_name = "M", X_name = "X", group_name = "group")
make_data_formal <- function(data, Y_name = NULL, M_name = NULL, X_name = NULL, group_name = NULL){
  data_renamed = rename_all_vars(data, Y_name, M_name, X_name, group_name)

  data_formal = recode_groups(data_renamed)

  data_formal %<>% dplyr::arrange(group)

  return(data_formal)
}

