
####################################################################
### This script generates a dataset and constructs bootstrap CIs ###
####################################################################

set.seed(1)


n = 100  # Sample size for each group
K = 3   # Number of groups
B = 20   # Number of bootstrap replicates


all_reg_pars = make_all_reg_pars()                # Use default settings for parameter values
data = make_validation_data(n, K, all_reg_pars)


# This analysis takes a little over a minute on my machine, running in serial
tictoc::tic()

results = run_analysis(data, B, .verbose = TRUE, .parallel = FALSE)


cat("\n")
tictoc::toc()


results$group       # Group labels (including `fixed` for estimates of the fixed effects)
results$med_type    # Label for mediation effect (de: direct effect, ie: indirect effect, te: total effect)
results$lcl         # Lower confidence limit of bootstrap interval for mediation effect
results$ucl         # Upper confidence limit of bootstrap interval for mediation effect
results$estimate    # Point estimate of mediation effect. Depends only on group and mediation effect (i.e. constant                         over bootstrap type and CI type)
results$CI_type     # Type of bootstrap interval (e.g. "percentile", "basic")
results$boot_type   # Type of bootstrap: parametric ("par"), non-parametric ("npar") or semi-parametric ("spar")





