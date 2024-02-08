
library(foreach)

# num_failed_boots = 0

n = 20
K = 3
all_reg_pars = make_all_reg_pars()
data = make_validation_data(n, K, all_reg_pars)


# Monte Carlo study of bootstrap intervals ----

num_MC_reps = 100
# num_MC_reps = 5

# Initialize Cluster ----
n_cores = 10
# n_cores = parallel::detectCores() - 1
# n_cores = detectCores()
my_cluster = parallel::makeCluster(n_cores)
doSNOW::registerDoSNOW(cl = my_cluster)
snow::clusterEvalQ(my_cluster,{
  devtools::load_all("./")
})
# parallel::clusterExport(my_cluster, c("dat.ma", "mod_Y", "mod_M"))


tictoc::tic()


### Initialize Progress Bar ----
### Note: DoSNOW_opts is only used if .parallel == T
prog = utils::txtProgressBar(max = num_MC_reps, style = 3)
prog_update = function(n) utils::setTxtProgressBar(prog, n)
DoSNOW_opts = list(progress = prog_update)

tictoc::tic()
all_boot_results_parallel = foreach::foreach(i = seq_len(num_MC_reps), .options.snow = DoSNOW_opts) %dopar% {
  set.seed(i * 1000)

  data = make_validation_data(n, K, all_reg_pars)

  this_boot_results = run_analysis(data, 500, .verbose = FALSE)
  # this_boot_results = run_analysis(data, 2, .verbose = FALSE)
  save(this_boot_results, file = paste0("./Data/boot_results-", i, ".RData"))

  return(this_boot_results)
}
cat("\n")
tictoc::toc()


# tic()
# all_boot_results_serial = foreach::foreach(i = seq_len(num_MC_reps)) %do% {
#   set.seed(i * 1000)
#
#   this_boot_results = run_analysis(data, 10)
#
#   return(this_boot_results)
# }
# toc()

parallel::stopCluster(my_cluster)

#
#
# for(i in seq_along(all_boot_results_parallel)){
#   all_boot_results_parallel[[i]]$MC_iter = i
# }
#
# all_boot_results_serial %>%
#   purrr::imap(\(data, i) cbind(data, rep(i, times = nrow(data)))) %>%
#   purrr::list_rbind() %>%
#   dplyr::rename_with(\(name) "MC_iter", tidyselect::last_col())         # Rename the rightmost column
#
#
#
# true_Y_coeffs = all_reg_pars$beta_Y
# true_M_coeffs = all_reg_pars$beta_M
# true_med_effs = get_med_effs(true_Y_coeffs[3], true_Y_coeffs[2], true_M_coeffs[2])
