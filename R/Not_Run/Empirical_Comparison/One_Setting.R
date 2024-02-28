
# # Get simulation settings from command line arguments
# setting_number = as.numeric(commandArgs(trailingOnly = TRUE)[1])
#
# all_settings = read.table("All_Parameter_Combinations.csv", sep = ",")
# this_settings = all_settings[setting_number,]
#
# n = this_settings[1]
# K = this_settings[2]
# B = this_settings[3]


n = 100
K = 10
B = 200

# n = 11
# K = 3
# B = 5


num_MC_reps = 200
# num_MC_reps = 256
# num_MC_reps = 10

library(foreach)


results_prefix = paste0("./Data/boot_results_n=", n, "_K=", K, "_B=", B, "_M=", num_MC_reps)
dir.create(results_prefix, showWarnings = FALSE)


all_reg_pars = make_all_reg_pars()


# Monte Carlo study of bootstrap intervals ----




# Initialize Cluster ----
n_cores = 10
# n_cores = parallel::detectCores() - 1
# n_cores = parallel::detectCores()
my_cluster = parallel::makeCluster(n_cores)
doSNOW::registerDoSNOW(cl = my_cluster)
snow::clusterEvalQ(my_cluster,{
  devtools::load_all("./")
})
# parallel::clusterExport(my_cluster, c("dat.ma", "mod_Y", "mod_M"))
parallel::clusterExport(my_cluster, c("n", "K", "B", "all_reg_pars", "results_prefix"))


# tictoc::tic()


### Initialize Progress Bar ----
### Note: DoSNOW_opts is only used if .parallel == T
prog = utils::txtProgressBar(max = num_MC_reps, style = 3)
prog_update = function(n) utils::setTxtProgressBar(prog, n)
DoSNOW_opts = list(progress = prog_update)


test_boot_results = pbapply::pbsapply(1:B, function(i){
  data = make_validation_data(n, K, all_reg_pars)

  this_boot_results = run_analysis(data, B, .verbose = FALSE, .parallel = FALSE)
  # run_analysis_one_bootstrap(real_data, .verbose = TRUE, .parallel = FALSE)

  save(this_boot_results, file = paste0(results_prefix, "/i=", i, ".RData"))
  return(this_boot_results)
  }, cl = my_cluster)


# all_boot_results_parallel = foreach::foreach(i = seq_len(num_MC_reps), .options.snow = DoSNOW_opts) %dopar% {
#   set.seed(i * 1000)
#
#   data = make_validation_data(n, K, all_reg_pars)
#
#   # tictoc::tic()
#   this_boot_results = run_analysis(data, B, .verbose = FALSE, .parallel = FALSE)
#   # this_boot_results = run_analysis(data, B, .verbose = TRUE, .parallel = TRUE)
#   # this_boot_results = run_analysis(data, B, .verbose = TRUE, .parallel = FALSE)
#   # tictoc::toc()
#
#   # this_boot_results = run_analysis(data, B, .verbose = FALSE, .parallel = TRUE)
#   # this_boot_results = run_analysis(data, 2, .verbose = FALSE)
#   save(this_boot_results, file = paste0(results_prefix, "/i=", i, ".RData"))
#
#   return(this_boot_results)
# }
# cat("\n")
# tictoc::toc()



parallel::stopCluster(my_cluster)


