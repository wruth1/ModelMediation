
# # Get simulation settings from command line arguments
setting_number = as.numeric(commandArgs(trailingOnly = TRUE)[1])

# # The actual grid of parameter values I want to evaluate coverage probabilities on
# load("all_par_combinations.RData")
# this_par_comb = all_pars[setting_number,]

# # A small grid of small parameter values for estimating timing of the coverage probability study
load("some_par_combinations.RData")
this_par_comb = some_pars[setting_number,]


# # Extract parameter values for the current run
# n = this_par_comb$n
# K = this_par_comb$K
# B = this_par_comb$B

#
# all_settings = read.table("All_Parameter_Combinations.csv", sep = ",")
# this_settings = all_settings[setting_number,]
#
# n = this_settings[1]
# K = this_settings[2]
# B = this_settings[3]

devtools::load_all(".")

n = 40
K = 2
B = 10

# n = 11
# K = 3
# B = 5


# num_MC_reps = 256
num_MC_reps = 4
# num_MC_reps = 144	# = 48 * 3, where 48 is the number of cores in an entire node for Cedar

library(doParallel)


external_results_prefix = paste0("../../../Data/boot_results_n=", n, "_K=", K, "_B=", B, "_M=", num_MC_reps)
dir.create(external_results_prefix, showWarnings = FALSE)
external_runtime_prefix = paste0("./Runtimes/n=", n, "_K=", K, "_B=", B, "_M=", num_MC_reps, ".RData")

cluster_results_prefix = paste0("scratch/ModelMediation/Data/boot_results_n=", n, "_K=", K, "_B=", B, "_M=", num_MC_reps)
cluster_runtime_prefix = paste0("scratch/ModelMediation/R/Not_Run/Empirical_Comparison/Runtimes/n=", n, "_K=", K, "_B=", B, "_M=", num_MC_reps, ".RData")


all_reg_pars = make_all_reg_pars()


# Monte Carlo study of bootstrap intervals ----




# Initialize Cluster ----
# n_cores = 10
# n_cores = parallel::detectCores() - 1
#n_cores = parallel::detectCores()
nodeslist = unlist(strsplit(Sys.getenv("NODESLIST"), split=" "))
my_cluster = makeCluster(nodeslist, type = "PSOCK")
registerDoParallel(my_cluster)
clusterEvalQ(my_cluster,{
  devtools::load_all("scratch/ModelMediation/")
})
clusterExport(my_cluster, c("n", "K", "B", "all_reg_pars", "cluster_results_prefix"))


tictoc::tic()


# ### Initialize Progress Bar ----
# ### Note: DoSNOW_opts is only used if .parallel == T
# prog = utils::txtProgressBar(max = num_MC_reps, style = 3)
# prog_update = function(n) utils::setTxtProgressBar(prog, n)
# DoSNOW_opts = list(progress = prog_update)


test_boot_results = pbapply::pbsapply(1:num_MC_reps, function(i){
  data = make_validation_data(n, K, all_reg_pars)

    #tictoc::tic()
    # this_boot_results = run_analysis(data, B, .verbose = TRUE, .parallel = FALSE)
    this_boot_results = run_analysis_no_CIs(data, B, .verbose = FALSE, .parallel = FALSE)
    #tictoc::toc()

    # run_analysis_one_bootstrap(real_data, .verbose = TRUE, .parallel = FALSE)

    save(this_boot_results, file = paste0(cluster_results_prefix, "/i=", i, ".RData"))
    # save(this_boot_results, file = paste0(external_results_prefix, "/i=", i, ".RData"))

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





parallel::stopCluster(my_cluster)



cat("\n")
runtime = tictoc::toc()

save(runtime, file = external_runtime_prefix)
