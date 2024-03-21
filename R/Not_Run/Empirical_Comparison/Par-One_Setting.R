
# Get simulation settings from command line arguments
array_index = as.numeric(commandArgs(trailingOnly = TRUE)[1])
# array_index = 1


# # # The actual grid of parameter values I want to evaluate coverage probabilities on
# load("all_par_combinations.RData")
# this_par_comb = all_pars[setting_number,]
#
# # # A small grid of small parameter values for estimating timing of the coverage probability study
# # load("some_par_combinations.RData")
# # this_par_comb = some_pars[setting_number,]
#
#
# # Extract parameter values for the current run
# n = this_par_comb$n
# K = this_par_comb$K
# B = this_par_comb$B


# n = 500
# K = 10
# B = 500



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
B = 4

# n = 11
# K = 3
# B = 5


# num_MC_reps = 256
num_MC_reps = 2
# num_MC_reps = 50
# num_MC_reps = 144	# = 48 * 3, where 48 is the number of cores in an entire node for Cedar

library(doParallel)


external_results_prefix = paste0("../../../Data/Timing/boot_results_n=", n, "_K=", K, "_B=", B, "_M=", num_MC_reps)
dir.create(external_results_prefix, showWarnings = FALSE, recursive = TRUE)
external_runtime_prefix = paste0("Runtimes/n=", n, "_K=", K, "_B=", B, "_M=", num_MC_reps, ".RData")
dir.create("Runtimes", showWarnings = FALSE, recursive = TRUE)


cluster_results_prefix = paste0("scratch/ModelMediation/Data/Timing/boot_results_n=", n, "_K=", K, "_B=", B, "_M=", num_MC_reps)
# dir.create(cluster_results_prefix, showWarnings = FALSE, recursive = TRUE)
cluster_runtime_prefix = paste0("scratch/ModelMediation/R/Not_Run/Empirical_Comparison/", external_runtime_prefix)


all_reg_pars = make_all_reg_pars()


# Monte Carlo study of bootstrap intervals ----

# # Initialize cluster on my machine ----
# n_cores = 10
# my_cluster = makeCluster(n_cores, type = "PSOCK")
# registerDoParallel(my_cluster)
# clusterEvalQ(my_cluster,{
#   devtools::load_all(".")
# })
# clusterExport(my_cluster, c("n", "K", "B", "all_reg_pars"))


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
clusterSetRNGStream(my_cluster, iseed = 10000*array_index)



tictoc::tic()

for(i in 1:num_MC_reps) {
  data = make_validation_data(n, K, all_reg_pars)

  this_boot_results = run_analysis_parallel(data, B, my_cluster, .verbose = FALSE)


  dir.create(cluster_results_prefix, showWarnings = FALSE, recursive = TRUE)
  save(this_boot_results, file = paste0(cluster_results_prefix, "/Arr_Ind=", array_index, ",i=", i, ".RData"))

  return(this_boot_results)
}

parallel::stopCluster(my_cluster)


cat("\n")
runtime = tictoc::toc()

save(runtime, file = external_runtime_prefix)

