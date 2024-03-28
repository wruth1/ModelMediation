
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


n = 5000
K = 9
# B = 1008
B = 240



#
# all_settings = read.table("All_Parameter_Combinations.csv", sep = ",")
# this_settings = all_settings[setting_number,]
#
# n = this_settings[1]
# K = this_settings[2]
# B = this_settings[3]

devtools::load_all(".")

# n = 40
# K = 2
# B = 4

# n = 11
# K = 3
# B = 4


num_MC_reps = 1
# num_MC_reps = 2
# num_MC_reps = 50
# num_MC_reps = 144	# = 48 * 3, where 48 is the number of cores in an entire node for Cedar

library(doParallel)
library(rlang)


external_results_prefix = paste0("Boot_Dists/boot_results_n=", n, "_K=", K, "_B=", B, "_M=", num_MC_reps)
dir.create(external_results_prefix, showWarnings = FALSE, recursive = TRUE)
external_runtime_prefix = paste0("Runtimes/n=", n, "_K=", K, "_B=", B, "_M=", num_MC_reps, ".RData")
dir.create("Runtimes", showWarnings = FALSE, recursive = TRUE)


cluster_results_prefix = paste0("scratch/ModelMediation/Data/Timing/boot_results_n=", n, "_K=", K, "_B=", B, "_M=", num_MC_reps)
# dir.create(cluster_results_prefix, showWarnings = FALSE, recursive = TRUE)
cluster_runtime_prefix = paste0("scratch/ModelMediation/R/Not_Run/Empirical_Comparison/", external_runtime_prefix)


all_reg_pars = make_all_reg_pars()


# Monte Carlo study of bootstrap intervals ----

# # Initialize cluster on my machine ----
# n_cores = 2
# # n_cores = parallel::detectCores() - 1
# my_cluster = makeCluster(n_cores, type = "PSOCK")
# registerDoParallel(my_cluster)
# clusterEvalQ(my_cluster,{
#   devtools::load_all(".")
# })
# clusterExport(my_cluster, c("n", "K", "B", "all_reg_pars"))
# clusterSetRNGStream(my_cluster, iseed = 10000*array_index)


# Initialize Cluster ----

nodeslist = unlist(strsplit(Sys.getenv("NODESLIST"), split=" "))
my_cluster = makeCluster(nodeslist, type = "PSOCK")
registerDoParallel(my_cluster)
clusterEvalQ(my_cluster,{
  devtools::load_all("scratch/ModelMediation/")
})
clusterExport(my_cluster, c("n", "K", "B", "all_reg_pars", "cluster_results_prefix"))
clusterSetRNGStream(my_cluster, iseed = 10000*array_index)



tictoc::tic()


# Generate and analyse many datasets ----
for(i in 1:num_MC_reps) {
  set.seed(i)

  ## Make data ----
  data_and_REs = make_validation_data(n, K, all_reg_pars, return_REs = TRUE)
  data = data_and_REs$data
  all_REs = data_and_REs$all_REs

  ## Get true group-specific mediation effects ----
  M_coeffs_fix = all_reg_pars$beta_M
  Y_coeffs_fix = all_reg_pars$beta_Y

  true_med_effs_wide = data.frame()
  for(j in seq_along(all_REs)){
    this_info = all_REs[[j]]
    M_coeffs_ran = this_info$M
    Y_coeffs_ran = this_info$Y

    M_coeffs_mix = M_coeffs_fix + c(M_coeffs_ran, rep(0, times=length(M_coeffs_fix) - length(M_coeffs_ran)))
    Y_coeffs_mix = Y_coeffs_fix + c(Y_coeffs_ran, rep(0, times=length(Y_coeffs_fix) - length(Y_coeffs_ran)))

    reg_coeffs = get_med_coeffs(M_coeffs_mix, Y_coeffs_mix)
    med_effs = rlang::inject(get_med_effs(!!!reg_coeffs)) %>%     # inject() facilitates the use of !!! to pass function arguments as a vector
      t() %>% data.frame()

    med_effs$group = paste0("G", j)

    true_med_effs_wide = rbind(true_med_effs_wide, med_effs)

  }

  ## Get true fixed-effect mediation effects ----
  reg_coeffs_fix = get_med_coeffs(M_coeffs_fix, Y_coeffs_fix)
  med_effs_fix = rlang::inject(get_med_effs(!!!reg_coeffs_fix)) %>%     # inject() facilitates the use of !!! to pass function arguments as a vector
    t() %>% data.frame() %>% dplyr::mutate(group = "fixed")

  true_med_effs_wide = rbind(true_med_effs_wide, med_effs_fix)

  true_med_effs = true_med_effs_wide %>%  med_effs_wide_2_tall() %>% dplyr::rename(truth = estimate)


  ## Run analysis ----
  this_boot_results = run_analysis_boot_dist(data, B, my_cluster, .verbose = FALSE) %>%
    tidyr::pivot_longer(c("de", "ie", "te"), names_to = "med_type", values_to = "estimate")

  ## Add true mediation effects ----
  results_with_truth = dplyr::full_join(this_boot_results, true_med_effs, by = c("med_type", "group"))


  dir.create(external_results_prefix, showWarnings = FALSE, recursive = TRUE)
  save(results_with_truth, file = paste0(external_results_prefix, "/Arr_Ind=", array_index, ",i=", i, ".RData"))

}

print(cluster_results_prefix)

parallel::stopCluster(my_cluster)


cat("\n")
runtime = tictoc::toc()

save(runtime, file = external_runtime_prefix)

