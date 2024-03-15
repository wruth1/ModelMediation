
dir_prefix = "Data/Timing/"

list.files(dir_prefix)

load("R/Not_Run/Empirical_Comparison/all_par_combinations.RData")
all_pars

all_boot_CIs = data.frame()

library(pbapply)
for(j in 1:nrow(all_pars)){
  this_pars = all_pars[j,]
  n = this_pars$n
  K = this_pars$K
  B = this_pars$B
  num_MC_reps = 50

  this_dir_prefix = paste0(dir_prefix, "boot_results_n=", n, "_K=", K, "_B=", B, "_M=", num_MC_reps)
  # for(i in 1:num_MC_reps){
    some_boot_CIs = pbsapply(1:num_MC_reps, function(i){

    # print(i)
    # some_boot_CIs = pbsapply(1:num_MC_reps, function(i){
    # Load bootstrap estimates
    this_file = paste0(this_dir_prefix, "/i=", i, ".RData")
    load(this_file)

    # Construct bootstrap CIs

    boot_results_par = this_boot_results[["boot_results_par"]]
    boot_results_spar = this_boot_results[["boot_results_spar"]]
    boot_results_npar = this_boot_results[["boot_results_npar"]]

    get_boot_med_effs(boot_results_par)

    boot_CIs_par = get_boot_CIs(boot_results_par, type = "percentile")
    boot_CIs_spar = get_boot_CIs(boot_results_spar, type = "percentile")
    boot_CIs_npar = get_boot_CIs(boot_results_npar, type = "percentile")

    boot_CIs_par$boot_type = "par"
    boot_CIs_spar$boot_type = "spar"
    boot_CIs_npar$boot_type = "npar"
    this_boot_CIs = rbind(boot_CIs_par, boot_CIs_spar, boot_CIs_npar)

    this_boot_CIs$n = n
    this_boot_CIs$K = K
    this_boot_CIs$B = B
    this_boot_CIs$MC_iter = i

    all_boot_CIs = rbind(all_boot_CIs, this_boot_CIs)
    # return(all_boot_CIs)
  })
  # }
}






  external_results_prefix = paste0("Data/Timing/boot_results_n=", n, "_K=", K, "_B=", B, "_M=", num_MC_reps)
  dir.create(external_results_prefix, showWarnings = FALSE, recursive = TRUE)
  external_runtime_prefix = paste0("Runtimes/n=", n, "_K=", K, "_B=", B, "_M=", num_MC_reps, ".RData")

  cluster_results_prefix = paste0("scratch/ModelMediation/Data/Timing/boot_results_n=", n, "_K=", K, "_B=", B, "_M=", num_MC_reps)
  cluster_runtime_prefix = paste0("scratch/ModelMediation/R/Not_Run/Empirical_Comparison/", external_runtime_prefix)

  all_reg_pars = make_all_reg_pars()

  # Monte Carlo study of bootstrap intervals ----

  # Initialize Cluster ----
  # n_cores = 10
  # n_cores = parallel::detectCores() - 1
  #n_cores = parallel::detectCores()
  my_cluster = parallel::makeCluster(48)
  doParallel::registerDoParallel(my_cluster)
  parallel::clusterEvalQ(my_cluster,{
    devtools::load_all(".")
  })
  parallel::clusterExport(my_cluster, c("n", "K", "B", "all_reg_pars", "external_results_prefix"))

  tictoc::tic()

  ### Initialize Progress Bar ----
  ### Note: DoParallel_opts is only used if .parallel == T
  prog = utils::txtProgressBar(max = num_MC_reps, style = 3)
  prog_update = function(n) utils::setTxtProgressBar(prog, n)
  DoParallel_opts = list(progress = prog_update)

  all_boot_results_parallel = foreach::foreach(i = seq_len(num_MC_reps), .options.doParallel = DoParallel_opts) %dopar% {
    set.seed(i * 1000)

    data = make_validation_data(n, K, all_reg_pars)

    this_boot_results = run_analysis(data, B, .verbose = FALSE, .parallel = FALSE)

    save(this_boot_results, file
}
