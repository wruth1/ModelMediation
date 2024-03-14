


test_boot_results = pbapply::pbsapply(1:num_MC_reps, function(i){
  data = make_validation_data(n, K, all_reg_pars)

    #tictoc::tic()
    # this_boot_results = run_analysis(data, B, .verbose = FALSE, .parallel = FALSE)
    this_boot_results = run_analysis(data, B, .verbose = FALSE, .parallel = FALSE)
    #tictoc::toc()

    # run_analysis_one_bootstrap(real_data, .verbose = TRUE, .parallel = FALSE)

    save(this_boot_results, file = paste0(cluster_results_prefix, "/i=", i, ".RData"))
    # save(this_boot_results, file = paste0(external_results_prefix, "/i=", i, ".RData"))

    return(this_boot_results)
 }, cl = my_cluster)
# })

