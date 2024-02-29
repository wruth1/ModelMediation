
devtools::load_all(".")

n = 21
K = 3
B = 10



all_reg_pars = make_all_reg_pars()

results_prefix = "./R/Not_Run/Empirical_Comparison/Runtimes/"
dir.create(results_prefix, showWarnings = FALSE)


data = make_validation_data(n, K, all_reg_pars)

tictoc::tic()
# this_boot_results = run_analysis(data, B, .verbose = FALSE, .parallel = FALSE)
this_boot_results = run_analysis(data, B, .verbose = TRUE, .parallel = FALSE)
time_info = tictoc::toc()
saveRDS(time_info, file = paste0(results_prefix, "/n=", n, "_K=", K, "_B=", B, ".rds"))


# time_info = readRDS(file = paste0(results_prefix, "/n=", n, "_K=", K, "_B=", B, ".rds"))
