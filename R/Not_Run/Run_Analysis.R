
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
parallel::clusterExport(my_cluster, c("data"))


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
  this_boot_results = run_analysis(data, 500, .verbose = TRUE, .parallel = TRUE)
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




# Read-in results of MC study ----

all_boot_results_list = list()
file_names = list.files("./Data")

all_boot_results_list = purrr::map(file_names, function(this_name){
  load(paste0("./Data/", this_name))
  this_MC_iter = as.numeric(str_extract(this_name, "\\d+"))
  this_boot_results$MC_iter = this_MC_iter

  return(this_boot_results)
})
all_boot_results = purrr::list_rbind(all_boot_results_list)


# Add true mediation effects ----
true_Y_coeffs = all_reg_pars$beta_Y
true_M_coeffs = all_reg_pars$beta_M
true_med_effs = get_med_effs(true_Y_coeffs["X"], true_Y_coeffs["M"], true_M_coeffs["X"])

data_true_med_effs = data.frame(true_val = true_med_effs, med_type = names(true_med_effs))

all_cover_rates = dplyr::full_join(all_boot_results, data_true_med_effs, by = "med_type") %>%
  dplyr::mutate(cover = (lcl < true_val) & (true_val < ucl)) %>%
  dplyr::group_by(group, med_type, CI_type, boot_type) %>%
  dplyr::summarise(cover_rate = mean(cover)) %>%
  dplyr::ungroup()


cover_by_var <- function(var_name, data){
  data %>%
    dplyr::group_by(!!rlang::sym(eval(var_name))) %>%
    dplyr::summarise(cover_rate = mean(cover_rate))
}



cover_by_group = all_cover_rates %>% dplyr::group_by(group) %>% dplyr::summarise(cover_rate = mean(cover_rate))
cover_by_med_type = all_cover_rates %>% dplyr::group_by(med_type) %>% dplyr::summarise(cover_rate = mean(cover_rate))
cover_by_CI_type = all_cover_rates %>% dplyr::group_by(CI_type) %>% dplyr::summarise(cover_rate = mean(cover_rate))
cover_by_boot_type = all_cover_rates %>% dplyr::group_by(boot_type) %>% dplyr::summarise(cover_rate = mean(cover_rate))

pct_cover_rates = dplyr::filter(all_cover_rates, CI_type == "pct")

pct_cover_by_group = cover_by_var("group", pct_cover_rates)
pct_cover_by_med_type = cover_by_var("med_type", pct_cover_rates)
pct_cover_by_boot_type = cover_by_var("boot_type", pct_cover_rates)


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
