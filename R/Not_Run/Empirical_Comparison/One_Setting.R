
# # Get simulation settings from command line arguments
# setting_number = as.numeric(commandArgs(trailingOnly = TRUE)[1])
#
# all_settings = read.table("All_Parameter_Combinations.csv", sep = ",")
# this_settings = all_settings[setting_number,]
#
# n = this_settings[1]
# K = this_settings[2]
# B = this_settings[3]


# n = 100
# K = 3
# B = 200

n = 10
K = 3
B = 5


num_MC_reps = 256
# num_MC_reps = 10

library(foreach)


results_prefix = paste0("./Data/boot_results_n=", n, "_K=", K, "_B=", B, "_M=", num_MC_reps)
dir.create(results_prefix, showWarnings = FALSE)


all_reg_pars = make_all_reg_pars()


# Monte Carlo study of bootstrap intervals ----




# Initialize Cluster ----
# n_cores = 10
# n_cores = parallel::detectCores() - 1
n_cores = parallel::detectCores()
my_cluster = parallel::makeCluster(n_cores)
doSNOW::registerDoSNOW(cl = my_cluster)
snow::clusterEvalQ(my_cluster,{
  devtools::load_all("./")
})
# parallel::clusterExport(my_cluster, c("dat.ma", "mod_Y", "mod_M"))
parallel::clusterExport(my_cluster, c("n", "K", "B", "all_reg_pars", "results_prefix"))


tictoc::tic()


### Initialize Progress Bar ----
### Note: DoSNOW_opts is only used if .parallel == T
prog = utils::txtProgressBar(max = num_MC_reps, style = 3)
prog_update = function(n) utils::setTxtProgressBar(prog, n)
DoSNOW_opts = list(progress = prog_update)

all_boot_results_parallel = foreach::foreach(i = seq_len(num_MC_reps), .options.snow = DoSNOW_opts) %dopar% {
  set.seed(i * 1000)

  data = make_validation_data(n, K, all_reg_pars)

  # tictoc::tic()
  this_boot_results = run_analysis(data, B, .verbose = FALSE, .parallel = FALSE)
  # this_boot_results = run_analysis(data, B, .verbose = TRUE, .parallel = TRUE)
  # tictoc::toc()

  # this_boot_results = run_analysis(data, B, .verbose = FALSE, .parallel = TRUE)
  # this_boot_results = run_analysis(data, 2, .verbose = FALSE)
  save(this_boot_results, file = paste0(results_prefix, "/i=", i, ".RData"))

  return(this_boot_results)
}
cat("\n")
tictoc::toc()



parallel::stopCluster(my_cluster)




# Read-in results of MC study ----

all_boot_results_list = list()
dir_names = list.files("./Data")

this_dir = paste0("./Data/", dir_names[1], "/")
file_names = list.files(this_dir)



all_boot_results_list = purrr::map(file_names, function(this_name){
  load(paste0(this_dir, this_name))
  this_MC_iter = as.numeric(stringr::str_extract(this_name, "\\d+"))
  this_boot_results$MC_iter = this_MC_iter

  return(this_boot_results)
})
all_boot_results = purrr::list_rbind(all_boot_results_list)


# # Check for NAs and remove corresponding MC runs
# ind_na = which(is.na(all_boot_results), arr.ind=T)
# table(all_boot_results[ind_na[,1], "MC_iter"])
#
# all_boot_results %<>% dplyr::filter(!(MC_iter %in% c(62, 175, 189)))
# sum(is.na(all_boot_results))




# all_boot_results = purrr::list_rbind(all_boot_results_parallel)
#
#
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


cover_global = mean(all_cover_rates$cover_rate)
cover_by_group = all_cover_rates %>% dplyr::group_by(group) %>% dplyr::summarise(cover_rate = mean(cover_rate))
cover_by_med_type = all_cover_rates %>% dplyr::group_by(med_type) %>% dplyr::summarise(cover_rate = mean(cover_rate))
cover_by_CI_type = all_cover_rates %>% dplyr::group_by(CI_type) %>% dplyr::summarise(cover_rate = mean(cover_rate))
cover_by_boot_type = all_cover_rates %>% dplyr::group_by(boot_type) %>% dplyr::summarise(cover_rate = mean(cover_rate))

pct_cover_rates = dplyr::filter(all_cover_rates, CI_type == "pct")

pct_cover_global = mean(pct_cover_rates$cover_rate)
pct_cover_by_group = cover_by_var("group", pct_cover_rates)
pct_cover_by_med_type = cover_by_var("med_type", pct_cover_rates)
pct_cover_by_boot_type = cover_by_var("boot_type", pct_cover_rates)

