
B = 200

# Import and process data ----

## Load dat.ma ----
load(paste0("Data/","CleanDataFile-Trust-Nov102023.RData"))

## Convert dat.ma to a formal data frame ----
real_data = dat.ma %>%
  dplyr::mutate(q8.pcis_sure = (q8.pcis == "sure"),
                q8.pcis_medium = (q8.pcis == "medium")) %>%
  dplyr::select(q5.fc, q4.src, q8.pcis_sure, country, q1.cc, q2.dc, q3.pc, q7.la, q8.pcis_medium, q9.edu, age_group, gender) %>%
  lapply(as.character) %>%  as.data.frame() %>%
  make_data_formal(Y_name = "q5.fc", M_name = "q4.src", X_name = "q8.pcis_sure", group_name = "country") %>%
  dplyr::mutate(Y = dplyr::case_match(Y, "all_or_most" ~ 1, "no_or_some" ~ 0),
                M = dplyr::case_match(M, "all_or_most" ~ 1, "no_or_some" ~ 0))

real_data$Y = as.factor(real_data$Y)
real_data$M = as.factor(real_data$M)

# real_data %<>% dplyr::group_by(group) %>%      # Split data into groups
#   dplyr::slice_sample(n = 20, replace = FALSE) %>% # Resample within each group
#   dplyr::ungroup() %>%                              # Remove grouping structure
#   data.frame()

# tictoc::tic()
# mod_M = fit_mod_M_formal(real_data)
# tictoc::toc()
#
# tictoc::tic()
# mod_Y = fit_mod_Y_formal(real_data)
# tictoc::toc()

# save(mod_Y, mod_M, file = "Data/Fitted_Models-Real_Data.RData")
load(file = "Data/Fitted_Models-Real_Data.RData")

library(foreach)


results_prefix = paste0("./Data/Real_Data_Results/")
dir.create(results_prefix, showWarnings = FALSE)








# # Initialize Cluster ----
# n_cores = 10
# # n_cores = parallel::detectCores() - 1
# # n_cores = parallel::detectCores()
# my_cluster = parallel::makeCluster(n_cores)
# snow::clusterSetupRNG(my_cluster)
# doSNOW::registerDoSNOW(cl = my_cluster)
# snow::clusterEvalQ(my_cluster,{
#   devtools::load_all("./")
# })
# # parallel::clusterExport(my_cluster, c("dat.ma", "mod_Y", "mod_M"))
# parallel::clusterExport(my_cluster, c("B", "real_data", "results_prefix"))
#
#
# library(pbapply)
#
#
#
#
# test_boot_results = pbapply::pbsapply(1:B, function(i){
#   this_boot_results = run_analysis_one_bootstrap(real_data, .verbose = FALSE, .parallel = FALSE)
#   # run_analysis_one_bootstrap(real_data, .verbose = TRUE, .parallel = FALSE)
#
#   save(this_boot_results, file = paste0(results_prefix, "/i=", i, ".RData"))
#   return(this_boot_results)
#   }, cl = my_cluster)
#
#
#
# parallel::stopCluster(my_cluster)



# Read-in and aggregate samples ----

## Get names of files containing intermediate bootstrap results ----
results_prefix = paste0("./Data/Real_Data_Results/")
file_names = list.files(results_prefix)


## Perform aggregation ----
boot_results_par = data.frame()
boot_results_spar = data.frame()
boot_results_npar = data.frame()


for(this_name in file_names){
  load(paste0(results_prefix, this_name))
  this_boot_ind = as.numeric(stringr::str_extract(this_name, "\\d+"))

  this_results_par = this_boot_results[["par"]] %>% dplyr::mutate(b = this_boot_ind)
  this_results_spar = this_boot_results[["spar"]] %>% dplyr::mutate(b = this_boot_ind)
  this_results_npar = this_boot_results[["npar"]] %>% dplyr::mutate(b = this_boot_ind)

  boot_results_par = rbind(boot_results_par, this_results_par)
  boot_results_spar = rbind(boot_results_spar, this_results_spar)
  boot_results_npar = rbind(boot_results_npar, this_results_npar)
}

## Sort the data frames by bootstrap iteration ---- ########################################### ToDo: Handle this more elegantly
boot_results_par %<>% dplyr::arrange(b) %>% dplyr::mutate(boot_type = "par")
boot_results_npar %<>% dplyr::arrange(b) %>% dplyr::mutate(boot_type = "npar")
boot_results_spar %<>% dplyr::arrange(b) %>% dplyr::mutate(boot_type = "spar")

## Store all bootstrap regression coefficients ----
all_boot_results = rbind(boot_results_par, boot_results_spar, boot_results_npar)
save(all_boot_results, file = "Data/Real_Data_Boot_Coeffs.RData")



# Construct confidence intervals from each sample ----

# tictoc::tic()
# boot_CIs_par = get_boot_CIs(boot_results_par, mod_Y=mod_Y, mod_M=mod_M)
# tictoc::toc()
#
# tictoc::tic()
# boot_CIs_spar = get_boot_CIs(boot_results_spar, mod_Y=mod_Y, mod_M=mod_M)
# tictoc::toc()
#
# tictoc::tic()
# boot_CIs_npar = get_boot_CIs(boot_results_npar, mod_Y=mod_Y, mod_M=mod_M)
# tictoc::toc()


# save(boot_CIs_par, boot_CIs_spar, boot_CIs_npar, file = "Data/Real_Data_Boot_CIs.RData")
load(file = "Data/Real_Data_Boot_CIs.RData")
