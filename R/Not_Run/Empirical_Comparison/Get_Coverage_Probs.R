
library(stringr)

# Read-in results of MC study ----

num_MC_reps = 144



results_folder = "./Data/From_Cluster"

folder_names = list.files(results_folder)

# Keep only runs with the correct number of MC replicates
all_folders = str_subset(folder_names, paste0("M=", num_MC_reps))



# Load all confidence intervals and combine into a single data frame
all_intervals = pbapply::pblapply(all_folders, function(this_folder){

  this_par_all_CIs = data.frame()

  # Load all files with the current parameter configuration
  for(i in 1:num_MC_reps){
    this_file = paste0(results_folder, "/", this_folder, "/i=", i, ".RData")
    if(file.exists(this_file)){ # A very small number of bootstrap samples failed, leading to no output for that value of i
      load(this_file)

      # Get just the fixed effects CIs, since I didn't save the random effects
      fixef_CIs = this_boot_results %>% dplyr::filter(group == "fixed")

      this_par_all_CIs = rbind(this_par_all_CIs, fixef_CIs)

    }


  }

  # Get the current parameter values and add them to the data frame of CIs
  this_n = this_folder %>% str_extract("n=\\d+") %>% str_extract("\\d+") %>% as.numeric()
  this_K = this_folder %>% str_extract("K=\\d+") %>% str_extract("\\d+") %>% as.numeric()
  this_B = this_folder %>% str_extract("B=\\d+") %>% str_extract("\\d+") %>% as.numeric()

  this_par_all_CIs$n = this_n
  this_par_all_CIs$K = this_K
  this_par_all_CIs$B = this_B

  return(this_par_all_CIs)

}) %>%
  purrr::list_rbind() %>%          # Combine all data frames
  na.omit()                        # Remove rows with missing values (due to n being too small)


# Count number of infinities ----
count_infinities = 0
for(i in 1:nrow(all_intervals)){
  print(paste0("Row ", i, " of ", nrow(all_intervals)))
  for(j in 1:ncol(all_intervals)){
    count_infinities = count_infinities + is.infinite(unlist(all_intervals[i,j]))

  }
}

row_counts = pbsapply(1:nrow(all_intervals), function(i){
  this_row = all_intervals[i,]

  this_row_infinities = 0
  for(j in 1:length(this_row)){
    this_row_infinities = this_row_infinities + is.infinite(unlist(this_row[j]))
  }
  return(this_row_infinities)
})



# Add true mediation effects ----
all_reg_pars = make_all_reg_pars()
true_Y_coeffs = all_reg_pars$beta_Y
true_M_coeffs = all_reg_pars$beta_M
true_med_effs = get_med_effs(true_Y_coeffs["X"], true_Y_coeffs["M"], true_M_coeffs["X"])

data_true_med_effs = data.frame(true_val = true_med_effs, med_type = names(true_med_effs))



# Compute coverage rates ----
all_cover_rates = dplyr::full_join(all_intervals, data_true_med_effs, by = "med_type") %>%
  dplyr::mutate(cover = (lcl < true_val) & (true_val < ucl)) %>%
  dplyr::group_by(med_type, CI_type, boot_type, n, K, B) %>%
  dplyr::summarise(cover_rate = mean(cover)) %>%
  dplyr::ungroup()


# Investigate coverage rates ----

## Get coverage rate corresponding to a particular variable ----
cover_by_var <- function(var_name, data){
  data %>%
    dplyr::group_by(!!rlang::sym(eval(var_name))) %>%
    dplyr::summarise(cover_rate = mean(cover_rate))
}


## Top-level coverage rates ----
cover_global = mean(all_cover_rates$cover_rate)
# cover_by_group = all_cover_rates %>% dplyr::group_by(group) %>% dplyr::summarise(cover_rate = mean(cover_rate))
cover_by_med_type = cover_by_var("med_type", all_cover_rates)
cover_by_CI_type = cover_by_var("CI_type", all_cover_rates)
cover_by_boot_type = cover_by_var("boot_type", all_cover_rates)
cover_by_n = cover_by_var("n", all_cover_rates)
cover_by_K = cover_by_var("K", all_cover_rates)
cover_by_B = cover_by_var("B", all_cover_rates)


## Focus on parametric ----
par_cover_rates = dplyr::filter(all_cover_rates, boot_type == "par")

par_cover_global = mean(par_cover_rates$cover_rate)
# par_cover_by_group = cover_by_var("group", par_cover_rates)
par_cover_by_med_type = cover_by_var("med_type", par_cover_rates)
par_cover_by_CI_type = cover_by_var("CI_type", par_cover_rates)
par_cover_by_n = cover_by_var("n", par_cover_rates)
par_cover_by_K = cover_by_var("K", par_cover_rates)
par_cover_by_B = cover_by_var("B", par_cover_rates)


## Further focus on percentile intervals ----
pct_par_cover_rates = dplyr::filter(par_cover_rates, CI_type == "pct")

pct_par_cover_global = mean(pct_par_cover_rates$cover_rate)
# pct_par_cover_by_group = cover_by_var("group", pct_par_cover_rates)
pct_par_cover_by_med_type = cover_by_var("med_type", pct_par_cover_rates)
pct_par_cover_by_n = cover_by_var("n", pct_par_cover_rates)
pct_par_cover_by_K = cover_by_var("K", pct_par_cover_rates)
pct_par_cover_by_B = cover_by_var("B", pct_par_cover_rates)



# Investigate interval widths ----
CI_info = dplyr::full_join(all_intervals, data_true_med_effs, by = "med_type") %>%
  dplyr::mutate(cover = (lcl < true_val) & (true_val < ucl), width = ucl - lcl) %>%
  dplyr::group_by(med_type, CI_type, boot_type, n, K, B) %>%
  dplyr::summarise(cover_rate = mean(cover), mean_width = mean(width), sd_width = sd(width)) %>%
  dplyr::ungroup()
