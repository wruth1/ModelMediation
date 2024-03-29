
# Read-in results of MC study ----

n = 200
K = 10
B = 200
# num_MC_reps = 256
num_MC_reps = 200


results_prefix = paste0("./Data/boot_results_n=", n, "_K=", K, "_B=", B, "_M=", num_MC_reps, "/")

file_names = list.files(results_prefix)

all_boot_results_list = purrr::map(file_names, function(this_name){
  load(paste0(results_prefix, this_name))
  this_MC_iter = as.numeric(stringr::str_extract(this_name, "\\d+"))
  this_boot_results$MC_iter = this_MC_iter

  return(this_boot_results)
})
all_boot_results = purrr::list_rbind(all_boot_results_list)


# Add true mediation effects ----
all_reg_pars = make_all_reg_pars()
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

pct_par_cover_rates = dplyr::filter(pct_cover_rates, boot_type != "npar")


pct_par_cover_global = mean(pct_par_cover_rates$cover_rate)
pct_par_cover_by_group = cover_by_var("group", pct_par_cover_rates)
pct_par_cover_by_med_type = cover_by_var("med_type", pct_par_cover_rates)


