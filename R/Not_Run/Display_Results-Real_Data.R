
library(ggplot2)

# Plot CIs by group and boot_type

# Load data ----
load(file = "Data/Real_Data_Boot_CIs.RData")

boot_CIs_par$boot_type = "par"
boot_CIs_spar$boot_type = "spar"
boot_CIs_npar$boot_type = "npar"

all_boot_CIs = rbind(boot_CIs_par, boot_CIs_spar, boot_CIs_npar)


data_plot_boot_CIs = all_boot_CIs %>% dplyr::mutate(is_fixed = (group == "fixed"), boot_CI_type = paste0(boot_type, "-", CI_type)) %>% dplyr::arrange(group)


plot_boot_CIs = ggplot(data_plot_boot_CIs, aes(y = group, group = group, color = is_fixed)) + geom_point(aes(x = estimate)) +
  geom_linerange(aes(xmin = lcl, xmax = ucl)) + facet_grid(rows = vars(boot_CI_type), cols = vars(med_type)) +
  geom_vline(xintercept = 1) + guides(color="none")
plot_boot_CIs




# Plot bootstrap distributions ----
load(file = "Data/Real_Data_Boot_Coeffs.RData")


B = max(all_boot_results$b)

## Regression coefficients ----
data_reg_coeffs_plot = tidyr::pivot_longer(all_boot_results, cols = c("X_in_Y", "M_in_Y", "X_in_M"), names_to = "coef", values_to = "estimate")
all_groups = sort(unique(data_reg_coeffs_plot$group))
all_reg_plots = list()
for(i in seq_along(all_groups)){
  this_group = all_groups[i]
  this_plot = ggplot(dplyr::filter(data_reg_coeffs_plot, group==!!this_group), aes(x = estimate)) + geom_density() + facet_grid(rows = vars(boot_type), cols = vars(coef)) + ggtitle(paste0("Group: ", this_group, ", B: ", B))
  all_reg_plots[[i]] = this_plot
}

for(i in seq_along(all_reg_plots)){
  plot(all_reg_plots[[i]])
}
