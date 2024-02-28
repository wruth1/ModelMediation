
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


