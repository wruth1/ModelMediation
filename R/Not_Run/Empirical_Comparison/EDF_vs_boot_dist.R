

library(gridExtra)

# EDF ----

## Load results ----
# load(file = paste0("R/Not_Run/Empirical_Comparison/sim_med_effs/n=", n, "_K=", K, "_B=", B, ".RData"))

## Make plots ----

### Fixed effects ----

data_plot = all_results %>% filter(group == "fixed")


ggplot(data_plot, aes(x = estimate)) +
  geom_histogram() +
  facet_wrap(~med_type) +
  geom_vline(aes(xintercept = truth), color = "red") +
  labs(title = "Monte Carlo Sampling Distribution - Fixed Effects", x = "Estimate")

ggplot(data_plot, aes(x = estimate)) +
  geom_histogram() +
  facet_wrap(~med_type, scales = "free") +
  geom_vline(aes(xintercept = truth), color = "red") +
  labs(title = "Monte Carlo Sampling Distribution - Fixed Effects", x = "Estimate")



### Group-specific effects ----

data_group1 = all_results %>% filter(group == "G1") %>% mutate(centered = estimate - truth) %>% filter(abs(centered) < 50)

ggplot(data_group1, aes(x = centered)) +
  geom_histogram() +
  facet_wrap(~med_type) +
  geom_vline(xintercept = 0, color = "red") +
  labs(title = "Monte Carlo Sampling Distribution - Group 1", x = "Estimate")

ggplot(data_group1, aes(x = centered)) +
  geom_histogram() +
  facet_wrap(~med_type, scales = "free") +
  geom_vline(xintercept = 0, color = "red") +
  labs(title = "Monte Carlo Sampling Distribution - Group 1", x = "Estimate")





# Boot dists ----

n = 1000
K = 9
B = 240
M = 1

arr_ind = 1
i = 1


all_results_with_truth = data.frame()

for(arr_ind in 1:10){

  load(paste0("R/Not_Run/Empirical_Comparison/Boot_Dists/boot_results_n=", n, "_K=", K, "_B=", B, "_M=", M, "/Arr_ind=", arr_ind, ",i=", i, ".RData"))


  all_results_with_truth = rbind(all_results_with_truth, results_with_truth)

}

results_with_truth = all_results_with_truth


### Fixed effects ----

data_plot_boot = results_with_truth %>% filter(group == "fixed")


ggplot(data_plot_boot, aes(x = estimate)) +
  geom_histogram() +
  # geom_density() +
  facet_grid(rows = vars(boot_type), cols = vars(med_type)) +
  geom_vline(aes(xintercept = truth), color = "red") +
  labs(title = "Monte Carlo Bootstrap Distribution - Fixed Effects", x = "Estimate")

ggplot(data_plot_boot, aes(x = estimate)) +
  geom_histogram() +
  # geom_density() +
  facet_grid(rows = vars(boot_type), cols = vars(med_type), scales = "free") +
  geom_vline(aes(xintercept = truth), color = "red") +
  labs(title = "Monte Carlo Bootstrap Distribution - Fixed Effects", x = "Estimate")


ggplot(filter(data_plot_boot, boot_type != "par"), aes(x = estimate)) +
  geom_histogram() +
  # geom_density() +
  facet_grid(rows = vars(boot_type), cols = vars(med_type), scales = "free") +
  geom_vline(aes(xintercept = truth), color = "red") +
  labs(title = "Monte Carlo Bootstrap Distribution - Fixed Effects", x = "Estimate")

### Group-specific effects ----

data_group1_boot = results_with_truth %>% filter(group == "G1") %>% mutate(centered = estimate - truth) %>% filter(abs(centered) < 20)

ggplot(data_group1_boot, aes(x = centered)) +
  # geom_histogram() +
  geom_density() +
  facet_grid(rows = vars(boot_type), cols = vars(med_type)) +
  geom_vline(xintercept = 0, color = "red") +
  labs(title = paste0("Monte Carlo Bootstrap Distribution - Group ", g), x = "Estimate")

ggplot(data_group1_boot, aes(x = centered)) +
  geom_histogram() +
  # geom_density() +
  facet_grid(rows = vars(boot_type), cols = vars(med_type), scales = "free") +
  geom_vline(xintercept = 0, color = "red") +
  labs(title = paste0("Monte Carlo Bootstrap Distribution - Group ", g), x = "Estimate")

filter(data_group1_boot, boot_type == "spar", med_type == "de")

for(g in 1:K){
  print(g)
  data_group_boot = results_with_truth %>% filter(group == paste0("G", g)) %>% mutate(centered = estimate - truth)

  plot1 = (ggplot(data_group_boot, aes(x = centered)) +
    # geom_histogram() +
    geom_density() +
    facet_wrap(~med_type) +
    geom_vline(xintercept = 0, color = "red") +
    labs(title = paste0("Monte Carlo Bootstrap Distribution - Group ", g), x = "Estimate"))

  plot2 = (ggplot(data_group_boot, aes(x = centered)) +
    # geom_histogram() +
    geom_density() +
    facet_wrap(~med_type, scales = "free") +
    geom_vline(xintercept = 0, color = "red") +
    labs(title = paste0("Monte Carlo Bootstrap Distribution - Group ", g), x = "Estimate"))


  data_group_boot_restricted = data_group_boot %>% filter(abs(centered) < 5)

  plot3 = (ggplot(data_group_boot_restricted, aes(x = centered)) +
    # geom_histogram() +
    geom_density() +
    facet_wrap(~med_type) +
    geom_vline(xintercept = 0, color = "red") +
    labs(title = paste0("Monte Carlo Bootstrap Distribution - Group ", g, ", Truncated"), x = "Estimate"))

    plot4 = (ggplot(data_group_boot_restricted, aes(x = centered)) +
    # geom_histogram() +
    geom_density() +
    facet_wrap(~med_type, scales = "free") +
    geom_vline(xintercept = 0, color = "red") +
    labs(title = paste0("Monte Carlo Bootstrap Distribution - Group ", g, ", Truncated"), x = "Estimate"))

    grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)
}
