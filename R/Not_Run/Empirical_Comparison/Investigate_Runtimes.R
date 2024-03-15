
library(dplyr)

# Load parameter settings from timing study

load("R/Not_Run/Empirical_Comparison/all_par_combinations.RData")
all_pars


# Extract and store runtimes ----

runtime_prefix = "R/Not_Run/Empirical_Comparison/Runtimes/"

runtime_data = all_pars
all_runtimes = rep(0, times=nrow(runtime_data))

for(i in 1:nrow(runtime_data)){
  n = runtime_data[i, "n"]
  K = runtime_data[i, "K"]
  B = runtime_data[i, "B"]
  num_MC_reps = 2

  runtime_address = paste0(runtime_prefix, "n=", n, "_K=", K, "_B=", B, "_M=", num_MC_reps, ".RData")

  load(runtime_address)

  runtime_message = runtime$callback_msg

  run_secs = stringr::str_extract(runtime_message, "\\d+\\.\\d+") %>% as.numeric()
  all_runtimes[i] = run_secs

}


runtime_data$runtime = all_runtimes




# Runtime per bootstrap sample ----
runtime_data$runtime_per_boot = runtime_data$runtime / runtime_data$B

runtime_per_boot = runtime_data %>%
  group_by(K, n) %>%
  summarise(mean_time = mean(runtime_per_boot),
            sd_time = sd(runtime_per_boot)) %>%
  ungroup()

runtime_per_n_per_boot = runtime_per_boot %>%
  mutate(mean_per_n = mean_time / n)

runtime_per_K_per_boot = runtime_per_boot %>%
  mutate(mean_per_K = mean_time / K)


## Estimate time to run four such analyses
four_runs = runtime_data %>% mutate(four_runs = runtime*4, four_runs_hours = four_runs/3600)



# Make some plots ----
plot(runtime_data$n, runtime_data$runtime)
runtime_data %>% group_by(n) %>% summarise(mean_runtime = mean(runtime)) %>% plot(.$n, .$mean_runtime, main = "n")
runtime_data %>% group_by(B) %>% summarise(mean_runtime = mean(runtime)) %>% plot(.$B, .$mean_runtime, main = "B")
runtime_data %>% group_by(K) %>% summarise(mean_runtime = mean(runtime)) %>% plot(.$K, .$mean_runtime, main="K")



fit_full = lm(runtime ~ (n + K + B)^3, data = runtime_data)
# summary(fit_full)

fit_3 = lm(runtime ~ n:B:K, data = runtime_data)
# summary(fit_3)

fit_1 = lm(runtime ~ n + B + K, data = runtime_data)
# summary(fit_1)

fit_2 = lm(runtime ~ n:B + n:K + B:K, data = runtime_data)
# summary(fit_2)

fit_12 = lm(runtime ~ n + B + K + n:B + n:K + B:K, data = runtime_data)
# summary(fit_12)

fit_13 = lm(runtime ~ n + B + K + n:B:K, data = runtime_data)

fit_23 = lm(runtime ~ n:B + n:K + B:K + n:B:K, data = runtime_data)


anova(fit_full, fit_13)

anova(fit_13, fit_3)


predict(fit_13)




# Predict runtimes for larger datasets ----
new_K = c(3,5,10)
new_n = c(20, 50, 200, 500)
new_B = c(50, 200, 500)

new_data = expand.grid(K=new_K, n=new_n, B=new_B)
pred_secs = predict(fit_13, newdata = new_data)

pred_hours = pred_secs / 3600

data_pred_hours = cbind(new_data, pred_hours)





# Build CIs from output of timing study ----

