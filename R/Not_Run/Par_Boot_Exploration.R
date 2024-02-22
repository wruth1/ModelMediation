


B = 2

# Import and process data ----

## Load dat.ma ----
load(paste0("Data/","CleanDataFile-Trust-Nov102023.RData"))

## Convert dat.ma to a formal data frame ----
real_data = dat.ma %>%
  dplyr::mutate(q8.pcis_sure = (q8.pcis == "sure"),
                q8.pcis_medium = (q8.pcis == "medium")) %>%
  dplyr::select(q5.fc, q4.src, q8.pcis_sure, country, q1.cc, q2.dc, q3.pc, q7.la, q8.pcis_medium, q9.edu, age_group, gender) %>%
  lapply(as.character) %>%  as.data.frame() %>%
  make_data_formal(Y_name = "q5.fc", M_name = "q4.src", X_name = "q8.pcis_sure", group_name = "country")

real_data$Y = as.factor(real_data$Y)
real_data$M = as.factor(real_data$M)

real_data %<>% dplyr::group_by(group) %>%      # Split data into groups
  dplyr::slice_sample(n = 20, replace = FALSE) %>% # Resample within each group
  dplyr::ungroup() %>%                              # Remove grouping structure
  data.frame()


mod_M = fit_mod_M_formal(real_data)
mod_Y = fit_mod_Y_formal(real_data)
