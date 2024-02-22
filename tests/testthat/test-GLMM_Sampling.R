
n = 1000
K = 10
all_reg_pars = make_all_reg_pars()
valid_info = make_validation_data(n, K, all_reg_pars, return_REs = TRUE)
data = valid_info[["data"]]
REs = valid_info[["all_REs"]]


# Separate data into groups ----
data_by_group = split(data, data$group)


# Construct mixed effects coefficient for each group ----
fix_effs = all_reg_pars$beta_M
mix_effs_by_group = lapply(REs, function(this_REs){
  this_M_REs = this_REs$M

  this_mix_effs = fix_effs
  this_mix_effs[1] = this_mix_effs[1] + this_M_REs[1]
  this_mix_effs[2] = this_mix_effs[2] + this_M_REs[2]

  return(this_mix_effs)
})

# Compute empirical and theoretical marginal means for M in each group ----
## See the vignette `Bootstrap_Testing` for details

lin_pred_M <- function(mix_effs, X, C1, C2){
  mix_effs[1] + mix_effs[2]*X + mix_effs[3]*C1 + mix_effs[4]*C2
}

cond_mean_M <- function(mix_effs, X, C1, C2){
  boot::inv.logit(lin_pred_M(mix_effs, X, C1, C2))
}

marginal_mean_M <- function(mix_effs){
  output = 0
  for(x in 0:1){
    for(c1 in 0:1){
      for(c2 in 0:1){
        output = output + cond_mean_M(mix_effs, x, c1, c2)
      }
    }
  }
  return(output/8)
}


marginal_means_by_group = sapply(mix_effs_by_group, marginal_mean_M)# %>% sort()
group_names = paste0("G", 1:10)
M_bar_by_group = sapply(group_names, function(this_group_name){
  this_group = data_by_group[[this_group_name]]
  mean(this_group$M)
})# %>% sort()


Z_stat_by_group = purrr::map2_dbl(marginal_means_by_group, M_bar_by_group, \(marginal_mean, M_bar) {
  (marginal_mean - M_bar)/sqrt(marginal_mean*(1 - marginal_mean)/n)
})

p_vals_by_group = 2*stats::pnorm(abs(Z_stat_by_group), lower.tail = FALSE)

# sum(p_vals_by_group < 0.05)

test_that("No more than 1 group differs significantly from the theoretical mean of M",{
  expect_true(sum(p_vals_by_group < 0.05) <= 1)
})







# Test marginal probability that Y=1 ----


## Setup helper functions ----


lin_pred_Y <- function(mix_effs, M, X, C1, C2){
  mix_effs[1] + mix_effs[2]*M + mix_effs[3]*X + mix_effs[4]*C1 + mix_effs[5]*C2
}

cond_mean_Y <- function(mix_effs, M, X, C1, C2){
  boot::inv.logit(lin_pred_Y(mix_effs, M, X, C1, C2))
}


marginal_mean_Y <- function(mix_effs_Y, mix_effs_M){
  output = 0

  ## M=1
  for(x in 0:1){
    for(c1 in 0:1){
      for(c2 in 0:1){
        output = output + cond_mean_M(mix_effs_M, x, c1, c2) * cond_mean_Y(mix_effs_Y, 1, x, c1, c2)
      }
    }
  }

  ## M=0
  for(x in 0:1){
    for(c1 in 0:1){
      for(c2 in 0:1){
        output = output + (1 - cond_mean_M(mix_effs_M, x, c1, c2)) * cond_mean_Y(mix_effs_Y, 0, x, c1, c2)
      }
    }
  }

  return(output/8)
}


## Construct mixed effects coefficient for each group ----

### M-effects ----
fix_effs_M = all_reg_pars$beta_M

mix_effs_by_group_M = lapply(REs, function(this_REs){
  this_M_REs = this_REs$M

  this_mix_effs = fix_effs_M
  this_mix_effs[1] = this_mix_effs[1] + this_M_REs[1]
  this_mix_effs[2] = this_mix_effs[2] + this_M_REs[2]

  return(this_mix_effs)
})

### Y-effects ----
fix_effs_Y = all_reg_pars$beta_Y
mix_effs_by_group_Y = lapply(REs, function(this_REs){
  this_Y_REs = this_REs$Y

  this_mix_effs = fix_effs_Y
  this_mix_effs[1] = this_mix_effs[1] + this_Y_REs[1]
  this_mix_effs[2] = this_mix_effs[2] + this_Y_REs[2]
  this_mix_effs[3] = this_mix_effs[3] + this_Y_REs[3]

  return(this_mix_effs)
})



## Compute relevant objects for test ----
marginal_means_by_group_Y = purrr::map2_dbl(mix_effs_by_group_Y, mix_effs_by_group_M, marginal_mean_Y)# %>% sort()
group_names = paste0("G", 1:10)
Y_bar_by_group = sapply(group_names, function(this_group_name){
  this_group = data_by_group[[this_group_name]]
  mean(this_group$Y)
})# %>% sort()


Z_stat_by_group_Y = purrr::map2_dbl(marginal_means_by_group_Y, Y_bar_by_group, \(marginal_mean, Y_bar) {
  (marginal_mean - Y_bar)/sqrt(marginal_mean*(1 - marginal_mean)/n)
})

p_vals_by_group_Y = 2*stats::pnorm(abs(Z_stat_by_group_Y), lower.tail = FALSE)

# sum(p_vals_by_group < 0.05)

test_that("No more than 1 group differs significantly from the theoretical mean of Y",{
  expect_true(sum(p_vals_by_group_Y < 0.05) <= 1)
})


