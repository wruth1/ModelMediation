
n = 200
K = 10
all_reg_pars = make_all_reg_pars()
data = make_validation_data(n, K, all_reg_pars)


mod_Y = fit_mod_Y(data)
mod_M = fit_mod_M(data)

summary(mod_Y)
summary(mod_M)

est_coef_Y = lme4::fixef(mod_Y)
true_coef_Y = all_reg_pars$beta_Y
both_coef_Y = rbind(est_coef_Y, true_coef_Y)

est_coef_M = lme4::fixef(mod_M)
true_coef_M = all_reg_pars$beta_M
both_coef_M = rbind(est_coef_M, true_coef_M)

# Insert a column of NAs between intercept and X in both_coef_M
both_coef_M = cbind(both_coef_M[,1], NA, both_coef_M[,2:4])

all_coeffs = rbind(both_coef_Y, both_coef_M)
