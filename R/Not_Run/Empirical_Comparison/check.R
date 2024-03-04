devtools::load_all(".")


n = 20
K = 3
B = 5

all_reg_pars = make_all_reg_pars()

data = make_validation_data(n, K, all_reg_pars)

mod_Y = fit_mod_Y_formal(data)
mod_M = fit_mod_M_formal(data)

print(mod_Y)
print(mod_M)
