
# Short list, for timing study
ns = c(20, 40, 60)
Ks = c(2, 4, 6)
Bs = c(20, 40, 60)

some_pars = expand.grid(n=ns, K=Ks, B=Bs)
save(some_pars, file="some_par_combinations.RData")




# Long list, for empirical coverage probabilities

all_ns = c(10, 50, 200)
all_Ks = c(2, 5, 10)
all_Bs = c(50, 200, 500)

all_pars = expand.grid(n=all_ns, K=all_Ks, B=all_Bs)
save(all_pars, file="all_par_combinations.RData")
