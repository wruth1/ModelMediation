all_Ks = c(3, 5, 10)
all_ns = c(10, 50, 200)
all_Bs = c(50, 200, 500)


all_pars = expand.grid(K = all_Ks, n = all_ns, B = all_Bs)
save(all_pars, file = "all_par_combinations.RData")




some_Ks = c(2, 4, 6)
some_Ns = c(20, 100, 200)
some_Bs = c(20, 40, 60)


some_pars = expand.grid(K=some_Ks, n=some_Ns, B=some_Bs)
save(some_pars, file = "some_par_combinations.RData")
