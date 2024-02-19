all_Ks = c(3, 5, 10, 20)
all_ns = c(10, 50, 200)
all_Bs = c(50, 200, 500)


all_par_combs = expand.grid(K = all_Ks, n = all_ns, B = all_Bs)
write.table(all_par_combs, file = "R/Not_Run/Empirical_Comparison/All_Parameter_Combinations.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, sep=",")
