all_runtime_names = list.files("Runtimes/")

load("all_par_combinations.RData")

all_checks = rep(F, length=nrow(all_pars))

for(i in 1:nrow(all_pars)){
	n = all_pars[i,"n"]
	K = all_pars[i,"K"]
	B = all_pars[i,"B"]

	this_file_name = paste0("n=", n, "_K=", K, "_B=", B, "_M=2.RData")

	all_checks[i] = (this_file_name %in% all_runtime_names)
}
