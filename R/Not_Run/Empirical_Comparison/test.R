
setting_number = as.numeric(commandArgs(trailingOnly = TRUE)[1]) 

load("some_par_combinations.RData")  
this_par_comb = some_pars[setting_number,]  


print(setting_number)
print(this_par_comb)
