
# Get simulation settings from command line arguments
setting_number = as.numeric(commandArgs(trailingOnly = TRUE)[1])

all_settings = read.table("All_Parameter_Combinations.csv", sep = ",")
this_settings = all_settings[setting_number,]

n = this_settings[1]
K = this_settings[2]
B = this_settings[3]

# n=5
# K=3
# B=20
#

print(paste0("n = ", n, ", K = ", K, ", B = ", B))
