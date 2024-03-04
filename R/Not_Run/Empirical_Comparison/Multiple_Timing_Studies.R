
# Using the RStudio API for slightly more information about the running job ----

library(rstudioapi)


# rstudioapi::jobRunScript(path = "D:/William/Research/ModelMediation/R/Not_Run/Empirical_Comparison/Timing_Study.R",
#                          name = "Timing_Study.R",
#                          workingDir = "D:/William/Research/ModelMediation")



# Using callr for more flexibility in passing parameters ----

library(callr)


n = 22
K = 3
B = 10

test = callr::r_bg(function(sim_pars){
    source("D:/William/Research/ModelMediation/R/Not_Run/Empirical_Comparison/Timing_Study-2.R", local = TRUE)
  },
  args = list(list(n=n, K=K, B=B))
)


# # Check how to access printed output
# q = r_bg(function(sim_pars){
#   print(sim_pars)
# },
# args = list(list(n=n, K=K, B=B))
# )


test$read_all_output()
