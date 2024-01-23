

# Some useful functions for package development.
# Contained inside a function because loading the package causes all scripts to be run (despite inclusion in .Rbuildignore)


some_info <- function() {
  # Basically boilerplate code. Contains most functions used for package development ----
  library(devtools)

  # Facilitates writing unit tests ----
  library(testthat)


  # Setup a new package ----

  ## Setup git and create github repo ----
  use_git()
  use_github(private = T)

  ## Choose a licence ----
  ## Many options exist. See, e.g., help page for this function
  use_mit_license()




  # File navigation ----
  # Note: Functions must be defined in .R files inside the R directory, WITHOUT ANY SUB-DIRECTORIES

  ## Create/Open R script defining current function ----
  ## If no argument, uses current location of cursor
  use_r()
  use_r("coef_vec_2_med_effs")

  ## Create/Open test file for current function ----
  ## If no argument, uses current location of cursor
  use_test()
  use_test("coef_vec_2_med_effs")




  # Use my package ----

  ## Pseudo-load the package ----
  ## Makes functions accessible, including those not exported. Used for development/testing
  load_all()

  ## Make the package available for loading the usual way ----
  ## I.e. load(my_package). Can only access exported functions
  install()
  library(my_package)
  library(ModelMediation)




  # Update backend stuff ----

  ## (Re-)Build Documentation ----
  document()

  ## Re-Build readme.md from readme.rmd ----
  ## Note: Edit the latter and let this function create the former.
  build_readme()

  ## Indicate that another package is required ----
  ## Note: use package::function() instead of just function() throughout scripts
  use_package("a_package")
  use_package("tibble")





  # Check package health ----

  ## Run unit tests ----
  test()

  ## Run devtools tests plus my unit tests ----
  check()



}
