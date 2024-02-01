

# This functionality is tested in test-get_med_effs_binY_binM.R

#
#
# X_in_Y = 0
# M_in_Y = 0
# X_in_M = 0
#
# trivial_med_effs = get_med_effs(X_in_Y, M_in_Y, X_in_M)
#
#
# test_that("Size of output is correct",{
#   expect_equal(length(trivial_med_effs), 3)
# })
#
# test_that("Trivial mediation effects are computed correctly",{
#   expect_equal(as.numeric(trivial_med_effs), c(1,1,1))
# })
#
# test_that("Names of mediation effects are correct",{
#   med_eff_names = names(trivial_med_effs)
#   expect_equal(med_eff_names, c("de", "ie", "te"))
# })
