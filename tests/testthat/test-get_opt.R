# Create an environment and define a test option in it
env1 <- rlang::new_environment()
options::define_option("test_opt",
                       default = "test",
                       envir = env1)

test_that("Option can be extracted", {
  opt <- get_opt(opt_name = "test_opt",
                 env = env1)

  expect_equal(opt, "test")
})

test_that("Option can be overwritten with global option", {
  glob_opt_list <- list(atmos.test_opt = "glob_opt")

  opt <- withr::with_options(glob_opt_list,
                             get_opt(opt_name = "test_opt",
                                     global_opt_name = "atmos.test_opt",
                                     env = env1))

  expect_equal(opt, "glob_opt")
})
