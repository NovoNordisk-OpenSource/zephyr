# Test environment setup
test_env <- new.env()

# Tests for option_spec_pkg
test_that("opt_source_pkg identifies correct source", {
  # Setup
  test_env <- new.env()
  define_option_pkg("test_option", default = 42, envir = test_env, print_spec = FALSE)

  # Test default
  expect_equal(
    opt_source_pkg(test_env$.options$test_option, envir = "zephyr"),
    "default",
    info = "Should be 'default' when no option or envvar is set"
  )

  # Test package-specific global option
  options(zephyr.test_option = 100)
  expect_equal(
    opt_source_pkg(test_env$.options$test_option, envir = "zephyr"),
    "option",
    info = "Should be 'option' when package-specific global option is set"
  )

  # Test environment variable
  options(zephyr.test_option = NULL)
  Sys.setenv(R_TEST_OPTION = "200")
  expect_equal(
    opt_source_pkg(test_env$.options$test_option, envir = "zephyr"),
    "envvar",
    info = "Should be 'envvar' when environment variable is set"
  )

  # Test R option
  Sys.unsetenv("R_TEST_OPTION")
  options(test_option = 300)
  expect_equal(
    opt_source_pkg(test_env$.options$test_option, envir = "zephyr"),
    "option",
    info = "Should be 'option' when R option is set"
  )

  # Test package environment
  options(test_option = NULL)
  assign("test_option", 400, envir = test_env)
  expect_equal(
    opt_source_pkg(test_env$.options$test_option, envir = test_env),
    "package",
    info = "Should be 'package' when option is set in package environment"
  )

  # Test prioritization
  options(zephyr.test_option = 100)
  Sys.setenv(R_TEST_OPTION = "200")
  options(test_option = 300)
  expect_equal(
    opt_source_pkg(test_env$.options$test_option, envir = "zephyr"),
    "option",
    info = "Should prioritize package-specific global option when multiple sources are set"
  )

  # Test non-existent option
  expect_equal(
    opt_source_pkg(list(name = "non_existent", envvar_name = "NON_EXISTENT"), envir = "zephyr"),
    "default",
    info = "Should return 'default' for non-existent options"
  )

  # Clean up
  options(zephyr.test_option = NULL)
  options(test_option = NULL)
  Sys.unsetenv("R_TEST_OPTION")
  rm(list = ls(envir = test_env), envir = test_env)
})

# Tests for opt_pkg
test_that("opt_pkg retrieves correct option value", {
  test_env <- new.env()
  define_option_pkg("test_option", default = 42, envir = test_env, print_spec = FALSE)

  expect_equal(opt_pkg("test_option", envir = test_env), 42,
    info = "Should return default when no option is set")

  options(zephyr.test_option = 100)
  expect_equal(opt_pkg("test_option", envir = test_env), 100,
    info = "Should return package-specific global option value")

  Sys.setenv(R_TEST_OPTION = "200")
  expect_equal(opt_pkg("test_option", envir = test_env), "200",
    info = "Should prioritize environment variable")

  # Clean up
  options(zephyr.test_option = NULL)
  Sys.unsetenv("R_TEST_OPTION")
})

# Tests for get_option_spec_pkg
test_that("get_option_spec_pkg retrieves correct specification", {
  define_option_pkg("test_option", default = 42, desc = "Test description", envir = test_env, print_spec = FALSE)

  spec <- get_option_spec_pkg("test_option", envir = test_env)
  expect_s3_class(spec, "option_spec_pkg")
  expect_equal(spec$name, "test_option")
  expect_equal(spec$expr, 42)
  expect_equal(spec$desc, "Test description")
})

# Tests for opts_pkg
test_that("opts_pkg retrieves all options correctly", {
  # Create a new environment for testing
  test_env <- new.env(parent = emptyenv())

  # Define options in the test environment
  define_option_pkg("test_option1", default = 42, envir = test_env, print_spec = FALSE)
  define_option_pkg("test_option2", default = "hello", envir = test_env, print_spec = FALSE)

  # Test retrieving all options
  all_opts <- opts_pkg(envir = test_env)
  expect_equal(length(all_opts), 2)
  expect_equal(all_opts$test_option1, 42)
  expect_equal(all_opts$test_option2, "hello")

  # Test retrieving only option names
  opt_names <- opts_pkg(envir = test_env, names_only = TRUE)
  expect_equal(sort(opt_names), c("test_option1", "test_option2"))

  # Test retrieving full option specifications
  full_opts <- opts_pkg(envir = test_env, full = TRUE)
  expect_true(inherits(full_opts$test_option1, "option_spec_pkg"))
  expect_true(inherits(full_opts$test_option2, "option_spec_pkg"))

  # Clean up
  rm(list = ls(envir = test_env), envir = test_env)
})

# Tests for as_roxygen_docs_pkg
test_that("as_roxygen_docs_pkg generates correct documentation", {
  # Create a new environment for testing
  test_env <- new.env(parent = emptyenv())

  # Define the option in the test environment
  define_option_pkg("test_option", default = 42, desc = "Test description", envir = test_env, print_spec = FALSE)

  # Call the function with the test environment
  docs <- as_roxygen_docs_pkg(test_env)

  # Check the generated documentation
  expect_true(any(grepl("@section Options:", docs)))
  expect_true(any(grepl("\\\\code\\{test_option\\}", docs)))
  expect_true(any(grepl("Test description", docs)))
  expect_true(any(grepl("Default: \\\\preformatted\\{", docs)))
  expect_true(any(grepl("42", docs)))
  expect_true(any(grepl("Option: \\\\code\\{.*\\.test_option\\}", docs)))
  expect_true(any(grepl("Environment variable: \\\\code\\{R_TEST_OPTION\\}", docs)))

  # Print the docs for debugging
  cat("\nGenerated docs:\n")
  cat(paste(docs, collapse = "\n"))
})

# Tests for as_params_pkg
test_that("as_params_pkg generates correct parameter descriptions", {
  # Create a new environment for testing
  test_env <- new.env(parent = emptyenv())

  # Define the option in the test environment
  define_option_pkg("test_option", default = 42, desc = "Test description", envir = test_env, print_spec = FALSE)

  # Call as_params_pkg with the test environment
  params <- as_params_pkg(envir = test_env)

  # Check the generated parameter descriptions
  expect_true(any(grepl("@param test_option Test description", params)))
  expect_true(any(grepl("Default: 42", params)))

  # Check that the option name and environment variable are included
  expect_true(any(grepl("Option: ", params)))
  expect_true(any(grepl("Environment variable: R_TEST_OPTION", params)))

  # Print the params for debugging
  cat("\nGenerated params:\n")
  cat(paste(params, collapse = "\n"))
})

# Tests for envvar_is_true_pkg
test_that("envvar_is_true_pkg correctly interprets truthy values", {
  # Create a new environment for testing
  test_env <- new.env(parent = emptyenv())

  # Define a test option in the environment
  define_option_pkg("test_option", default = FALSE, desc = "Test option", envir = test_env, print_spec = FALSE)

  is_true <- envvar_is_true_pkg(envir = test_env)

  Sys.setenv(R_TEST_OPTION = "true")
  expect_true(is_true("test_option"))

  Sys.setenv(R_TEST_OPTION = "false")
  expect_false(is_true("test_option"))

  Sys.setenv(R_TEST_OPTION = "1")
  expect_true(is_true("test_option"))

  # Test with option value (not environment variable)
  Sys.unsetenv("R_TEST_OPTION")
  assign("test_option", TRUE, envir = test_env)
  expect_true(is_true("test_option"))

  assign("test_option", FALSE, envir = test_env)
  expect_false(is_true("test_option"))

  # Clean up
  Sys.unsetenv("R_TEST_OPTION")
  rm("test_option", envir = test_env)
})

# Tests for envvar_str_split_pkg
test_that("envvar_str_split_pkg correctly splits strings", {
  # Create a new environment for testing
  test_env <- new.env(parent = emptyenv())

  # Create split functions
  split_comma <- envvar_str_split_pkg(envir = test_env)
  split_pipe <- envvar_str_split_pkg(delim = "|", envir = test_env)

  # Test with environment variables
  Sys.setenv(R_TEST_OPTION = "a,b,c")
  expect_equal(split_comma("test_option"), c("a", "b", "c"))

  Sys.setenv(R_TEST_OPTION = "x|y|z")
  expect_equal(split_pipe("test_option"), c("x", "y", "z"))

  # Test with options set in the environment
  Sys.unsetenv("R_TEST_OPTION")
  assign("test_option", "1,2,3", envir = test_env)
  expect_equal(split_comma("test_option"), c("1", "2", "3"))

  assign("test_option", "red|green|blue", envir = test_env)
  expect_equal(split_pipe("test_option"), c("red", "green", "blue"))

  # Test with numeric values
  assign("test_option", 42, envir = test_env)
  expect_equal(split_comma("test_option"), "42")

  # Test with empty or NULL values
  Sys.unsetenv("R_TEST_OPTION")
  rm("test_option", envir = test_env)
  expect_null(split_comma("test_option"))

  assign("test_option", "", envir = test_env)
  expect_null(split_comma("test_option"))

  assign("test_option", NULL, envir = test_env)
  expect_null(split_comma("test_option"))

  # Test with single value (no splitting)
  Sys.setenv(R_TEST_OPTION = "single")
  expect_equal(split_comma("test_option"), "single")

  Sys.unsetenv("R_TEST_OPTION")
  assign("test_option", "lone", envir = test_env)
  expect_equal(split_comma("test_option"), "lone")

  # Clean up
  Sys.unsetenv("R_TEST_OPTION")
  rm(list = ls(envir = test_env), envir = test_env)
})
