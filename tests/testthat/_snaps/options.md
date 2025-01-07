# create_option

    Code
      print(expect_s3_class(testenv[[".zephyr_options"]], "zephyr_options"))
    Output
      
      -- verbosity_level 
      Dummy verbosity in this package
      * Default: `NA_character_`
      * Option: `testenv.verbosity_level`
      * Environment: `R_TESTENV_VERBOSITY_LEVEL`
      
      -- test_option 
      test_option
      * Default: `42`
      * Option: `testenv.test_option`
      * Environment: `R_TESTENV_TEST_OPTION`

---

    Code
      print(expect_s3_class(testenv[[".zephyr_options"]][["test_option"]],
      "zephyr_option"))
    Output
      
      -- test_option 
      test_option
      * Default: `42`
      * Option: `testenv.test_option`
      * Environment: `R_TESTENV_TEST_OPTION`

---

    Code
      str(testenv[[".zephyr_options"]][["test_option"]])
    Output
      List of 4
       $ default    : num 42
       $ name       : chr "test_option"
       $ description: chr "test_option"
       $ environment: chr "testenv"
       - attr(*, "class")= chr "zephyr_option"

# zephyr options

    Code
      print(expect_length(expect_s3_class(list_options(.envir = "zephyr"),
      "zephyr_options"), 1))
    Output
      
      -- verbosity_level 
      test
      * Default: `"verbose"`
      * Option: `zephyr.verbosity_level`
      * Environment: `R_ZEPHYR_VERBOSITY_LEVEL`

# list_options - as list

    Code
      print(expect_length(expect_s3_class(list_options(.envir = testenv),
      "zephyr_options"), 2))
    Output
      
      -- verbosity_level 
      Dummy verbosity in this package
      * Default: `NA_character_`
      * Option: `testenv.verbosity_level`
      * Environment: `R_TESTENV_VERBOSITY_LEVEL`
      
      -- test_option 
      test_option
      * Default: `42`
      * Option: `testenv.test_option`
      * Environment: `R_TESTENV_TEST_OPTION`

---

    Code
      print(expect_length(expect_type(list_options(as = "params", .envir = testenv),
      "character"), 2))
    Output
      [1] "@param verbosity_level Dummy verbosity in this package. Default: `NA`."
      [2] "@param test_option test_option. Default: `42`."                        

---

    Code
      cat(expect_length(expect_type(list_options(as = "markdown", .envir = testenv),
      "character"), 1))
    Output
      ## verbosity_level
      Dummy verbosity in this package
      * Default: `NA`
      * Option: `testenv.verbosity_level`
      * Environment: `R_TESTENV_VERBOSITY_LEVEL`
      ## test_option
      test_option
      * Default: `42`
      * Option: `testenv.test_option`
      * Environment: `R_TESTENV_TEST_OPTION`

