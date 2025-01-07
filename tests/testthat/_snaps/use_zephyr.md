# use_zephyr

    Code
      use_zephyr()
    Message
      
      -- Setting up zephyr -----------------------------------------------------------
      v Adding zephyr to 'Imports' field in DESCRIPTION.
      [ ] Refer to functions with `zephyr::fun()`.
      v Writing 'R/mypkg-options.R'.
      [ ] Edit 'R/mypkg-options.R'.
      i Add new options with `zephyr::create_option()`.
      i And reuse their documentation with in functions with `@inheritParams mypkg-options-params`.
      i Run `devtools::document()` to update documentation.

---

    Code
      devtools::document()
    Message
      i Updating mypkg documentation
      i Loading mypkg
      x mypkg-options.R:3: @description refers to unavailable topic zephyr::verbosity_level.
      x mypkg-options.R:13: @param refers to unavailable topic zephyr::verbosity_level.
      Writing 'mypkg-options.Rd'
      Writing 'mypkg-options-params.Rd'

