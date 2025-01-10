# integration in new package

    Code
      expect_true(run_output_project(function() zephyr::use_zephyr(), libpath,
      testpkg))
    Output
      
      ── Setting up zephyr ───────────────────────────────────────────────────────────
      ℹ Add new options with `zephyr::create_option()`.
      ℹ And reuse their documentation with in functions with `@inheritParams testpkg-options-params`.
      ℹ Run `devtools::document()` to update documentation.

---

    Code
      run_output_project(function() devtools::document(quiet = TRUE), libpath,
      testpkg)
    Output
      ℹ Loading testpkg
      Writing 'NAMESPACE'
      Writing 'greet.Rd'
      Writing 'testpkg-options.Rd'
      Writing 'testpkg-options-params.Rd'
      NULL

