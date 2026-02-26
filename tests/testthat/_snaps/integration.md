# integration in new package

    Code
      expect_true(run_output_project(function() zephyr::use_zephyr(), libpath,
      testpkg))
    Output
      
      ── Setting up zephyr ───────────────────────────────────────────────────────────
      ℹ Add new options with `zephyr::create_option()`.
      ℹ And reuse their documentation within functions with `@inheritParams testpkg-options-params`.
      ℹ Run `devtools::document()` to update documentation.

---

    Code
      run_output_project(function() list.files("man"), libpath, testpkg)
    Output
      [1] "greet.Rd"                  "testpkg-options-params.Rd"
      [3] "testpkg-options.Rd"       

