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
      run_output_project(function() devtools::document(quiet = TRUE), libpath,
      testpkg)
    Output
      ℹ Loading testpkg
      ✖ testpkg-options.R:10: @details Could not resolve link to topic
        "testpkg-options" in the dependencies or base packages.
      ℹ If you haven't documented "testpkg-options" yet, or just changed its name,
        this is normal. Once "testpkg-options" is documented, this warning goes away.
      ℹ Make sure that the name of the topic is spelled correctly.
      ℹ Always list the linked package as a dependency.
      ℹ Alternatively, you can fully qualify the link with a package name.
      Writing 'NAMESPACE'
      Writing 'greet.Rd'
      Writing 'testpkg-options.Rd'
      Writing 'testpkg-options-params.Rd'
      NULL

