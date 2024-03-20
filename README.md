
<!-- README.md is generated from README.Rmd. Please edit that file -->

# zephyr

<!-- badges: start -->

[![R-CMD-check](https://github.com/NN-OpenSource/zephyr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NN-OpenSource/zephyr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This package is a template for future R packages developed by the ATMOS
team.

The template sets branch policies, package development standards, and
GitHub action workflows. For details see below:

To create a new package you have two options:

1.  Press “Use this template” to create a new repository based on this
    template. Settings are not copied, and have to be manually set or
    set using the following functions from your editor:

    1.  `use_atmos_rulesets()`: Copies rule sets for branch protection
        etc.
    2.  `use_atmos_pages()`: Enables GitHub pages
    3.  `use_atmos_settings()`: Copies general settings

2.  Use the R function `create_atmos_package()` from your editor to
    create a new repository based on this template. By default this also
    sets all settings to be equal to the settings applied to this
    template repository.

## Branch policies

The following applies to the `main` branch:

- Requires a pull request before merging
- Pull requests has to be approved by 2 reviewers
- Pull requests has to be reviewed by at least one code owner
- All conversations on code must be resolved before a pull request can
  be merged
- When merging pull requests only merge and squash commits are enabled
- TODO: Requirement of linked issues

## Standards

- Set licence to Apache licence version 2.0 with Novo Nordisk A/S as the
  copyright holder. Check that year is correct!
- Use a README.Rmd
- Use `testthat` for testing
- Use `pkgdown` to create a documentation webpage
- Setup `lintr` with default style

## GitHub actions

- Checks:
  - styling: MegaLinter (calls `lintr` for R scripts)
  - updated documentation
  - updated README
- R CMD Check
- Test coverage
- Pkgdown webpage hosted on GitHub pages. Pags for pull requests are
  also hosted under “base-url/dev/PR-number”.
- Upon creation a pull request is created with all package specific
  files updated to use the new package name.

## Design (todo)

- Some great design guide to be followed and include some awesome CSS
