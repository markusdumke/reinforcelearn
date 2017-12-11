# # check package
#
# # devtools check (Rd, vignettes, tests, examples, ...)
# devtools::check()
#
# # check spelling
# devtools::spell_check()
#
# # linting
# source("tests/testthat/helper_lint.R")
# lintr::lint_package(linters = linters)
# # fixme: change linter to allow UpperCamelCase for R6 class names
#
# # update readme (write sessionInfo)
# knitr::knit("README.Rmd")
#
# # update pkgdown website
# pkgdown::build_site()
#
# # run files in examples/
