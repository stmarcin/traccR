library(usethis)
library(devtools)
library(roxygen2)
library(testthat)

use_readme_rmd()

use_spell_check()

use_package("data.table")

use_version()
use_r("proximity")

usethis::use_build_ignore("zzz_temporary")

usethis::use_package("stringr")

usethis::use_package("glue", "Suggests")

usethis::use_r("utils")
usethis::use_test("argument")
usethis::use_test("utils")


