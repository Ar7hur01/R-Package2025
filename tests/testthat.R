# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(InFieldR)

test_check("InFieldR")
testthat::test_file("Z:/Windows_eagle_local/Geoanalysis/Rpackagetask/clone/R-Package2025/R/stack_shp_on_rast1.R")
Devtools::check("")
