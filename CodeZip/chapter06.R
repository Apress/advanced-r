## load checkpoint and required packages
library(checkpoint)
checkpoint("2016-09-04", R.version = "3.3.1")
library(testthat)
library(devtools)
library(roxygen2)
library(covr)
library(ggplot2)
options(width = 70) # only 70 characters per line


setup(
  path = "AdvancedRPkg/",
  rstudio = FALSE)

list.files("AdvancedRPkg")

list.files("AdvancedRPkg/R")

load_all("AdvancedRPkg")
ls(name = "package:AdvancedRPkg")


test("AdvancedRPkg")


# or a package in another directory
cov <- package_coverage("AdvancedRPkg")
cov

as.data.frame(cov)[1:3, c(1, 2, 3, 11)]

## roxygenize to create documentation
document("AdvancedRPkg")


## data
sampleData <- data.frame(
  Num = 1:10,
  Letter = LETTERS[1:10])
save(sampleData, file = "AdvancedRPkg/data/sampleData.rda")


## re-roxygenize to create documentation
document("AdvancedRPkg")

build("AdvancedRPkg")

check("AdvancedRPkg", cran = TRUE)


