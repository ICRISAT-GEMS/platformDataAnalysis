# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

library(usethis)
library(devtools)
library(golem)
library(git2r)
library(gitcreds)

setwd("D:/Mes Donnees/WD/R/packages/platform/platformDataAnalysis")

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/
# golem::add_module(name = "name_of_module1", with_test = TRUE) # Name of the module
# golem::add_module(name = "name_of_module2", with_test = TRUE) # Name of the module

## Add dependencies -----
use_package("SpATS")
use_package("ggplot2")
use_package("lubridate")
use_package("tidyr")

## Add helper functions ----
## Creates fct_* and utils_*
# golem::add_fct("helpers", with_test = TRUE)
# golem::add_utils("helpers", with_test = TRUE)

golem::add_fct("convert_old_unit_row_col", with_test = FALSE)
golem::add_fct("median_computation", with_test = FALSE)
golem::add_fct("plot_trend", with_test = FALSE)
golem::add_fct("outlier_boxplot_detect", with_test = FALSE)
golem::add_fct("timepoint_prop_non_missing", with_test = FALSE)
golem::add_fct("spatial_adjustment", with_test = FALSE)
golem::add_fct("add_timeNumber", with_test = FALSE)
golem::add_fct("extract_trait_data_adj_TS", with_test = FALSE)

golem::add_utils("spatial_adjustment_ij", with_test = FALSE)


## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
# usethis::use_data_raw(name = "my_dataset", open = FALSE)

# raw data produced with Unit_sector_correspondance.R
setwd("D:/Mes Donnees/WD/ICRISAT/Phenotyping/LeasyScan")
load(file = "unit_row_col_block.RData")

# (old) new
LS_unit_row_lk <- d_unit_row_col_block$rowNum
names(LS_unit_row_lk) <- d_unit_row_col_block$unit

LS_unit_col_lk <- d_unit_row_col_block$colNum
names(LS_unit_col_lk) <- d_unit_row_col_block$unit

LS_unit_block_lk <- d_unit_row_col_block$block
names(LS_unit_block_lk) <- d_unit_row_col_block$unit

# new unit
LS_new_unit_row_lk <- d_unit_row_col_block$rowNum
names(LS_new_unit_row_lk) <- d_unit_row_col_block$new_unit

LS_new_unit_col_lk <- d_unit_row_col_block$colNum
names(LS_new_unit_col_lk) <- d_unit_row_col_block$new_unit

LS_new_unit_block_lk <- d_unit_row_col_block$block
names(LS_new_unit_block_lk) <- d_unit_row_col_block$new_unit

setwd("D:/Mes Donnees/WD/R/packages/platform/platformDataAnalysis")
usethis::use_data(LS_unit_row_lk, LS_unit_col_lk, LS_unit_block_lk,
                  LS_new_unit_row_lk, LS_new_unit_col_lk, LS_new_unit_block_lk,
                  overwrite = TRUE)


## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("platformDataAnalysis")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis()
usethis::use_travis_badge()

# AppVeyor
usethis::use_appveyor()
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
