library(testthat)
source("xml_filter.R")

context("xml filters")
test_that("drop_childless_stations drops childless stations", {
  tree <- xmlParse(test_refl_2015)
  retain_species_catch(tree, c("171342", "172930"))
  xpath = "//m:fishstation[not(m:catchsample)]"
  xpath2 = "//m:fishstation"
  namespaces="m"
  n_fishstations_pre <- length(getNodeSet(tree, xpath2, namespaces))
  n_childless_pre <- length(getNodeSet(tree, xpath, namespaces))
  drop_childless_stations(tree)
  n_fishstations_post <- length(getNodeSet(tree, xpath2, namespaces))
  n_childess_post <- length(getNodeSet(tree, xpath, namespaces))
  
  expect_that(n_fishstations_post + n_childless_pre, equals(n_fishstations_pre))
  expect_that(n_childess_post, equals(0))
})

test_that("retain_species_catch retains catchsamples with multiple given species",{
  tree <- xmlParse(test_refl_2015)
  xpath = "//m:catchsample[@species='171342']"
  xpath2 = "//m:catchsample[@species='172930']"
  xpath3 = "//m:catchsample"
  namespaces="m"
  ns1_pre <- length(getNodeSet(tree, xpath, namespaces))
  ns2_pre <- length(getNodeSet(tree, xpath2, namespaces))
  tot_pre <- length(getNodeSet(tree, xpath3, namespaces))
  retain_species_catch(tree, c("171342", "172930"))
  ns1_post <- length(getNodeSet(tree, xpath, namespaces))
  ns2_post <- length(getNodeSet(tree, xpath2, namespaces))
  tot_post <- length(getNodeSet(tree, xpath3, namespaces))
  
  expect_that(ns1_pre + ns2_pre, equals(tot_post))
})

test_that("retain_species_catch retains catchsamples with single species",{
  tree <- xmlParse(test_refl_2015)
  xpath = "//m:catchsample[@species='171342']"
  xpath2 = "//m:catchsample[@species='172930']"
  xpath3 = "//m:catchsample"
  namespaces="m"
  ns1_pre <- length(getNodeSet(tree, xpath, namespaces))
  ns2_pre <- length(getNodeSet(tree, xpath2, namespaces))
  tot_pre <- length(getNodeSet(tree, xpath3, namespaces))
  retain_species_catch(tree, c("171342"))
  ns1_post <- length(getNodeSet(tree, xpath, namespaces))
  ns2_post <- length(getNodeSet(tree, xpath2, namespaces))
  tot_post <- length(getNodeSet(tree, xpath3, namespaces))
  
  expect_that(ns1_pre, equals(tot_post))
  expect_that(ns2_post, equals(0))
  expect_that(ns2_pre, is_more_than(ns2_post))
})

context("file handling")
test_that("apply_xml_filters finishes without errors", {
  apply_xml_filters(test_refl_2015, "/dev/stderr", c(function(tree){retain_species_catch(tree, c("171342", "172930"))}, drop_childless_stations, drop_childless_missions))
})