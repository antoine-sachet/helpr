context("Normalise")
library("helpr")

test_that("Normalise L1 works", {
  mat <- matrix(1:16, nrow = 4)
  expect_equal(colSums(normalise(mat, norm = 1, by = "col")), 
               rep(1.0, 4))
  expect_equal(rowSums(normalise(mat, norm = 1, by = "row")), 
               rep(1.0, 4))
})

test_that("Normalise L2 works", {
  mat <- matrix(1:16, nrow = 4)
  expect_equal(colSums(normalise(mat, norm = 2, by = "col")^2), 
               rep(1.0, 4))
  expect_equal(rowSums(normalise(mat, norm = 2, by = "row")^2), 
               rep(1.0, 4))
})

 