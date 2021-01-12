# Test the output of getScopusMetaData
if (!require(testthat))
  install.packages("testthat")

testMatrix <-
  getScopusMetaData(metaMatrix = createTextMatrixFromPDF(), myAPIKey = myAPIKey)

test_that("check title retrieval", {
  expect_true(any(!is.na(testMatrix[, "Title"])))
})

test_that("check year retrieval", {
  expect_true(any(!is.na(testMatrix[, "Year"])))
})

test_that("check month retrieval", {
  expect_true(any(!is.na(testMatrix[, "Month"])))
})

test_that("check day retrieval", {
  expect_true(any(!is.na(testMatrix[, "Day"])))
})

test_that("check authors retrieval", {
  expect_true(any(!is.na(testMatrix[, "Authors"])))
})

test_that("check volume retrieval", {
  expect_true(any(!is.na(testMatrix[, "Volume"])))
})

test_that("check issue retrieval", {
  expect_true(any(!is.na(testMatrix[, "Issue"])))
})

test_that("check pages retrieval", {
  expect_true(any(!is.na(testMatrix[, "Pages"])))
})

test_that("check citedby retrieval", {
  expect_true(any(!is.na(testMatrix[, "CitedBy"])))
})

test_that("check CitationPerYear retrieval", {
  expect_true(any(!is.na(testMatrix[, "CitationPerYear"])))
})

test_that("check Scopus-ID retrieval", {
  expect_true(any(!is.na(testMatrix[, "Scopus-ID"])))
})

test_that("check Publisher retrieval", {
  expect_true(any(!is.na(testMatrix[, "Publisher"])))
})

test_that("check Affiliation retrieval", {
  expect_true(any(!is.na(testMatrix[, "Affiliation"])))
})

test_that("check Affiliation-City retrieval", {
  expect_true(any(!is.na(testMatrix[, "Affiliation-City"])))
})

test_that("check Affiliation-Country retrieval", {
  expect_true(any(!is.na(testMatrix[, "Affiliation-Country"])))
})
