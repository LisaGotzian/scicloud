# Test the output of getScopusMetaData

if (!require(testthat))
  install.packages("testthat")

with_mock_API({
  URL <-
    "http://api.elsevier.com/content/abstract/doi/10.1002/jid.1634"
  myAPIKey <- "testtest123123"
  
  serverResponse <-
    httr::GET(URL,
              httr::add_headers(`X-ELS-APIKey` = myAPIKey, Accept = "application/json"))
  JSON <- httr::content(serverResponse)
  
  
  test_that("check title retrieval", {
    expect_true(!is.na(
      JSON$`abstracts-retrieval-response`$coredata$`dc:title`
    ))
  })
  
  test_that("check year retrieval", {
    expect_true(!is.na(
      JSON$`abstracts-retrieval-response`$coredata$`prism:coverDate`
    ))
  })
  
  test_that("check authors retrieval", {
    expect_true(
      !is.na(
        JSON$`abstracts-retrieval-response`$coredata$`dc:creator`$author[[1]]$`preferred-name`$`ce:surname`
      )
    )
  })
  
  test_that("check volume retrieval", {
    expect_true(!is.na(
      JSON$`abstracts-retrieval-response`$coredata$`prism:volume`
    ))
  })
  
  test_that("check issue retrieval", {
    expect_true(
      !is.na(
        JSON$`abstracts-retrieval-response`$coredata$`prism:issueIdentifier`
      )
    )
  })
  
  test_that("check pages retrieval", {
    expect_true(!is.na(
      JSON$`abstracts-retrieval-response`$coredata$`prism:pageRange`
    ))
  })
  
  test_that("check citedby retrieval", {
    expect_true(!is.na(
      JSON$`abstracts-retrieval-response`$coredata$`citedby-count`
    ))
  })
  
  test_that("check Scopus-ID retrieval", {
    expect_true(!is.na(
      JSON$`abstracts-retrieval-response`$coredata$`dc:identifier`
    ))
  })
  
  #  test_that("check Publisher retrieval", {
  #    expect_true(!is.na(
  #      JSON$`abstracts-retrieval-response`$coredata$`dc:publisher`
  #    ))
  #  })
  
  test_that("check Affiliation retrieval", {
    expect_true(!is.na(
      JSON$`abstracts-retrieval-response`$affiliation$affilname
    ))
  })
  
  test_that("check Affiliation-City retrieval", {
    expect_true(!is.na(
      JSON$`abstracts-retrieval-response`$affiliation$`affiliation-city`
    ))
  })
  
  test_that("check Affiliation-Country retrieval", {
    expect_true(
      !is.na(
        JSON$`abstracts-retrieval-response`$affiliation$`affiliation-country`
      )
    )
  })
})