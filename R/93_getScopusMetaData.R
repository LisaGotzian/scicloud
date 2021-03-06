# the second function to the word analysis with scicloud. This
#     function accepts a dataframe of DOI numbers and/or Scopus-IDs.
#     It downloads article metadata like title, author and year and returns
#     the input dataframe filled with that metadata. Make sure to be connected
#     to your institution's network (e.g. via VPN).
#
# @param metaMatrix a dataframe generated
#     either from PDFs with \code{\link{createTextMatrixFromPDF}} or by
#     searching Scopus with \code{\link{searchScopus}}
# @param myAPIKey your private Elsevier API key for communicating with the
#     API. You can request one at \url{https://dev.elsevier.com/index.jsp}.
# @param long_msg logical variable to whether print long message or not
# @return A dataframe with metadata of all articles provided to the function.
#
getScopusMetaData <- function(metaMatrix,
                              myAPIKey = NA,
                              long_msg = FALSE) {
  Check <- ArgumentCheck::newArgCheck()
  if (is.na(myAPIKey)) {
    ArgumentCheck::addError(
      msg = "Please input your API key from Elsevier!",
      argcheck = Check
    )
  }
  ArgumentCheck::finishArgCheck(Check)
  #### PHASE II: GET METADATA OF THE RESOURCES FOR WHICH WE COULD GET THE DOIs/SCOPUS-IDs ####

  # a function that returns a custom error message
  errorMessage <- function(cond) {
    message(
      paste0(
        "Error in while retrieving the title - DOI: ",
        metaMatrix[i, "DOI"],
        ", Scopus-ID: ",
        metaMatrix[i, "Scopus-ID"]
      )
    )
    message("Here's the original error message:")
    message(cond)
    # Choose a return value in case of error
    return(NA)
  }
  # a function that returns a custom warning message
  warningMessage <- function(cond) {
    message(
      paste0(
        "Warning in while retrieving the title - DOI: ",
        metaMatrix[i, "DOI"],
        ", Scopus-ID: ",
        metaMatrix[i, "Scopus-ID"]
      )
    )
    message("Here's the original warning message:")
    message(cond)
    # Choose a return value in case of warning
    return(NULL)
  }

  # get a new end value as our result matrix might have been smaller than we intended
  start <- 1
  end <- nrow(metaMatrix)

  # let users know what is happening at this stage
  cat("\nAccessing Scopus to update the metadata...\n")
  # reset the progress bar (using new `end` value)
  pb <- utils::txtProgressBar(min = start, max = end, style = 3)


  # extract values for each resource whose DOI or Scopus-ID is known
  for (i in start:end) {
    # uses the DOI to get the paper if DOI exists
    if (!is.na(metaMatrix[i, "DOI"])) {
      URL <-
        paste0(
          "http://api.elsevier.com/content/abstract/doi/",
          metaMatrix[i, "DOI"]
        )
      # uses Scopus ID to get the paper if DOI does not exist
    } else {
      URL <-
        paste0(
          "http://api.elsevier.com/content/abstract/scopus_id/",
          metaMatrix[i, "Scopus-ID"]
        )
    }

    # to interact with Scopus server
    serverResponse <-
      httr::GET(
        URL,
        httr::add_headers(`X-ELS-APIKey` = myAPIKey, Accept = "application/json")
      )
    JSON <- httr::content(serverResponse)


    # It uses find first, because find all caused matrix joining errors. ???  sometimes abstracts were present
    # twice in an xml response ???

    # retrieve the TITLE of the paper
    intermediateResultTitle <- tryCatch(
      {
        JSON$`abstracts-retrieval-response`$coredata$`dc:title`
      },
      error = errorMessage,
      warning = warningMessage
    )

    # we assume users have subscribed to Scopus API
    subscriber <- TRUE

    # retrieve the DESCRIPTION of the paper
    intermediateResultAbstract <- tryCatch(
      {
        JSON$`abstracts-retrieval-response`$coredata$`dc:description`
      },
      error = errorMessage,
      warning = warningMessage
    )

    # in case the user has not subscribed to Scopus API, abstracts can't be retrieved
    if (length(intermediateResultAbstract) == 0) {
      subscriber <- FALSE
    }

    # retrieve the DATE OF PUBLICATION of the paper ## check to see if this is correct
    intermediateResultJournalDate <- tryCatch(
      {
        JSON$`abstracts-retrieval-response`$coredata$`prism:coverDate`
      },
      error = errorMessage,
      warning = warningMessage
    )

    # retrieve the NAME OF PUBLICATION where the paper was published
    intermediateResultJournal <- tryCatch(
      {
        JSON$`abstracts-retrieval-response`$coredata$`prism:publicationName`
      },
      error = errorMessage,
      warning = warningMessage
    )

    # retrieve the VOLUME NUMBER of the journal/other publication
    intermediateResultVolume <- tryCatch(
      {
        JSON$`abstracts-retrieval-response`$coredata$`prism:volume`
      },
      error = errorMessage,
      warning = warningMessage
    )

    # retrieve the ISSUE NUMBER of the journal/other publication
    intermediateResultIssue <- tryCatch(
      {
        JSON$`abstracts-retrieval-response`$coredata$`prism:issueIdentifier`
      },
      error = errorMessage,
      warning = warningMessage
    )

    # retrieve the PAGE NUMBERS where the paper appears on the journal/other publication
    intermediateResultPages <- tryCatch(
      {
        JSON$`abstracts-retrieval-response`$coredata$`prism:pageRange`
      },
      error = errorMessage,
      warning = warningMessage
    )

    # retrieve the NUMBER OF CITATIONS the paper has
    intermediateResultCitedBy <- tryCatch(
      {
        JSON$`abstracts-retrieval-response`$coredata$`citedby-count`
      },
      error = errorMessage,
      warning = warningMessage
    )

    # retrieve the DOI
    intermediateResultDOI <- tryCatch(
      {
        JSON$`abstracts-retrieval-response`$coredata$`prism:doi`
      },
      error = errorMessage,
      warning = warningMessage
    )

    # retrieve the PUBLISHER
    intermediateResultPublisher <- tryCatch(
      {
        JSON$`abstracts-retrieval-response`$coredata$`dc:publisher`
      },
      error = errorMessage,
      warning = warningMessage
    )

    # retrieve the SCOPUS ID
    intermediateResultScopusID <- tryCatch(
      {
        intermediateResultScopusID <-
          JSON$`abstracts-retrieval-response`$coredata$`dc:identifier`
        intermediateResultScopusID <-
          sub("SCOPUS_ID:", "", intermediateResultScopusID)
      },
      error = errorMessage,
      warning = warningMessage
    )

    # retrieve the FULL NAME OF AUTHORS
    if (subscriber == TRUE) {
      intermediateResultAuthors <- tryCatch(
        {
          intermediateResultAuthors <-
            sapply(JSON$`abstracts-retrieval-response`$authors$author, function(x) {
              x$`preferred-name`$`ce:surname`
            })
          intermediateResultAuthors <-
            paste(intermediateResultAuthors, collapse = ", ")
        },
        error = errorMessage,
        warning = warningMessage
      )
    } else {
      # retrieve the FULL NAME OF THE FIRST AUTHOR (in case, user is not allowed to retrieve all of them)
      intermediateResultAuthors <- tryCatch(
        {
          intermediateResultAuthors <-
            JSON$`abstracts-retrieval-response`$coredata$`dc:creator`$author[[1]]$`preferred-name`$`ce:surname`
        },
        error = errorMessage,
        warning = warningMessage
      )
    }
    # retrieve the (INSTITUTIONAL) AFFILIATION OF THE AUTHORS
    intermediateResultAffiliation <- tryCatch(
      {
        if (!is.null(JSON$`abstracts-retrieval-response`$affiliation$affilname)) {
          intermediateResultAffiliation <-
            JSON$`abstracts-retrieval-response`$affiliation$affilname
        } else {
          intermediateResultAffiliation <-
            sapply(JSON$`abstracts-retrieval-response`$affiliation, function(x) {
              x$affilname
            })
        }
        intermediateResultAffiliation <-
          paste(intermediateResultAffiliation, collapse = ", ")
      },
      error = errorMessage,
      warning = warningMessage
    )

    # retrieve the CITY THAT THE AUTHORS' INSTUTIONS ARE AFFILIATED TO
    intermediateResultAffiliationCity <- tryCatch(
      {
        if (!is.null(JSON$`abstracts-retrieval-response`$affiliation$`affiliation-city`)) {
          intermediateResultAffiliationCity <-
            JSON$`abstracts-retrieval-response`$affiliation$`affiliation-city`
        } else {
          intermediateResultAffiliationCity <-
            sapply(
              JSON$`abstracts-retrieval-response`$affiliation,
              function(x) {
                x$`affiliation-city`
              }
            )
        }
        if (!is.null(intermediateResultAffiliationCity)) {
          intermediateResultAffiliationCity <-
            paste(intermediateResultAffiliationCity, collapse = ", ")
        } else {
          NA
        }
      },
      error = errorMessage,
      warning = warningMessage
    )

    # retrieve the COUNTRY OF AFFILIATION OF THE AUTHORS
    intermediateResultAffiliationCountry <- tryCatch(
      {
        if (!is.null(JSON$`abstracts-retrieval-response`$affiliation$`affiliation-country`)) {
          intermediateResultAffiliationCountry <-
            JSON$`abstracts-retrieval-response`$affiliation$`affiliation-country`
        } else {
          intermediateResultAffiliationCountry <-
            sapply(
              JSON$`abstracts-retrieval-response`$affiliation,
              function(x) {
                x$`affiliation-country`
              }
            )
        }
        if (!is.null(intermediateResultAffiliationCountry)) {
          intermediateResultAffiliationCountry <-
            paste(intermediateResultAffiliationCountry, collapse = ", ")
        } else {
          NA
        }
      },
      error = errorMessage,
      warning = warningMessage
    )


    # The following lines do this: VISIT each row of each column of the matrix and: CHECK if the length of the
    # content is bigger than zero IF the length of the content is greater than zero: it doesn't do anything ELSE
    # IF the length of the content = 0: it puts NA into the cell Besides that, there are three cases where date
    # formatting is performed

    metaMatrix[i, "Title"] <-
      if (length(intermediateResultTitle) > 0) {
        intermediateResultTitle
      } else {
        NA
      }
    metaMatrix[i, "Abstract"] <-
      if (length(intermediateResultAbstract) > 0) {
        intermediateResultAbstract
      } else {
        NA
      }
    metaMatrix[i, "Journal"] <-
      if (length(intermediateResultJournal) > 0) {
        intermediateResultJournal
      } else {
        NA
      }
    metaMatrix[i, "Year"] <-
      if (length(intermediateResultJournalDate) > 0) {
        suppressWarnings(as.numeric(format(
          as.Date(intermediateResultJournalDate, origin = "1900-01-01"),
          "%Y"
        )))
      } else {
        NA
      }
    metaMatrix[i, "Month"] <-
      if (length(intermediateResultJournalDate) > 0) {
        suppressWarnings(as.numeric(format(
          as.Date(intermediateResultJournalDate, origin = "1900-01-01"),
          "%m"
        )))
      } else {
        NA
      }
    metaMatrix[i, "Day"] <-
      if (length(intermediateResultJournalDate) > 0) {
        suppressWarnings(as.numeric(format(
          as.Date(intermediateResultJournalDate, origin = "1900-01-01"),
          "%d"
        )))
      } else {
        NA
      }
    metaMatrix[i, "Volume"] <-
      if (length(intermediateResultVolume) > 0) {
        intermediateResultVolume
      } else {
        NA
      }
    metaMatrix[i, "Issue"] <-
      if (length(intermediateResultIssue) > 0) {
        intermediateResultIssue
      } else {
        NA
      }
    metaMatrix[i, "Pages"] <-
      if (length(intermediateResultPages) > 0) {
        intermediateResultPages
      } else {
        NA
      }
    metaMatrix[i, "DOI"] <- if (length(intermediateResultDOI) > 0) {
      intermediateResultDOI
    } else {
      NA
    }
    metaMatrix[i, "CitedBy"] <-
      if (length(intermediateResultCitedBy) > 0) {
        intermediateResultCitedBy
      } else {
        NA
      }
    metaMatrix[i, "Publisher"] <-
      if (length(intermediateResultPublisher) > 0) {
        intermediateResultPublisher
      } else {
        NA
      }
    metaMatrix[i, "Scopus-ID"] <-
      if (length(intermediateResultScopusID) > 0) {
        intermediateResultScopusID
      } else {
        NA
      }
    metaMatrix[i, "Authors"] <-
      if (length(intermediateResultAuthors) > 0) {
        intermediateResultAuthors
      } else {
        NA
      }
    metaMatrix[i, "Affiliation"] <-
      if (length(intermediateResultAffiliation) > 0) {
        intermediateResultAffiliation
      } else {
        NA
      }
    metaMatrix[i, "Affiliation-City"] <-
      if (length(intermediateResultAffiliationCity) > 0) {
        intermediateResultAffiliationCity
      } else {
        NA
      }
    metaMatrix[i, "Affiliation-Country"] <-
      if (length(intermediateResultAffiliationCountry) > 0) {
        intermediateResultAffiliationCountry
      } else {
        NA
      }


    # calculate citation per year based on the data we have collected
    CitationPerYear <-
      if ((!is.na(metaMatrix[i, "Year"])) &
        (!is.na(metaMatrix[i, "CitedBy"])) &
        (as.numeric(format(
          Sys.time(),
          "%Y"
        )) - as.numeric(metaMatrix[i, "Year"]) > 0)) {
        suppressWarnings(as.numeric(metaMatrix[i, "CitedBy"]) / (as.numeric(format(
          Sys.time(), "%Y"
        )) - as.numeric(metaMatrix[
          i,
          "Year"
        ])))
      } else {
        NA
      }
    # add the 'citation per year' value in the cells (put `NA` if no digit found)
    metaMatrix[i, "CitationPerYear"] <-
      if (!is.na(CitationPerYear)) {
        signif(CitationPerYear, digits = 2)
      } else {
        NA
      }




    # Show another progress bar (grows as `i` increases)
    utils::setTxtProgressBar(pb, i)

    # API allows 9 requests per second
    Sys.sleep(1 / 9)
  }

  # close the progress bar utility once the loop has finished
  close(pb)

  # Checks whether user is classified as a subscriber by Scopus
  if (!subscriber) {
    warning(
      "You are currently not able to retrieve abstracts and information about all authors. In order to exploit the full potential of scicloud, make sure you are connected to your institution's network (e.g. via VPN) and run again."
    )
  }

  # Informs the user about the remaining quota
  cat(
    "\nRemaining quota:",
    serverResponse$headers$`x-ratelimit-remaining`,
    "requests within the next 7 days.\n"
  )
  Sys.sleep(1)

  # Final Changes before returning the results:

  # trim whitespace in front and behind every string inside the matrix
  metaMatrix <-
    apply(metaMatrix, MARGIN = c(1, 2), stringr::str_trim)
  # replacy any `''` (empty string) with an `NA`
  metaMatrix <-
    apply(metaMatrix, MARGIN = c(1, 2), function(x) {
      if (!is.na(x)) {
        if ((nchar(x) == 0)) {
          NA
        } else {
          x
        }
      } else {
        x
      }
    })
  # check for redundant entries
  metaMatrix <- unique(metaMatrix)

  # Draw 5 checks if catching the metadata worked and return the one that's not NA
  metaMatrixCheck <- vector()
  n <- nrow(metaMatrix)
  for (i in 1:5) {
    r <- abs(round(rnorm(
      1,
      mean = n / 2, sd = n / 4
    )))
    while (r > n) {
      r <- abs(round(rnorm(
        1,
        mean = n / 2, sd = n / 4
      )))
    } # to not be above n

    metaMatrixCheck[i] <- metaMatrix[r, 6]
  }

  metaMatrixCheckValue <-
    metaMatrixCheck[which(!is.na(metaMatrixCheck))]

  cat(
    paste0(
      "\nIf catching the Metadata worked, this should show you some Journal: \n'",
      metaMatrixCheckValue[1],
      "'\nIf it didn't work out, check your API key.\n"
    )
  )
  if (long_msg == TRUE) {
    cat(
      paste0(
        "\n###################################################################################################\n\n"
      )
    )
  }

  return(metaMatrix)
}
