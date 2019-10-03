# Author: Matthias Nachtmann, Jan. 2017, L??neburg @workgroup of Henrik von Wehrden at Leuphana University

# 24.11. error messages are now given by a function which reduces the code immensely comments to better
# understand the function.  replaced print() with cat() 15.1. added ordinationFunction = FALSE to
# getScopusMetaData()


# This Scopus search resource allows for the submission of Boolean queries into the Scopus index, retrieving
# relevant result metadata in a user-specific text formats.' (From
# https://dev.elsevier.com/documentation/ScopusSearchAPI.wadl) API Key Settings:
# https://dev.elsevier.com/api_key_settings.html GET request methods:
# https://dev.elsevier.com/documentation/ScopusSearchAPI.wadl#simple The default value for countIncrement is
# set to 25 (-> maximum for standard users). Subscribers can raise the limit up to 200 to increase the speed
# of the function.  maxResults defines the maximum amount of results that are parsed from the server. The
# limit of Scopus is at around 5000 (in 2017)



#' @title searchScopus
#'
#' @description This function accepts an search string in URL format and an
#'     Elsevier API Key and returns the DOI numbers of all search results as
#'     a vector for further processing.
#'
#' @author Matthias Nachtmann, \email{matthias.nachtmann@@stud.leuphana.de},
#'     Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de}, Prabesh Dhakal,
#'     \email{prabesh.dhakal@@stud.leuphana.de}
#' @param searchString The search string you want to ask the server. See the
#'     Scopus API (\url{https://dev.elsevier.com/}) for details.
#' @param maxResults The maximum amount of accepted search results. Usually,
#'     Scopus does not provide more than 5000 results.
#' @param countIncrement The number of resuts per GET request. A private user
#'     can't exeed 25 per request. If you are inside an subscribed IP range,
#'     you can use the maximum of 200 per request.
#' @param myAPIKey Your private Elsevier API key for the server communication.
#'     You can request one at \url{https://dev.elsevier.com/}.
#' @return A data frame containing the DOI numbers and Scopus-IDs of the search
#'     results, as well as some placeholder columns.
#' @export
#' @examples \dontrun{
#' DOInumbers <- searchScopus('TITLE-ABS-KEY(sustainability) AND PUBYEAR > 2009',
#'     '1234567890ABCDEF', maxResults = 160, countIncrement = 20)
#' DOInumbers}
searchScopus <- function(searchString, myAPIKey, maxResults = 10, countIncrement = 200) {


    #### PHASE I: GET THE DOIs and SCOPUS IDs OF THE SEARCH RESULT ####

    # percent-encode the search string
    searchString <- utils::URLencode(searchString)

    # initialize an empty results object
    searchResults <- NULL

    start <- 1
    end <- maxResults

    # initialize the progress bar utility
    pb <- utils::txtProgressBar(min = start, max = end, style = 3)

    # a function that returns a custom error message
    errorMessage <- function(cond) {
        message(paste0("Error in while retrieving the title - DOI: ", intermediateSearchResults[i, "DOI"], ", Scopus-ID: ",
            intermediateSearchResults[i, "Scopus-ID"]))
        message("Here's the original error message:")
        message(cond)
        # Choose a return value in case of error
        return(NA)
    }
    # a function that returns a custom warning message
    warningMessage <- function(cond) {
        message(paste0("Warning in while retrieving the title - DOI: ", intermediateSearchResults[i, "DOI"],
            ", Scopus-ID: ", intermediateSearchResults[i, "Scopus-ID"]))
        message("Here's the original warning message:")
        message(cond)
        # Choose a return value in case of warning
        return(NULL)
    }

    while (start < end) {

        # create the url that is sent as a GET request
        URL <- paste0("http://api.elsevier.com/content/search/scopus?query=", searchString, "&count=", countIncrement,
            "&start=", start)

        # response from the server (stored in JSON format)
        serverResponse <- httr::GET(URL, httr::add_headers(`X-ELS-APIKey` = myAPIKey, Accept = "application/json"))

        # store the JSON content from the response
        JSON <- httr::content(serverResponse)

        # ???
        start = start + countIncrement

        numberOfEntries = maxResults  #length(JSON$`search-results`$entry)
        # numberOfEntries = length(JSON$`search-results`$entry) # ideally, this should work

        # initialize a matrix that stores the results (more efficient this way)
        intermediateSearchResults <- matrix(NA, nrow = numberOfEntries, ncol = 20)
        # assign column header names to the matrix where we store the results
        colnames(intermediateSearchResults) <- c("Title", "Year", "Month", "Day", "Authors", "Journal", "Volume",
            "Issue", "Pages", "CitedBy", "CitationPerYear", "DOI", "Scopus-ID", "Publisher", "Affiliation",
            "Affiliation-City", "Affiliation-Country", "FileName", "Abstract", "FullText")

        # let users what is happening at this stage (assissted with progress bar later)
        cat("Accessing DOIs and Scopus IDs of the search result...\n")

        # attempt to grab the contents that we are interested in
        for (i in 1:numberOfEntries) {

            # grab the DOI
            intermediateResultDOI <- tryCatch({
                JSON$`search-results`$entry[[i]]$`prism:doi`
            }, error = errorMessage, warning = warningMessage)

            # Grab the scopus ID
            intermediateResultScopusID <- tryCatch({
                intermediateResultScopusID <- JSON$`search-results`$entry[[i]]$`dc:identifier`
                intermediateResultScopusID <- sub("SCOPUS_ID:", "", intermediateResultScopusID)
            }, error = errorMessage, warning = warningMessage)

            # store DOI and Scopus ID
            intermediateSearchResults[i, "DOI"] <- if (length(intermediateResultDOI) > 0) {
                intermediateResultDOI
            } else {
                NA
            }
            intermediateSearchResults[i, "Scopus-ID"] <- if (length(intermediateResultScopusID) > 0) {
                intermediateResultScopusID
            } else {
                NA
            }

        }

        # add intermediateSearchResults to searchResults as a new row
        searchResults <- rbind(searchResults, intermediateSearchResults)


        # show progress Bar
        utils::setTxtProgressBar(pb, start)
    }

    # check for redundant entries
    searchResults <- unique(searchResults)
    # assign unique ID to the rows to avoid any collisions along the way
    searchResults <- cbind(searchResults, ID = 1:nrow(searchResults))

    # this ensures that the progress bar always ends with 100% (which might not happen due to possible
    # missmatches between countIncrement and maxResults)
    utils::setTxtProgressBar(pb, end)
    close(pb)  # close progress bar

    return(searchResults)
}



















#' @title getScopusMetaData
#'
#' @description takes a Data Frame of DOI numbers and/or Scopus-IDs (like
#'     \code{\link[ginko]{searchScopus}}) and downloads the abstracts and
#'     titles matching those numbers and returning a matrix containing both.
#'
#' @author Matthias Nachtmann, \email{matthias.nachtmann@@stud.leuphana.de},
#'     Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de}, Prabesh Dhakal,
#'     \email{prabesh.dhakal@@stud.leuphana.de}
#' @param searchResults a Data Frame produced by the functions
#'     \code{\link[ginko]{searchScopus}} or
#'     \code{\link[ginko]{createTextMatrixFromPDF}}
#' @param myAPIKey Your private Elsevier API key for the server communication.
#'     You can request one at \url{https://dev.elsevier.com/user/login}
#' @param ordinationFunction FALSE by default
#' @return A data frame with all meta data of the articles provided to the function
#'
#' @examples
#' \dontrun{
#'     processedMetaMatrix$MetaMatrix <- getScopusMetaData(processedMetaMatrix$MetaMatrix, myAPIKey)
#'     }
#' @export
getScopusMetaData <- function(searchResults, myAPIKey, ordinationFunction = FALSE) {

    #### PHASE II: GET METADATA OF THE RESOURCES FOR WHICH WE COULD GET THE DOIs/SCOPUS IDs ####
    metaMatrix <- searchResults


    # a function that returns a custom error message
    errorMessage <- function(cond) {
        message(paste0("Error in while retrieving the title - DOI: ", metaMatrix[i, "DOI"], ", Scopus-ID: ",
            metaMatrix[i, "Scopus-ID"]))
        message("Here's the original error message:")
        message(cond)
        # Choose a return value in case of error
        return(NA)
    }
    # a function that returns a custom warning message
    warningMessage <- function(cond) {
        message(paste0("Warning in while retrieving the title - DOI: ", metaMatrix[i, "DOI"], ", Scopus-ID: ",
            metaMatrix[i, "Scopus-ID"]))
        message("Here's the original warning message:")
        message(cond)
        # Choose a return value in case of warning
        return(NULL)
    }
    # start <- 1 # has already been set above ## remove this line

    # get a new end value as our result matrix might have been smaller than we intended
    start <- 1
    end <- nrow(metaMatrix)

    # let users know what is happening at this stage
    cat("Accessing the Meta-data of the search result...\n")
    # reset the progress bar (using new `end` value)
    pb <- utils::txtProgressBar(min = start, max = end, style = 3)


    # extract values for each resource whose DOI or SCOPUS ID is known
    for (i in start:end) {

        # uses the DOI to get the paper if DOI exists
        if (!is.na(metaMatrix[i, "DOI"])) {
            URL <- paste0("http://api.elsevier.com/content/abstract/doi/", metaMatrix[i, "DOI"])
            # uses Scopus ID to get the paper if DOI does not exist
        } else {
            URL <- paste0("http://api.elsevier.com/content/abstract/scopus_id/", metaMatrix[i, "Scopus-ID"])
        }

        # to interact with Scopus server
        serverResponse <- httr::GET(URL, httr::add_headers(`X-ELS-APIKey` = myAPIKey, Accept = "application/json"))
        JSON <- httr::content(serverResponse)


        # It uses find first, because find all caused matrix joining errors. ???  sometimes abstracts were present
        # twice in an xml response ???

        # retrieve the TITLE of the paper
        intermediateResultTitle <- tryCatch({
            JSON$`abstracts-retrieval-response`$coredata$`dc:title`
        }, error = errorMessage, warning = warningMessage)

        # retrieve the DESCRIPTION of the paper
        intermediateResultAbstract <- tryCatch({
            JSON$`abstracts-retrieval-response`$coredata$`dc:description`
        }, error = errorMessage, warning = warningMessage)

        # retrieve the DATE OF PUBLICATION of the paper ## check to see if this is correct
        intermediateResultJournalDate <- tryCatch({
            JSON$`abstracts-retrieval-response`$coredata$`prism:coverDate`
        }, error = errorMessage, warning = warningMessage)

        # retrieve the NAME OF PUBLICATION where the paper was published
        intermediateResultJournal <- tryCatch({
            JSON$`abstracts-retrieval-response`$coredata$`prism:publicationName`
        }, error = errorMessage, warning = warningMessage)

        # retrieve the VOLUME NUMBER of the journal/other publication
        intermediateResultVolume <- tryCatch({
            JSON$`abstracts-retrieval-response`$coredata$`prism:volume`
        }, error = errorMessage, warning = warningMessage)

        # retrieve the ISSUE NUMBER of the journal/other publication
        intermediateResultIssue <- tryCatch({
            JSON$`abstracts-retrieval-response`$coredata$`prism:issueIdentifier`
        }, error = errorMessage, warning = warningMessage)

        # retrieve the PAGE NUMBERS where the paper appears on the journal/other publication
        intermediateResultPages <- tryCatch({
            JSON$`abstracts-retrieval-response`$coredata$`prism:pageRange`
        }, error = errorMessage, warning = warningMessage)

        # retrieve the NUMBER OF CITATIONS the paper has
        intermediateResultCitedBy <- tryCatch({
            JSON$`abstracts-retrieval-response`$coredata$`citedby-count`
        }, error = errorMessage, warning = warningMessage)

        # retrieve the DOI
        intermediateResultDOI <- tryCatch({
            JSON$`abstracts-retrieval-response`$coredata$`prism:doi`
        }, error = errorMessage, warning = warningMessage)

        # retrieve the PUBLISHER
        intermediateResultPublisher <- tryCatch({
            JSON$`abstracts-retrieval-response`$coredata$`dc:publisher`
        }, error = errorMessage, warning = warningMessage)

        # retrieve the SCOPUS ID
        intermediateResultScopusID <- tryCatch({
            intermediateResultScopusID <- JSON$`abstracts-retrieval-response`$coredata$`dc:identifier`
            intermediateResultScopusID <- sub("SCOPUS_ID:", "", intermediateResultScopusID)
        }, error = errorMessage, warning = warningMessage)

        # retrieve the FULL NAME OF AUTHORS
        intermediateResultAuthors <- tryCatch({
            intermediateResultAuthors <- sapply(JSON$`abstracts-retrieval-response`$authors$author, function(x) x$`ce:surname`)
            intermediateResultAuthors <- paste(intermediateResultAuthors, collapse = ", ")
        }, error = errorMessage, warning = warningMessage)

        # retrieve the (INSTITUTIONAL) AFFILIATION OF THE AUTHORS
        intermediateResultAffiliation <- tryCatch({
            if (!is.null(JSON$`abstracts-retrieval-response`$affiliation$affilname)) {
                intermediateResultAffiliation <- JSON$`abstracts-retrieval-response`$affiliation$affilname
            } else {
                intermediateResultAffiliation <- sapply(JSON$`abstracts-retrieval-response`$affiliation, function(x) x$affilname)
            }
            intermediateResultAffiliation <- paste(intermediateResultAffiliation, collapse = ", ")
        }, error = errorMessage, warning = warningMessage)

        # retrieve the CITY THAT THE AUTHORS' INSTUTIONS ARE AFFILIATED TO
        intermediateResultAffiliationCity <- tryCatch({
            if (!is.null(JSON$`abstracts-retrieval-response`$affiliation$`affiliation-city`)) {
                intermediateResultAffiliationCity <- JSON$`abstracts-retrieval-response`$affiliation$`affiliation-city`
            } else {
                intermediateResultAffiliationCity <- sapply(JSON$`abstracts-retrieval-response`$affiliation,
                  function(x) x$`affiliation-city`)
            }
            if (!is.null(intermediateResultAffiliationCity)) {
                intermediateResultAffiliationCity <- paste(intermediateResultAffiliationCity, collapse = ", ")
            } else {
                NA
            }
        }, error = errorMessage, warning = warningMessage)

        # retrieve the COUNTRY OF AFFILIATION OF THE AUTHORS
        intermediateResultAffiliationCountry <- tryCatch({
            if (!is.null(JSON$`abstracts-retrieval-response`$affiliation$`affiliation-country`)) {
                intermediateResultAffiliationCountry <- JSON$`abstracts-retrieval-response`$affiliation$`affiliation-country`
            } else {
                intermediateResultAffiliationCountry <- sapply(JSON$`abstracts-retrieval-response`$affiliation,
                  function(x) x$`affiliation-country`)
            }
            if (!is.null(intermediateResultAffiliationCountry)) {
                intermediateResultAffiliationCountry <- paste(intermediateResultAffiliationCountry, collapse = ", ")
            } else {
                NA
            }

        }, error = errorMessage, warning = warningMessage)


        # The following lines do this: VISIT each row of each column of the matrix and: CHECK if the length of the
        # content is bigger than zero IF the length of the content is greater than zero: it doesn't do anything ELSE
        # IF the length of the content = 0: it puts NA into the cell Besides that, there are three cases where date
        # formatting is performed

        metaMatrix[i, "Title"] <- if (length(intermediateResultTitle) > 0) {
            intermediateResultTitle
        } else {
            NA
        }
        metaMatrix[i, "Abstract"] <- if (length(intermediateResultAbstract) > 0) {
            intermediateResultAbstract
        } else {
            NA
        }
        metaMatrix[i, "Journal"] <- if (length(intermediateResultJournal) > 0) {
            intermediateResultJournal
        } else {
            NA
        }
        metaMatrix[i, "Year"] <- if (length(intermediateResultJournalDate) > 0) {
            suppressWarnings(as.numeric(format(as.Date(intermediateResultJournalDate, origin = "1900-01-01"),
                "%Y")))
        } else {
            NA
        }
        metaMatrix[i, "Month"] <- if (length(intermediateResultJournalDate) > 0) {
            suppressWarnings(as.numeric(format(as.Date(intermediateResultJournalDate, origin = "1900-01-01"),
                "%m")))
        } else {
            NA
        }
        metaMatrix[i, "Day"] <- if (length(intermediateResultJournalDate) > 0) {
            suppressWarnings(as.numeric(format(as.Date(intermediateResultJournalDate, origin = "1900-01-01"),
                "%d")))
        } else {
            NA
        }
        metaMatrix[i, "Volume"] <- if (length(intermediateResultVolume) > 0) {
            intermediateResultVolume
        } else {
            NA
        }
        metaMatrix[i, "Issue"] <- if (length(intermediateResultIssue) > 0) {
            intermediateResultIssue
        } else {
            NA
        }
        metaMatrix[i, "Pages"] <- if (length(intermediateResultPages) > 0) {
            intermediateResultPages
        } else {
            NA
        }
        metaMatrix[i, "DOI"] <- if (length(intermediateResultDOI) > 0) {
            intermediateResultDOI
        } else {
            NA
        }
        metaMatrix[i, "CitedBy"] <- if (length(intermediateResultCitedBy) > 0) {
            intermediateResultCitedBy
        } else {
            NA
        }
        metaMatrix[i, "Publisher"] <- if (length(intermediateResultPublisher) > 0) {
            intermediateResultPublisher
        } else {
            NA
        }
        metaMatrix[i, "Scopus-ID"] <- if (length(intermediateResultScopusID) > 0) {
            intermediateResultScopusID
        } else {
            NA
        }
        metaMatrix[i, "Authors"] <- if (length(intermediateResultAuthors) > 0) {
            intermediateResultAuthors
        } else {
            NA
        }
        metaMatrix[i, "Affiliation"] <- if (length(intermediateResultAffiliation) > 0) {
            intermediateResultAffiliation
        } else {
            NA
        }
        metaMatrix[i, "Affiliation-City"] <- if (length(intermediateResultAffiliationCity) > 0) {
            intermediateResultAffiliationCity
        } else {
            NA
        }
        metaMatrix[i, "Affiliation-Country"] <- if (length(intermediateResultAffiliationCountry) > 0) {
            intermediateResultAffiliationCountry
        } else {
            NA
        }


        # calculate citation per year based on the data we have collected
        CitationPerYear <- if ((!is.na(metaMatrix[i, "Year"])) & (!is.na(metaMatrix[i, "CitedBy"])) & (as.numeric(format(Sys.time(),
            "%Y")) - as.numeric(metaMatrix[i, "Year"]) > 0)) {
            suppressWarnings(as.numeric(metaMatrix[i, "CitedBy"])/(as.numeric(format(Sys.time(), "%Y")) - as.numeric(metaMatrix[i,
                "Year"])))
        } else {
            NA
        }
        # add the 'citation per year' value in the cells (put `NA` if no digit found)
        metaMatrix[i, "CitationPerYear"] <- if (!is.na(CitationPerYear)) {
            signif(CitationPerYear, digits = 2)
        } else {
            NA
        }




        # Show another progress bar (grows as `i` increases)
        utils::setTxtProgressBar(pb, i)
    }

    # close the progress bar utility once the loop has finished
    close(pb)


    # Final Changes before returning the results:

    # trim whitespace in front and behind every string inside the matrix
    metaMatrix <- apply(metaMatrix, MARGIN = c(1, 2), stringr::str_trim)
    # replacy any `''` (empty string) with an `NA`
    metaMatrix <- apply(metaMatrix, MARGIN = c(1, 2), function(x) if (!is.na(x)) {
        if ((nchar(x) == 0)) {
            NA
        } else {
            x
        }
    } else {
        x
    })
    # check for redundant entries
    metaMatrix <- unique(metaMatrix)

    cat(paste0("\nIf catching the Metadata worked, this should show you some Journal: \n'", metaMatrix[1, 6],
        "'\nIf it didn't work out, check your API key."))
    if (ordinationFunction == TRUE) {
        cat(paste0("\n###################################################################################################\n\n"))
    }

    return(metaMatrix)
}
