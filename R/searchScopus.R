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
#' @param saveToWd a logical parameter whether or not to save the output of the
#'     function to the working directory. This is especially useful for later
#'     analysis steps and can be read in by using \code{\link[base]{saveRDS}}.
#' @return A data frame containing the DOI numbers and Scopus-IDs of the search
#'     results, as well as some placeholder columns.
#' @export
#' @examples \dontrun{
#' DOInumbers <- searchScopus('TITLE-ABS-KEY(sustainability) AND PUBYEAR > 2009',
#'     '1234567890ABCDEF', maxResults = 160, countIncrement = 20)
#' DOInumbers}
searchScopus <- function(searchString, myAPIKey, maxResults = 10,
                         countIncrement = 200, saveToWd = TRUE) {
  
  
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
    
    # save metaDOInumbers dataFrame to R object file to working directory & global env
    if (saveToWd == TRUE) {
      MetaDOInumbersFile <-
        paste0("metaDOInumbers", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"))
      saveRDS(searchResults, file = MetaDOInumbersFile)
      
      cat(
        paste0(
          "\nThe metaDOInumbers is now in your global environment.
          It is also saved as a file in your working directory.
          If you work with the same data again, you can skip this
          step in future analysis by reading in the file:
          \n DOInumbers <- readRDS(file= '",
          MetaDOInumbersFile,
          "')\n\n"
          )
        )
    }
    
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