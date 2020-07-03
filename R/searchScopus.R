#' @title searchScopus
#'
#' @description This function accepts a search string in URL format and an
#'     Elsevier API Key and returns the DOI numbers of all search results as
#'     a vector for further processing.
#'
#' @author Matthias Nachtmann, \email{matthias.nachtmann@@stud.leuphana.de},
#'     Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de}, Prabesh Dhakal,
#'     \email{prabesh.dhakal@@stud.leuphana.de}
#' @param searchString The search string you want to ask the server. See the
#'     Scopus API (\url{https://dev.elsevier.com/index.jsp/}) for details.
#' @param maxResults The maximum amount of accepted search results. Usually,
#'     Scopus does not provide more than 5000 results.
#' @param countIncrement The number of results per GET request. A private user
#'     can't exceed 25 per request. If you are inside a subscribed IP range,
#'     you can use the maximum of 200 per request. Note, that the weekly quota
#'     for requests is 20,000.
#' @param myAPIKey your private Elsevier API key for communicating with the
#'     API. You can request one at \url{https://dev.elsevier.com/index.jsp}.
#' @param saveToWd a logical parameter whether or not to save the output of the
#'     function to the working directory. This is especially useful for later
#'     analysis steps. The file can be read in by using \code{\link[base]{readRDS}}.
#' @family scicloud functions
#' @return A data frame containing the DOI numbers and Scopus-IDs of the search
#'     results, as well as some placeholder columns.
#' @export
#' @examples \dontrun{
#' DOInumbers <- searchScopus('TITLE-ABS-KEY(sustainability) AND PUBYEAR > 2009',
#'     '1234567890ABCDEF', maxResults = 160, countIncrement = 20)
#' DOInumbers}

searchScopus <- function(searchString,
                         myAPIKey,
                         maxResults = 10,
                         countIncrement = 200,
                         saveToWd = TRUE) {
  #### PHASE I: GET THE DOIs and SCOPUS IDs OF THE SEARCH RESULT ####
  
  # percent-encode the search string
  searchString <- utils::URLencode(searchString)
  
  # initialize an empty request results object
  searchEntries <- NULL
  
  # initialize start parameter for API call
  start <- 0
  
  # initialize the progress bar utility
  pb <-
    utils::txtProgressBar(min = start, max = maxResults, style = 3)
  
  # a function that returns a custom error message
  errorMessage <- function(cond) {
    message(
      paste0(
        "Error while retrieving the title - DOI: ",
        searchResults[i, "DOI"],
        ", Scopus-ID: ",
        searchResults[i, "Scopus-ID"]
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
        "Warning while retrieving the title - DOI: ",
        searchResults[i, "DOI"],
        ", Scopus-ID: ",
        searchResults[i, "Scopus-ID"]
      )
    )
    message("Here's the original warning message:")
    message(cond)
    # Choose a return value in case of warning
    return(NULL)
  }
  
  # evaluation of the number of requests in order to be
  # able to get more than 200 results
  numberOfRequests <- ceiling(maxResults / countIncrement)
  
  if (maxResults > 5000 | numberOfRequests > 20000) {
    stop("Scopus does not provide more than 5,000 search results or 20,000 requests.")
  }
  
  for (i in 1:numberOfRequests) {
    # store only maxResults entries
    if (i == numberOfRequests) {
      countIncrement <- maxResults %% countIncrement
    }
    
    # create the url that is sent as a GET request
    URL <-
      paste0(
        "http://api.elsevier.com/content/search/scopus?query=",
        searchString,
        "&count=",
        countIncrement,
        "&start=",
        start
      )
    
    # response from the server (stored in JSON format)
    serverResponse <-
      httr::GET(URL,
                httr::add_headers(`X-ELS-APIKey` = myAPIKey, Accept = "application/json"))
    
    # store the JSON content from the response
    responseContent <- httr::content(serverResponse)
    
    # store JSON content in entry vector
    searchEntries <-
      c(searchEntries, responseContent$`search-results`$entry)
    
    # update the current number of results ensuring the progress bar always ends with 100%
    start <- min(start + countIncrement, maxResults)
    
    # show progress Bar
    utils::setTxtProgressBar(pb, start)
    
    # API allows 9 requests per second
    if (i %% 9 == 0) {
      Sys.sleep(1)
    }
  }
  
  cat(
    "\nRemaining quota:",
    serverResponse$headers$`x-ratelimit-remaining`,
    "requests within the next 7 days.\n"
  )
  Sys.sleep(1)
  
  # initialize a matrix that stores the results (more efficient this way)
  searchResults <-
    matrix(NA, nrow = maxResults, ncol = 20)
  # assign column header names to the matrix where we store the results
  colnames(searchResults) <-
    c(
      "Title",
      "Year",
      "Month",
      "Day",
      "Authors",
      "Journal",
      "Volume",
      "Issue",
      "Pages",
      "CitedBy",
      "CitationPerYear",
      "DOI",
      "Scopus-ID",
      "Publisher",
      "Affiliation",
      "Affiliation-City",
      "Affiliation-Country",
      "FileName",
      "Abstract",
      "FullText"
    )
  
  # let users know what is happening at this stage (assissted with progress bar later)
  cat("\nAccessing DOIs and Scopus IDs of the search result...\n")
  
  # attempt to grab the contents that we are interested in
  for (i in 1:maxResults) {
    # grab the DOI
    resultDOI <- tryCatch({
      searchEntries[[i]]$`prism:doi`
    }, error = errorMessage, warning = warningMessage)
    
    # Grab the scopus ID
    resultScopusID <- tryCatch({
      sub("SCOPUS_ID:", "", searchEntries[[i]]$`dc:identifier`)
    }, error = errorMessage, warning = warningMessage)
    
    # store DOI and Scopus ID
    searchResults[i, "DOI"] <-
      if (length(resultDOI) > 0) {
        resultDOI
      } else {
        NA
      }
    searchResults[i, "Scopus-ID"] <-
      if (length(resultScopusID) > 0) {
        resultScopusID
      } else {
        NA
      }
  }
  
  # save metaDOInumbers dataFrame to R object file to working directory & global env
  if (saveToWd == TRUE) {
    save_data(searchResults, "metaDOInumbers")
  }
  
  # check for redundant entries
  searchResults <- unique(searchResults)
  # assign unique ID to the rows to avoid any collisions along the way
  searchResults <- cbind(searchResults, ID = 1:nrow(searchResults))
  
  close(pb)  # close progress bar
  
  return(searchResults)
}
