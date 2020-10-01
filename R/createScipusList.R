# ########################################################################
# Essential Wrapper Function
# ########################################################################
#' @title createScipusList
#' @export
createScipusList <- function(directory = file.path(".", "PDFs"),
                           myAPIKey = NA,
                           language = "SMART",
                           stemWords = TRUE,
                           saveToWd = FALSE,
                           long_msg = FALSE,
                           ignoreWords = c(),
                           keepWordsFile = NA) {
  
  # Argument Checks
  Check <- ArgumentCheck::newArgCheck()
  
  # Check API Key
  if(is.na(myAPIKey)){
    ArgumentCheck::addError(
      msg = "Please input your API key from Elsevier!",
      argcheck = Check
    )
  }
  ArgumentCheck::finishArgCheck(Check)
  
  # create matrix by reading PDFs 
  metaMatrix <- createTextMatrixFromPDF(directory, saveToWd)
  
  # preprocessing the corpus from the PDFs to create Tf-Idf and WordList
  scipusList <-
    createTfIdf(
      metaMatrix = metaMatrix,
      control = list(
        language = language, 
        stemWords = stemWords, 
        saveToWd = saveToWd, 
        long_msg = long_msg),
      ignoreWords = ignoreWords,
      keepWordsFile = keepWordsFile
    )
  
  # update info in the metaMatrix from Scopus using API
  scipusList$metaMatrix <-
    getScopusMetaData(
      metaMatrix = metaMatrix, 
      myAPIKey = myAPIKey, 
      long_msg = long_msg)
  
  return(scipusList)
}
