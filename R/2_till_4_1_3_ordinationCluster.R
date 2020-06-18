# ########################################################################
# Essential Wrapper Function
# ########################################################################


#' @title ordinationCluster
#'
#' @description The essential wrapper function that runs all steps of the word
#'     analysis.
#'
#' @author Matthias Nachtmann, \email{matthias.nachtmann@@stud.leuphana.de},
#'     Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de}
#' @param metaMatrix placeholder
#' @param language placeholder
#' @param numberOfClusters placeholder
#' @param minWordsPerCluster placeholder
#' @param maxWordsPerCluster placeholder
#' @param stemWords placeholder
#' @param ignoreWords placeholder
#' @param exactPosition placeholder
#' @param p placeholder
#' @param dendrogram placeholder
#' @param dendroLabels placeholder
#' @param myAPIKey placeholder
#' @param method takes "network", "hclust" or "both" as a method
#' @param saveToWd a logical parameter whether or not to save the output of
#'     the function to the working directory. This is especially useful for
#'     later analysis steps and can be read in by using \code{\link[base]{saveRDS}}.
#' @param longMessages by default \code{FALSE} to keep the output short.
#' @family ginko functions
#' @return placeholder
#' @export
#' @examples \dontrun{
#' placeholder}
ordinationCluster <- function(metaMatrix,
                              language = "SMART",
                              numberOfClusters = NA,
                              minWordsPerCluster = 5,
                              maxWordsPerCluster = 10,
                              stemWords = TRUE,
                              ignoreWords = c(),
                              myAPIKey = NA,
                              p = 0.05,
                              exactPosition = FALSE,
                              dendrogram = TRUE,
                              dendroLabels = "truncated",
                              saveToWd = TRUE,
                              method = "hclust",
                              longMessages = FALSE) {
  # Argument Checks
  Check <- ArgumentCheck::newArgCheck()
  if (sum(c(method == "hclust",
            method == "network",
            method == "both")) != 1) {
      ArgumentCheck::addError(
        msg = "Invalid inputs for 'method'! Valid input: 'hclust', 'network' or 'both'. Default is 'hclust'.", 
        argcheck = Check
      )
  }
  ArgumentCheck::finishArgCheck(Check)
  
  # to change minor things in other functions when running the big function, such as using readline() after the Dendrogram.
  ordinationFunction <-  TRUE
  
  processedMetaMatrix <-
    processMetaDataMatrix(
      metaMatrix,
      control = list(language = language,
      stemWords = stemWords,
      saveToWd = saveToWd,
      ordinationFunction = ordinationFunction),
      ignoreWords = ignoreWords
    )
  
  if(!is.na(myAPIKey)){
    processedMetaMatrix$MetaMatrix <-
      getScopusMetaData(processedMetaMatrix$MetaMatrix,
                        myAPIKey,
                        ordinationFunction = ordinationFunction)
  }
  
  
  if (any(c(method == "hclust"), (method == "both"))) {
    modeledData <-
      calculateModels(
        processedMetaMatrix,
        numberOfClusters = numberOfClusters,
        minWordsPerCluster = minWordsPerCluster,
        maxWordsPerCluster = maxWordsPerCluster,
        p = p,
        dendrogram = dendrogram,
        dendroLabels = dendroLabels,
        ordinationFunction = ordinationFunction,
        saveToWd = saveToWd,
        longMessages = longMessages
      )
    createOrdinationPlot(modeledData,
                         exactPosition = exactPosition,
                         ordinationFunction = ordinationFunction)
    
    if(!is.na(myAPIKey)){ # if there's no API key, we don't need the influencial papers
      ANSWER <-
        readline("Show most influencial papers per cluster? (y/n)")
      if (substr(ANSWER, 1, 1) == "y") {
        mostImportantPaperPerCluster(modeledData)
      }
    }
    
  }
  if (method == "both") {
    readline("Proceed with network analysis?")
  }
  if (any(c(method == "network"), (method == "both"))) {
    modeledNetwork <-
      calculateNetwork(
        processedMetaMatrix,
        saveToWd = saveToWd,
        ordinationFunction = ordinationFunction,
        longMessages = longMessages
      )
  }
  
  if (method == "hclust")
    return(modeledData)
  if (method == "network")
    return(modeledNetwork)
  if (method == "both")
    return(c(modeledData, modeledNetwork))
  
}
