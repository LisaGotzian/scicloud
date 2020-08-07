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
#' @param generateWordlist logical, if set to \code{TRUE}, it generates a wordlist in
#'     your working directory. The list contains all significant words that the
#'     indicator species analysis deemed significant to describe your paper
#'     clusters. You can now add a 0/1 behind each word or delete rows you
#'     don't consider important to the analysis. To re-run the analysis with
#'     the new wordlist, read it in using \code{keepWordsFile} as an argument
#'     to \code{\link{ordinationCluster}}.
#' @param keepWordsFile path to a .csv-file that specifies which words to keep
#'     during the analysis. Accepts 0/1 behind each word or takes the words
#'     as they are and disregards all other words of the analysis.
#' @param myAPIKey your private Elsevier API key for communicating with the
#'     API. You can request one at \url{https://dev.elsevier.com/index.jsp}.
#' @param method takes "network", "hclust" or "both" as a method
#' @param saveToWd  a logical parameter whether or not to save the output of the
#'     function to the working directory. This is especially useful for later
#'     analysis steps. The file can be read in by using \code{\link[base]{readRDS}}.
#' @family scicloud functions
#' @return placeholder
#' @export
#' @examples \dontrun{
#' 
#' ### The normal workflow of scicloud
#' myAPIKey <- "YOUR_API_KEY"
#' metaMatrix <- createTextMatrixFromPDF()
#'
#' 
#' # run the analysis
#' scicloudAnalysis <- ordinationCluster(metaMatrix,
#'                            myAPIKey = myAPIKey)
#'
#' # inspect the analysis
#' scicloudSpecs <- inspectScicloud(scicloudAnalysis)
#' 
#' 
#' ### Workflow for generating a wordlist to revise the words
#' scicloudAnalysis <- ordinationCluster(metaMatrix, 
#'                                       generateWordlist = TRUE,
#'                                       stemWords = TRUE, numberOfClusters = 4)
#' scicloudAnalysis <- ordinationCluster(metaMatrix,
#'                                       keepWordsFile = "YourFile.csv",
#'                                       stemWords = TRUE, numberOfClusters = 4)
#' # inspect the analysis
#' scicloudSpecs <- inspectScicloud(scicloudAnalysis)
#'}
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
                              generateWordlist = FALSE,
                              keepWordsFile = NA,
                              saveToWd = TRUE,
                              method = "hclust") {
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
  # ensure no of clusters defined is less than total number of papers available  
  if(numberOfClusters > dim(metaMatrix)[1]){
    ArgumentCheck::addError(
      msg = "Invalid input for numberOfClusters! It must be less than total no. of papers available!", 
      argcheck = Check
    ) 
  }
  ArgumentCheck::finishArgCheck(Check)
  
  # to change minor things in other functions when running the big function, such as using readline() after the Dendrogram.
  ordinationFunction <-  TRUE
  
  processedMetaDataMatrix <-
    processMetaDataMatrix(
      metaMatrix,
      control = list(language = language,
      stemWords = stemWords,
      saveToWd = saveToWd,
      ordinationFunction = ordinationFunction),
      ignoreWords = ignoreWords,
      keepWordsFile = keepWordsFile
    )
  
  if(!is.na(myAPIKey)){
    processedMetaDataMatrix$metaMatrix <-
      getScopusMetaData(processedMetaDataMatrix$metaMatrix,
                        myAPIKey,
                        ordinationFunction = ordinationFunction)
  }
  
  
  if (any(c(method == "hclust"), (method == "both"))) {
    scicloudAnalysis <-
      calculateModels(
        processedMetaDataMatrix,
        numberOfClusters = numberOfClusters,
        minWordsPerCluster = minWordsPerCluster,
        maxWordsPerCluster = maxWordsPerCluster,
        p = p,
        dendrogram = dendrogram,
        dendroLabels = dendroLabels,
        ordinationFunction = ordinationFunction,
        saveToWd = saveToWd,
        generateWordlist = generateWordlist
      )
    createOrdinationPlot(scicloudAnalysis,
                         exactPosition = exactPosition,
                         ordinationFunction = ordinationFunction)
    
    if(!is.na(myAPIKey)){ # if there's no API key, we don't need the influencial papers
      ANSWER <-
        readline("Show most influencial papers per cluster? (y/n)")
      if (substr(ANSWER, 1, 1) == "y") {
        mostImportantPaperPerCluster(scicloudAnalysis)
      }
    }
    
  }
  if (method == "both") {
    readline("Proceed with network analysis?")
  }
  if (any(c(method == "network"), (method == "both"))) {
    modeledNetwork <-
      calculateNetwork(
        processedMetaDataMatrix,
        saveToWd = saveToWd,
        ordinationFunction = ordinationFunction
      )
  }
  
  if (method == "hclust")
    return(scicloudAnalysis)
  if (method == "network")
    return(modeledNetwork)
  if (method == "both")
    return(c(scicloudAnalysis, modeledNetwork))
  
}
