# ########################################################################
# Essential Wrapper Function
# ########################################################################


#' @title ordinationCluster
#'
#' @description The essential wrapper function that runs all steps of the word
#'     analysis as the following: \cr
#'     1. processMetaDataMatrix \cr
#'     2. getScopusMetaData \cr
#'     3. calculateModels \cr
#'     4. calculateNetwork \cr
#'     5. createOrdinationPlot \cr
#'     6. mostImportantPaperPerCluster \cr
#'
#' @author Matthias Nachtmann, \email{matthias.nachtmann@@stud.leuphana.de},
#'     Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de}
#' @param metaMatrix a dataframe generated
#'     either from PDFs with \code{\link{createTextMatrixFromPDF}} or by
#'     searching Scopus with \code{\link{searchScopus}}
#' @param language this defines the stopwords to be filtered. the default is
#'     "SMART". Look at \code{\link[tm]{stopwords}} for more information.\cr
#' @param numberOfClusters integer or NA; It must be an integer value \cr
#'     not more than 14. An integer sets the number of clusters manually \cr
#'     for NA, the function automatically calculates results for 1 till 12 clusters
#' @param minWordsPerCluster minimum number of words to be plotted per cluster
#'     in \code{\link{createOrdinationPlot}}.
#' @param maxWordsPerCluster maximum number of words to be plotted per cluster
#'     in \code{\link{createOrdinationPlot}}.
#' @param stemWords logical variable which is passed to processMetaDataMatrix. 
#'     When it is TRUE, every word is transformed to its stem. Look at 
#'    \code{\link[tm]{stemDocument}} for more information.
#' @param ignoreWords a vector of words to be ignored which is passed to processMetaDataMatrix. 
#'     Refer \code{\link{processMetaDataMatrix.}} for details.
#' @param exactPosition logical variable which is passed to createOrdinationPlot. When set to \code{TRUE}, the words position will be marked by
#'     a dot and the label will be connected with a line to it. Refer \code{\link{createOrdinationPlot}} for details.
#' @param p the p-value that sets the significance level of individual words for
#'     the indicator species analysis. Only significant words will be plotted
#'     in \code{\link{createOrdinationPlot}}.
#' @param dendrogram logical, whether or not to show a dendrogram of the calculated
#'     clusters.
#' @param dendroLabels allows "truncated" or "break". This either truncates the
#'     labels of the dendrogram leaves or puts a line break. Line breaks are not
#'     recommended for a large number of PDFs.
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
#'@param long_msg logical variable to whether print long message or not 
#' @family scicloud functions
#' @return Returns a list with the following components:
#' \itemize{
#'     \item \code{IndVal}: the results of the indicator species analysis.
#'     \item \code{metaMatrix}: the metaMatrix that has been processed by
#'     \code{\link{processMetaDataMatrix}} including a cluster column for each paper
#'     \item \code{RepresentativePapers}: a dataframe of the most representative
#'     papers of each publication community. Papers are representative if they contain
#'     the highest number of significant words.
#'     \item \code{wordList}: a list of all words that have been used in the analysis.
#'     }
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
                              method = "hclust",
                              long_msg = FALSE) {
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
  if(!is.na(numberOfClusters)){
    if(numberOfClusters > dim(metaMatrix)[1]){
      ArgumentCheck::addError(
        msg = "Invalid input for numberOfClusters! It must be less than total no. of papers available!", 
        argcheck = Check
      ) 
    }
  }
  ArgumentCheck::finishArgCheck(Check)
  
  processedMetaDataMatrix <-
    processMetaDataMatrix(
      metaMatrix,
      control = list(language = language,
      stemWords = stemWords,
      saveToWd = saveToWd,
      long_msg = long_msg),
      ignoreWords = ignoreWords,
      keepWordsFile = keepWordsFile
    )
  
  if(!is.na(myAPIKey)){
    processedMetaDataMatrix$metaMatrix <-
      getScopusMetaData(processedMetaDataMatrix$metaMatrix,
                        myAPIKey,
                        long_msg = long_msg)
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
        long_msg = long_msg,
        saveToWd = saveToWd,
        generateWordlist = generateWordlist
      )
    createOrdinationPlot(scicloudAnalysis,
                         exactPosition = exactPosition)
    
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
        long_msg = long_msg
      )
  }
  
  if (method == "hclust")
    return(scicloudAnalysis)
  if (method == "network")
    return(modeledNetwork)
  if (method == "both")
    return(c(scicloudAnalysis, modeledNetwork))
  
}
