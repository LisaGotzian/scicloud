#' @title Perform scicloud analysis
#' @description The second function to be called to perform analysis with scicloud. 
#' It output a list of 4 components: IndVal, metaMatrix, RepresentativePapers and 
#' wordList. The function performs the analysis based on the method argument. 
#' By default the method is set to 'hclust' which an indicator species analysis
#' will be executed. Each word receives an indicator species value by 
#' \code{\link[labdsv]{indval}} for each cluster, showing how representative 
#' a word is for a cluster. The top representative words will be then visualize
#' with several plots.
#' @author Matthias Nachtmann, \email{matthias.nachtmann@@stud.leuphana.de},
#'     Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de},
#'     Jia Yan Ng, \email{Jia.Y.Ng@@stud.leuphana.de}
#'          
#' @param scicloudList output of \code{\link{createScicloudList}}
#' @param numberOfClusters integer or NA; It must be an integer value \cr
#'     not more than 14. An integer sets the number of clusters manually \cr
#'     for NA, the function automatically calculates results for 1 till 12 clusters
#' @param minWordsPerCluster minimum number of words to be plotted per cluster
#'     in \code{\link{createOrdinationPlot}}.
#' @param maxWordsPerCluster maximum number of words to be plotted per cluster
#'     in \code{\link{createOrdinationPlot}}.
#' @param p the p-value that sets the significance level of individual words for
#'     the indicator species analysis. Only significant words will be plotted
#'     in \code{\link{createOrdinationPlot}}.
#' @param exactPosition logical, the plot function tries to avoid overlapping 
#'     labels for the sake of visual simplicity over perfect precision. 
#'     When set to TRUE, the words position will be marked by a dot and the 
#'     label will be connected with a line to it.     
#' @param dendrogram logical, whether or not to show a dendrogram of the calculated
#'     clusters.
#' @param dendroLabels allows "truncated" or "break". This either truncates the
#'     labels of the dendrogram leaves or puts a line break. Line breaks are not
#'     recommended for a large number of PDFs.
#' @param saveToWd  a logical parameter whether or not to save the output of the
#'     function to the working directory. This is especially useful for later
#'     analysis steps. The file can be read in by using \code{\link[base]{readRDS}}.
#' @param method takes "network", "hclust" or "both" as a method
#' @param generateWordlist logical, if set to \code{TRUE}, it generates a wordlist in
#'     your working directory. The list contains all significant words that the
#'     indicator species analysis deemed significant to describe your paper
#'     clusters. You can now add a 0/1 behind each word or delete rows you
#'     don't consider important to the analysis. To re-run the analysis with
#'     the new wordlist, read it in using \code{keepWordsFile} as an argument
#'     to \code{\link{ordinationCluster}}.
#' @param long_msg logical variable to whether print long message or not
#' @return Returns a list with the following components:
#' \itemize{
#'     \item \code{IndVal}: the results of the indicator species analysis.
#'     \item \code{metaMatrix}: the metaMatrix that has been pre-processed
#'     \item \code{RepresentativePapers}: a dataframe of the most representative
#'     papers of each publication community. Papers are representative if they contain
#'     the highest number of significant words.
#'     \item \code{wordList}: a list of all words that have been used in the analysis.
#'     }
#' @family scicloud functions
#' @examples
#' \dontrun{
#' 
#' ### Workflow of performing analysis using scicloud
#' myAPIKey <- "YOUR_API_KEY"
#' # retrieving data from PDFs and Scorpus website using API  
#' scicloudList <- createScicloudList(myAPIKey = myAPIKey)
#'
#' # Run the analysis with a specified no. of cluster
#' scicloudAnalysis <- runAnalysis(scipusList = scipusList, numberOfClusters = 4)
#'                            
#' # Generate a summary of the analysis 
#' scicloudSpecs <- inspectScicloud(scicloudAnalysis)
#' }
#' @export
runAnalysis <- function(scicloudList,
                        numberOfClusters = NA,
                        minWordsPerCluster = 5,
                        maxWordsPerCluster = 10,
                        p = 0.05,
                        exactPosition = FALSE,
                        dendrogram = TRUE,
                        dendroLabels = c("truncated", "break"), 
                        saveToWd = TRUE,
                        method = c("hclust", "network", "both"),
                        generateWordlist = FALSE,
                        long_msg = FALSE) {
  
  #pick the argument input by user, default = "hclust"
  method <- match.arg(method) 
  
  # Argument Checks
  Check <- ArgumentCheck::newArgCheck()
  
  # ensure no of clusters defined is less than total number of papers available  
  if(!is.na(numberOfClusters)){
    if(numberOfClusters > nrow(scicloudList$metaMatrix)){
      ArgumentCheck::addError(
        msg = "Invalid input for numberOfClusters! It must be less than total no. of papers available!", 
        argcheck = Check
      ) 
    }
  }
  ArgumentCheck::finishArgCheck(Check)
  
  if (method == "hclust" | method == "both") {
    
    scicloudAnalysis <-
      calculateModels(
        scicloudList,
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
    
    # create plots for the analysis 
    createOrdinationPlot(scicloudAnalysis,
                         exactPosition = exactPosition)
    
    ANSWER <- readline("Show most influencial papers per cluster? (y/n)")
    if (substr(ANSWER, 1, 1) == "y") {
        mostImportantPaperPerCluster(scicloudAnalysis)
    }
    
  }
  if(method == "network" | method == "both"){
    modeledNetwork <-
      calculateNetwork(
        scicloudList,
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
