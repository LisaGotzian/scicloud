# ########################################################################
# Essential Wrapper Function
# ########################################################################
#' @title runAnalysis
#' @export
runAnalysis <- function(scipusList,
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
    if(numberOfClusters > nrow(scipusList$metaMatrix)){
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
        scipusList,
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
