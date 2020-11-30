#' @title Perform scicloud analysis
#' @description The second function to be called to perform the analysis with
#'   scicloud after \code{\link{createScicloudList}}. It outputs a list of 4
#'   components: IndVal, metaMatrix, RepresentativePapers and wordList for
#'   further use with \code{\link{inspectScicloud}}.\cr\cr
#'   The function performs the analysis depending on the method argument. By
#'   default, the method is set to 'hclust' that identifies clusters using
#'   \code{\link[stats]{hclust}}. The clusters are publication communities based
#'   on the words used in the papers. To then identify the words relevant to the
#'   communities, it runs an indicator species analysis. Each word receives an
#'   indicator species value by \code{\link[labdsv]{indval}} for each cluster,
#'   showing how representative each word is within a cluster. The top representative
#'   words will then be visualized with the following plots:\cr
#'   \itemize{
#'     \item a dendrogram of the clusters
#'     \item a wordcloud of the publication communities
#'     \item four visualizations of the communities by year and number of citations
#'     (which have been fetched from the Scopus API)
#'     }
#'   
#'   The 'network' method on the other hand also employs a clustering approach,
#'   but uses a network analysis. When done, it returns a list of global and
#'   local measures and also generates a clustered matrix. This matrix can then
#'   be further processed in network programs like Gephi.
#' 
#' @author Creator of the scicloud workflow: Henrik von Wehrden,
#'   \email{henrik.von_wehrden@@leuphana.de} \cr \cr
#'   Code by: Matthias Nachtmann,
#'   \email{matthias.nachtmann@@stud.leuphana.de},
#'   Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de},
#'   Jia Yan Ng, \email{Jia.Y.Ng@@stud.leuphana.de},
#'   Johann Julius Beeck, \email{johann.j.beeck@@stud.leuphana.de} \cr \cr
#'   First version of scicloud: Matthias Nachtmann,
#'   \email{matthias.nachtmann@@stud.leuphana.de}
#'          
#' @param scicloudList output of \code{\link{createScicloudList}}
#' @param numberOfClusters integer or NA; must be an integer value not more
#'   than 14 as more than 14 clusters are not recommended. An integer sets the
#'   number of clusters manually. For NA, the function automatically calculates
#'   the optimum number of clusters for a range of 1 till 12 possible clusters
#' @param dendrogram logical, whether or not to show a dendrogram of the calculated
#'     clusters.
#' @param dendroLabels allows "truncated" or "break". This either truncates the
#'     labels of the dendrogram leaves or puts a line break. Line breaks are not
#'     recommended for a large number of PDFs.
#' @param minWordsPerCluster minimum number of words per cluster to be plotted
#'     in the wordcloud.
#' @param maxWordsPerCluster maximum number of words per cluster to be plotted
#'     in the wordcloud.
#' @param p the p-value that sets the significance level of individual words for
#'     the indicator species analysis. Only significant words will be plotted.
#' @param exactPosition logical, the wordcloud tries to avoid overlapping 
#'     labels for the sake of visual simplicity over perfect precision. 
#'     When set to \code{TRUE}, the words position will be marked by a dot and the 
#'     label will be connected with a line to it.
#' @param sortby for the network method: the centrality measure to sort the
#'     words by, default is Eigenvector. Allows the following possible inputs:
#'     "Eigenvector", "Degree", "Closeness, "Betweenness".
#' @param keep for the network method: numeric, keeps by default 0.33 of all the
#'   words, sorted by the argument given by \code{sortby}. A smaller amount of
#'   words to keep facilitates computations for later use.
#' @param saveToWd a logical parameter whether or not to save the return of the
#'     function to the working directory. This is especially useful for later
#'     analysis steps. The file can be read in by using \code{\link[base]{readRDS}}.
#' @param method takes "network", "hclust" or "both" as a method
#' @return 'hclust' returns a list with the following components:
#' \itemize{
#'     \item \code{IndVal}: the results of the indicator species analysis.
#'     \item \code{metaMatrix}: the metaMatrix that has been pre-processed
#'     \item \code{RepresentativePapers}: a dataframe of the most representative
#'     papers of each publication community. Papers are representative if they contain
#'     the highest number of significant words.
#'     \item \code{wordList}: a list of all words that have been used in the analysis.
#'     } \cr
#'     'network' returns a list with the following components:
#'     \itemize{
#'         \item \code{LocalMeasures}: local measures for both papers and words
#'         \item \code{ReducedLocalMeasures}: 1/3 of the words (!) with their
#'         centrality measures & clustering according to three different clustering
#'         methods, arranged by default by eigenvector centrality using \code{sortby}
#'         \item \code{ReducedIncidenceMatrix}: 1/3 of the words arranged by
#'         eigenvector centrality, to be further processed e.g. in Gephi or with other
#'         clustering functions
#'         \item \code{GlobalMeasures}: global measures of the network
#'         }
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
#' scicloudAnalysis <- runAnalysis(scicloudList = scicloudList, numberOfClusters = 4)
#'                            
#' # Generate a summary of the analysis 
#' scicloudSpecs <- inspectScicloud(scicloudAnalysis)
#' }
#' @export
runAnalysis <- function(scicloudList,
                        numberOfClusters = NA,
                        dendrogram = TRUE,
                        dendroLabels = c("truncated", "break"), 
                        minWordsPerCluster = 5,
                        maxWordsPerCluster = 10,
                        p = 0.05,
                        exactPosition = FALSE,
                        sortby = c("Eigenvector","Degree",
                                   "Closeness","Betweenness"),
                        keep = 0.33,
                        saveToWd = FALSE,
                        method = c("hclust", "network", "both")) {
  
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
        dendroLabels = dendroLabels
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
        scicloudList, sortby = sortby, keep = keep
      )
  }
  
  if (saveToWd == TRUE) {
    save_data(runAnalysis, "runAnalysis")
  }
  
  if (method == "hclust")
    return(scicloudAnalysis)
  if (method == "network")
    return(modeledNetwork)
  if (method == "both")
    return(c(scicloudAnalysis, modeledNetwork))
  
}
