#' @title Summarize the result of scicloud analysis
#'
#' @description The last function to be called to perform analysis in scicloud.
#' It takes the result of \code{scicloudAnalysis} and returns a summary of the 
#' cluster analysis.
#' @param scicloudAnalysis the result of \code{scicloudAnalysis}
#'
#' @author Creator of the scicloud workflow: Henrik von Wehrden,
#'   \email{henrik.von_wehrden@@leuphana.de} \cr \cr
#'   Code by: Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de}\cr \cr
#'   First version of scicloud: Matthias Nachtmann,
#'   \email{matthias.nachtmann@@stud.leuphana.de}
#'
#' @return A summary of the analysis is printed into the console. It gives
#'     insights into the total number of papers and words used.
#'     If the method was hclust, the returned object contains:
#'     \itemize{
#'     \item \code{paperCluster}: a paper-cluster table. Each paper
#'     belongs to one cluster. Use \code{View(scicloudSpecs$paperCluster)} to see
#'     which paper belongs to which cluster.
#'     \item \code{IndVal}: a words-cluster table. Words do not
#'     belong to one single cluster. An indicator species analysis shows how
#'     representative each word is for each cluster. Use \code{View(scicloudSpecs$IndVal)}
#'     to view the results of the indicator species analysis.
#'     \item \code{excludedPapers}: the excluded papers. Use 
#'     \code{View(scicloudSpecs$excludedPapers)} to see which papers have
#'     been excluded, possibly because of a PDF error.
#'     \item \code{representativePapers}: the most representative papers. Use 
#'     \code{View(scicloudSpecs$representativePapers)} to see which papers
#'     are the most representative ones, weighted with the indicator species
#'     values of the words in the paper.
#'     \item \code{metaMatrix}: matrix with metadata of the papers. Use
#'     \code{View(scicloudSpecs$metaMatrix)} to view the original
#'     metaMatrix (without full texts, so it's safe to open).
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
#' scicloudAnalysis <- runAnalysis(scicloudList = scicloudList, numberOfClusters = 4)
#'                            
#' # Generate a summary of the analysis 
#' scicloudSpecs <- inspectScicloud(scicloudAnalysis)
#' }
#' @export

inspectScicloud <- function(scicloudAnalysis) {
  
  if("LocalMeasures" %in% names(scicloudAnalysis)){ # if network method
   cat( "Summary of the scicloud *network* analysis\n
The scicloud network analysis is saved to a .csv-file in your working directory. It contains the following components for further use in Gephi:
LocalMeasures: local measures for both papers and words
ReducedLocalMeasures: 1/3 of the words (!) with their centrality measures & clustering according to three different clustering methods, arranged by default by eigenvector centrality using sortby
ReducedIncidenceMatrix: 1/3 of the words arranged by eigenvector centrality, to be further processed e.g. in Gephi or with other clustering functions
GlobalMeasures: global measures of the network
###################################################################\n\n")
  }
  
  if("IndVal" %in% names(scicloudAnalysis)){ # if hclust method
  paperCluster <-
    cbind(scicloudAnalysis$metaMatrix[, "Cluster"], scicloudAnalysis$metaMatrix[, "FileName"])
  colnames(paperCluster) <- c("Cluster", "FileName")
  
  OriginalPapers <- scicloudAnalysis$metaMatrix[, "FileName"]
  excludedPapers <-
    setdiff(trimws(OriginalPapers), trimws(paperCluster[, 2]))
  
  cat(
    paste0(
      "Summary of the scicloud *hclust* analysis\n\nTotal papers: ",
      length(OriginalPapers),
      ", processed papers: ",
      length(paperCluster[, 2]),
      ", excluded papers: ",
      length(excludedPapers),
      ",\nTotal words: ",
      length(scicloudAnalysis$wordList),
      ", words that are in 5% of all papers that have been used for the analysis: ",
      length(levels(
        scicloudAnalysis[[1]]$`names(indSpeciesValues$pval)`
      )),
      # that is a very weird way to extract them.
      "\n\nPapers per Cluster:\n"
    )
  )
  
  NumberOfClusters <- max(paperCluster[, 1])
  
  for (i in 1:NumberOfClusters) {
    cat(paste0("Cluster ", i, " with ", sum(paperCluster[, 1] == i), " papers\n"))
  }
  
  
  scicloudSpecs <- list()
  
  # Which papers are in which cluster?
  scicloudSpecs[[1]] <- paperCluster
  
  cat(
    "\nThe following additional specs are available:\n- Paper-Cluster-Table: Each paper belongs to one cluster. Use View(scicloudSpecs$paperCluster) to see which paper belongs to which cluster.\n"
  )
  
  scicloudSpecs[[2]] <- scicloudAnalysis$IndVal
  cat(
    "- Words-Cluster-Table: Words do not belong to one single cluster. An indicator species analysis shows how representative each word is for each cluster. Use View(scicloudSpecs$IndVal) to view the results of the indicator species analysis.\n"
  )
  
  # So which papers have been excluded in step 2? (step 1 will come)
  scicloudSpecs[[3]] <- excludedPapers
  
  cat(
    "- excluded Papers: use View(scicloudSpecs$excludedPapers) to see which papers have been excluded, possibly because of a PDF error.\n"
  )
  
  scicloudSpecs[[4]] <- scicloudAnalysis$RepresentativePapers
  cat(
    "- most representative Papers: use View(scicloudSpecs$representativePapers) to see which papers are the most representative ones, weighted with the indicator species values of the words in the paper.\n"
  )
  
  scicloudSpecs[[5]] <-
    scicloudAnalysis$metaMatrix[, -which(colnames(scicloudAnalysis$metaMatrix) == "FullText")]
  # excluding with - doesn't work with ""
  cat(
    "- Matrix with metadata of the papers: use View(scicloudSpecs$metaMatrix) to view the original metaMatrix (without full texts, so it's safe to open)."
  )
  
  names(scicloudSpecs) <-
    c("paperCluster",
      "IndVal",
      "excludedPapers",
      "representativePapers",
      "metaMatrix")
  
  
  return(scicloudSpecs)
  }
}
