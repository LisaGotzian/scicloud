########################################################################
# Inspecting the results of the scicloud package
########################################################################

#' @title inspectScicloud
#'
#' @description placeholder
#' @param modeledData placeholder
#' @family scicloud functions
#' @seealso \code{\link{calculateModels}} or \code{\link{calculateNetwork}}
#'     for the preceding step,
#'     \code{\link{createOrdinationPlot}} for the graphics,
#'     \code{\link{mostImportantPaperPerCluster}}
#'
#' @author Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de}
#' @return placeholder
#' @export

inspectScicloud <- function(modeledData = modeledData) {
  paperCluster <-
    cbind(modeledData$MetaMatrix[, "Cluster"], modeledData$MetaMatrix[, "FileName"])
  colnames(paperCluster) <- c("Cluster", "FileName")
  
  OriginalPapers <- modeledData$MetaMatrix[, "FileName"]
  excludedPapers <-
    setdiff(trimws(OriginalPapers), trimws(paperCluster[, 2]))
  
  cat(
    paste0(
      "Summary of the scicloud analysis\n\nTotal papers: ",
      length(OriginalPapers),
      ", processed papers: ",
      length(paperCluster[, 2]),
      ", excluded papers: ",
      length(excludedPapers),
      ",\nTotal words: ",
      length(modeledData$numberOfWords),
      ", words that are in 5% of all papers that have been used for the analysis: ",
      length(levels(
        modeledData[[1]]$`names(indSpeciesValues$pval)`
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
  
  scicloudSpecs[[2]] <- modeledData$IndVal
  cat(
    "- Words-Cluster-Table: Words do not belong to one single cluster. An indicator species analysis shows how representative each word is for each cluster. Use View(scicloudSpecs$IndVal) to view the results of the indicator species analysis.\n"
  )
  
  # So which papers have been excluded in step 2? (step 1 will come)
  scicloudSpecs[[3]] <- excludedPapers
  
  cat(
    "- excluded Papers: use View(scicloudSpecs$excludedPapers) to see which papers have been excluded, possibly because of a PDF error.\n"
  )
  
  scicloudSpecs[[4]] <- modeledData$RepresentativePapers
  cat(
    "- most representative Papers: use View(scicloudSpecs$representativePapers) to see which papers are the most representative ones, weighted with the indicator species values of the words in the paper.\n"
  )
  
  scicloudSpecs[[5]] <-
    modeledData$MetaMatrix[, -which(colnames(modeledData$MetaMatrix) == "FullText")]
  # excluding with - doesn't work with ""
  cat(
    "- Matrix with metadata of the papers: use View(scicloudSpecs$MetaMatrix) to view the original MetaMatrix (without full texts, so it's safe to open)."
  )
  
  names(scicloudSpecs) <-
    c("paperCluster",
      "IndVal",
      "excludedPapers",
      "representativePapers",
      "MetaMatrix")
  
  
  return(scicloudSpecs)
}
