#' @title calculateModels
#'
#' @description The third function to the word analysis with scicloud. It takes the
#'     output of \code{\link[scicloud]{processMetaDataMatrix}} and calculates
#'     ordination and cluster models. Each paper is assigned to one cluster
#'     while each word receives an indicator value with \code{\link[labdsv]{indval}}
#'     for each cluster, showing how representative word is for a cluster. The top
#'     representative words will be used in the further process with \code{
#'     \link[scicloud]{createOrdinationPlot}}.
#'
#' @author Matthias Nachtmann, \email{matthias.nachtmann@@stud.leuphana.de},
#'     Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de}
#' @param processedData result of \code{\link[scicloud]{processMetaDataMatrix}}
#' @param numberOfClusters integer or NA; an integer forces the number of clusters,
#'     NA results in an automatic iteration to determine the optimal amount.
#' @param minWordsPerCluster minimum number of words to be plotted per cluster.
#' @param maxWordsPerCluster maximum number of words to be plotted per cluster.
#'     this is only happening if the indicator values are higher than the maximum
#'     value of the weakest cluster arm.
#' @param p p-Value to determine the significance of individual words. Only
#'     significant words will be plotted.
#' @param dendrogram shows a dendrogram of the calculated cluster if set to \code{TRUE}.
#' @param dendroLabels allows "truncated" or "break" to either truncate the
#'     labels of the dendrogram leaves or put a line break. Line breaks are not
#'     recommend for a large number of PDFs.
#' @param generateWordlist if set to \code{TRUE}, it generates a wordlist in
#'     your working directory. The list contains all significant words that the
#'     indicator species analysis deemed significant to describe your paper
#'     clusters. To work with the new wordlist, read it in using
#'     \code{keepWordsFile} as an argument to \code{\link[scicloud]{ordinationCluster}}.
#' @param saveToWd a logical parameter whether or not to save the output of the
#'     function to the working directory. This is especially useful for later
#'     analysis steps and can be read in by using \code{\link[base]{readRDS}}.
#' @param ordinationFunction placeholder
#' @param longMessages placeholder
#' @seealso \code{\link[scicloud]{processMetaDataMatrix}} for the preceding step,
#'     \code{\link[scicloud]{createOrdinationPlot}} for the graphics,
#'     \code{\link[scicloud]{mostImportantPaperPerCluster}} and
#'     \code{\link[scicloud]{inspectScicloud}} for a summary of the analysis
#' @family scicloud functions

#' @return list of two entries. First entry contains all the information to
#'     generate the ordination/cluster plot. Entry two is an updated version
#'     of the metaMatrix, which now included the assigned cluster number per
#'     paper to allow additional statistical analyses.
#' @export
#' @examples \dontrun{ placeholder }
calculateModels <- function(processedData,
                            numberOfClusters = NA,
                            minWordsPerCluster = 5,
                            maxWordsPerCluster = 10,
                            p = 0.05,
                            dendrogram = TRUE,
                            dendroLabels = "truncated",
                            generateWordlist = FALSE,
                            saveToWd = TRUE,
                            ordinationFunction = FALSE,
                            longMessages = FALSE) {
  # Argument Checks
  Check <- ArgumentCheck::newArgCheck()
  # ignore to tiny p values. labdvs::inval only provides three decimal digits
  if (p < 0.001) {
    p <- 0.001
    ArgumentCheck::addWarning(
      msg = "p values < 0.001 is not taken into account, it is set to p=0.001", 
      argcheck = Check
    )
  }
  
  if (maxWordsPerCluster < minWordsPerCluster) {
    ArgumentCheck::addError(
      msg = "Invalid maxWordsPerCluster! It is smaller than minWordsPerCluster!", 
      argcheck = Check
    )
  }
  
  if (!is.na(numberOfClusters)) {
    if (!is.numeric(numberOfClusters)) {
      ArgumentCheck::addError(
        msg = "Invalid numberOfClusters! It is not a numeric input!", 
        argcheck = Check
      )
    }
    if (length(numberOfClusters) != 1) {
      ArgumentCheck::addError(
        msg = "Invalid numberOfClusters! It is not a single value input!", 
        argcheck = Check
      )
    }
  }
  ArgumentCheck::finishArgCheck(Check)
  
  if (longMessages == TRUE) {
    Sys.sleep(1)
    cat("Calculating models\n")
    Sys.sleep(1)
  }
  
  model <-
    vegan::decorana(processedData$Tf_Idf, iweigh = 0) #all row sums must be >0 in the community matrix

  axisPositions <- vegan::scores(model, display = c("species"))
  
  
  #cluster
  # replaced agnes by hclust from mclust
  disthclust <-
    stats::dist(processedData$Tf_Idf, method = "euclidian")
  modelclust <- stats::hclust(disthclust, method = "ward.D")
  indSpeciesValues <- c()
  
  if (is.na(numberOfClusters)) {
    
    cat("Determining the optimal number of clusters...\n")
    
    
    numberOfSignificantIndicators <- vector(length = 12)
    for (cluster in 1:12) {# test up till 12 clusters
      
      cutmodel <-
        stats::cutree(modelclust, k = cluster) #assigns a cluster number to every paper
      indSpeciesValues <-
        labdsv::indval(processedData$Tf_Idf, cutmodel, numitr = 1000)
        cat(
          paste0(
            "- Number of clusters: ",
            cluster,
            "; significant words (p < ",
            p,
            "): ",
            length(indSpeciesValues$pval[indSpeciesValues$pval <= p]),
            "\n"
          )
        )
      
      numberOfSignificantIndicators[cluster] <-
        length(indSpeciesValues$pval[indSpeciesValues$pval <= p])
      
    }

    ANSWER <- readline("With how many clusters would you like to proceed? Define 'numberOfClusters = YOURANSWER' as an argument to skip this next time.")
    
    numberOfClusters <- as.numeric(substr(ANSWER, 1, 1))

  }
  
  
  cutmodel <-
    stats::cutree(modelclust, k = numberOfClusters) #assigns a cluster number to every paper
  Cluster <- as.numeric(cutmodel)
  
  
  
  # Performs a Dufrene-Legendre Indicator Species Analysis that calculates the indicator value
  # (fidelity and relative abundance) of species in clusters or types.
  indSpeciesValues <-
    labdsv::indval(processedData$Tf_Idf, cutmodel, numitr = 1000)
  
  # Combines all relevant values for the indicator species analysis (including axis positions)
  combIndSpeciesValues <-
    cbind(
      round(indSpeciesValues$indval, digits = 2),
      indSpeciesValues$pval,
      names(indSpeciesValues$pval),
      axisPositions
    )
  
  # Only takes the p values that are smaller than the given p value passed
  # as an argument (0.05 per default)
  signIndSpeciesValues <-
    combIndSpeciesValues[combIndSpeciesValues["indSpeciesValues$pval"] <= p,]
  
  if (generateWordlist == TRUE){
    data.table::fwrite(as.data.frame(
      signIndSpeciesValues$`names(indSpeciesValues$pval)`), col.names =  FALSE,
      file = "scicloudWordlist.csv")
    cat(
      paste0(
        "\nThe scicloudWordlist is now in your working directory. It ",
        "contains all significant words that the indicator species ",
        "analysis deemed significant to describe your paper clusters."))
    readline("Proceed with calculation? Press ESC to work with wordlist first.")
  }
  
  highestIndValPerCluster <- c()
  for (i in 1:numberOfClusters) {
    highestRankedWord <-
      signIndSpeciesValues[order(signIndSpeciesValues[, i], decreasing = T)[c(1)],]
    highestIndValPerCluster <-
      c(highestIndValPerCluster, highestRankedWord[, i])
  }
  
  
  numberOfWords <- c()
  
  signIndSpeciesValuesInclSubsetRow <- data.frame()
  
  
  highValues <-
    (signIndSpeciesValues[, 1] > min(highestIndValPerCluster))
  highValues <- length(highValues[highValues == TRUE])
  
  if (highValues > maxWordsPerCluster) {
    numberOfWords <- maxWordsPerCluster
  } else if (highValues > minWordsPerCluster) {
    numberOfWords <- highValues
  } else{
    numberOfWords <- minWordsPerCluster
  }
  
  signIndSpeciesValuesInclSubsetRow <-
    signIndSpeciesValues[order(signIndSpeciesValues[, 1], decreasing = T)
                         [c(1:numberOfWords)],]
  signIndSpeciesValuesInclSubsetRow$subset <-
    paste0("Cluster ", rep.int(1, numberOfWords))
  
  
  
  for (i in 2:numberOfClusters) {
    highValues <-
      (signIndSpeciesValues[, i] > min(highestIndValPerCluster))
    highValues <- length(highValues[highValues == TRUE])
    
    if (highValues > maxWordsPerCluster) {
      numberOfWords <- maxWordsPerCluster
    } else if (highValues > minWordsPerCluster) {
      numberOfWords <- highValues
    } else{
      numberOfWords <- minWordsPerCluster
    }
    
    subset <-
      signIndSpeciesValues[order(signIndSpeciesValues[, i], decreasing = T)[c(1:numberOfWords)],]
    subset$subset <- paste0("Cluster ", rep.int(i, numberOfWords))
    signIndSpeciesValuesInclSubsetRow <-
      rbind(signIndSpeciesValuesInclSubsetRow, subset)
  }
  
  
  
  modeledData <- list()
  modeledData[[1]] <- signIndSpeciesValuesInclSubsetRow
  
  
  
  # add a dendrogram of the papers
  if (dendrogram == TRUE) {
    if (dendroLabels == "break") {
      # this breaks the file names
      wordwrap <-
        function(x, len)
          paste(strwrap(x, width = len), collapse = "\n")
      graphics::plot(
        modelclust,
        cex = 0.6,
        hang = -1,
        main = "Word cluster dendrogram of papers",
        labels = sapply(processedData$MetaMatrix[, "FileName"], wordwrap, len =
                          15),
        padj = 1
      )
      #18th column is the filename, strwrap() splits the labels
    }
    if (dendroLabels == "truncated") {
      # this truncates the labels to 18 characters, followed by "..."
      longLabels <- processedData$MetaMatrix[, "FileName"]
      shortenedLabels <- NULL
      for (i in 1:length(longLabels)) {
        shortenedLabels[i] <-
          ifelse(
            nchar(longLabels[i]) > 20,
            # shorten labels from 1:18
            paste(
              paste(
                unlist(strsplit(longLabels[i], ""))[1:18],
                sep = "",
                collapse = ""
              ),
              "...",
              sep = "",
              collapse = ""
            ),
            longLabels[i]
          )
      }
      graphics::plot(
        modelclust,
        cex = 0.6,
        hang = -1,
        main = "Word cluster dendrogram of papers",
        labels = shortenedLabels
      )
    }
  }
  
  
  modeledData[[2]] <-
    cbind(processedData$MetaMatrix, "Cluster" = Cluster)
  
  
  # Representative papers based on the percentage of significant words in each paper
  # only the papers from signIndSpeciesValues from processed data
  ClusterContent <-
    signIndSpeciesValues[, "names(indSpeciesValues$pval)"]
  ClusterContent <- as.data.frame(ClusterContent)
  
  # select said columns (words) from processedData which is a 0-1 matrix of papers and words
  representativePapers <-
    as.data.frame(processedData$Tf_Idf[, ClusterContent[, 1]])
  rownames(representativePapers) <-
    processedData$MetaMatrix[, "FileName"] # take the filenames as row names
  
  # Extracting the percentage
  # give each paper a percentage value and call the column percentageOfSignWordsInPaper
  # this dataframe also has the words in it in case you'd like to further investigate the words used
  
  # A weighted percentage
  # This is done by adding up the indicator species value of word i in cluster j if the word exists
  # for paper 1 which is in cluster j.
  
  for (i in 1:nrow(representativePapers)) {
    ClusterOfPaper <- Cluster[i] # the cluster paper i is in
    representativePapers[i,] <-
      as.numeric(representativePapers[i,]) * #take the 0/1 if the word exists
      signIndSpeciesValues[[ClusterOfPaper]] # and multiply it by the indicator species value for
    # said cluster
    
  }
  
  
  representativePapers$percentageOfSignWordsInPaper <-
    rowSums(representativePapers) / ncol(representativePapers)
  
  representativePapersEasyToOpen <-
    cbind(representativePapers$percentageOfSignWordsInPaper,
          Cluster)
  representativePapersEasyToOpen <-
    as.data.frame(representativePapersEasyToOpen)
  rownames(representativePapersEasyToOpen) <-
    trimws(processedData$MetaMatrix[, "FileName"]) # take the filenames as row names
  colnames(representativePapersEasyToOpen) <-
    c("percentageOfSignWordsInPaper", "Cluster")
  
  modeledData[[3]] <- representativePapersEasyToOpen
  
  modeledData[[4]] <- processedData$wordList
  names(modeledData) <-
    c("IndVal",
      "MetaMatrix",
      "RepresentativePapers",
      "wordList")
  
  
  ## save each paper into one new folder
  if (dir.exists("PdfsPerCluster/")){
    warning("The existing paper-cluster folders have been overwritten")
  }
  dir.create("PdfsPerCluster/", showWarnings = FALSE)
  for (i in 1:nlevels(as.factor(cutmodel))) {
    dir.create(paste0("PdfsPerCluster/", i), showWarnings = FALSE)
    file.copy(
      c(paste0("PDFs/", rownames(representativePapersEasyToOpen[representativePapersEasyToOpen[, 2] ==i,]))),
      to = paste0("PdfsPerCluster/", i),
      copy.mode = T
    )
  }
  
  cat(
    paste0(
      "\nAll PDFs have been copied to different subfolders in the new folder 'PdfsPerCluster'
      according to the cluster they belong to.\n"
    )
  )
  
  if (saveToWd == TRUE) {
    save_data(modeledData, "modeledData", long_msg = !ordinationFunction)
  }
  return(modeledData)
}
