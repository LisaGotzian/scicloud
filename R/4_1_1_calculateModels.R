#' @title calculateModels
#'
#' @description The third function to the word analysis with ginko. It takes the
#'     output of \code{\link[ginko]{processMetaDataMatrix}} and calculates
#'     ordination and cluster models. Each paper is assigned to one cluster
#'     while each word receives an indicator value with \code{\link[labdsv]{indval}}
#'     for each cluster, showing how representative word is for a cluster. The top
#'     representative words will be used in the further process with \code{
#'     \link[ginko]{createOrdinationPlot}}.
#'
#' @author Matthias Nachtmann, \email{matthias.nachtmann@@stud.leuphana.de},
#'     Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de}
#' @param processedData result of \code{\link[ginko]{processMetaDataMatrix}}
#' @param numberOfClusters integer or NA; an integer forces the number of clusters,
#'     NA results in an automatic iteration to determine the optimal amount.
#' @param minWordsPerCluster minimum number of words to be plotted per cluster.
#' @param maxWordsPerCluster maximum number of words to be plotted per cluster.
#'     this is only hapening if the indicator values are higher than the maximum
#'     value of the weakest cluster arm.
#' @param p p-Value to determine the significance of individual words. Only
#'     significant words will be plotted.
#' @param dendrogram shows a dendrogram of the calculated cluster if set to \code{TRUE}.
#' @param dendroLabels allows "truncated" or "break" to either truncate the
#'     labels of the dendrogram leaves or put a line break. Line breaks are not
#'     recommend for a large number of PDFs.
#' @param saveToWd a logical parameter whether or not to save the output of the
#'     function to the working directory. This is especially useful for later
#'     analysis steps and can be read in by using \code{\link[base]{saveRDS}}.
#' @param ordinationFunction placeholder
#' @param longMessages placeholder
#' @seealso \code{\link{processMetaDataMatrix}} for the preceding step,
#'     \code{\link{createOrdinationPlot}} for the graphics,
#'     \code{\link{mostImportantPaperPerCluster}} and
#'     \code{\link{inspectGinko}} for a summary of the analysis
#' @family ginko functions

#' @return list of two entries. First entry contains all the information to
#'     generate the ordination/cluster plot. Entry two is an updated version
#'     of the metaMatrix, which now included the assigned cluster number per
#'     paper to allow additional statistical analyses.
#' @export
#' @examples \dontrun{
#' placeholder}
calculateModels <- function(processedData,
                            numberOfClusters = NA,
                            minWordsPerCluster = 5,
                            maxWordsPerCluster = 10,
                            p = 0.05,
                            dendrogram = TRUE,
                            dendroLabels = "truncated",
                            saveToWd = TRUE,
                            ordinationFunction = FALSE,
                            longMessages = FALSE) {
  # error handling
  # ignore to tiny p values. labdvs::inval only provides three decimal digits
  if (p < 0.001) {
    p <- 0.001
  }
  
  if (maxWordsPerCluster < minWordsPerCluster) {
    print("Warning: maxWordsPerCluster < minWordsPerCluster")
    print(paste0("Adjusting minWordsPerCluster to: ", maxWordsPerCluster))
    minWordsPerCluster <- maxWordsPerCluster
  }
  
  if (length(numberOfClusters) != 1) {
    print(paste0("numberOfClusters has an invalid value: ", numberOfClusters))
    print("switching to automatic calculation")
    numberOfClusters <- NA
  }
  
  
  if (!is.na(numberOfClusters)) {
    if (!is.numeric(numberOfClusters)) {
      print(paste0(
        "numberOfClusters has an invalid value: ",
        numberOfClusters
      ))
      print("switching to automatic calculation")
      numberOfClusters <- NA
    }
  }
  
  
  
  
  
  if (longMessages == TRUE) {
    Sys.sleep(1)
    cat("Calculating models\n")
    Sys.sleep(1)
  }
  
  model <-
    vegan::decorana(processedData$BinaryWordList, iweigh = 0) # all row sums must be >0 in
  # the community matrix
  axisPositions <- vegan::scores(model, display = c("species"))
  
  #plot(model,type="text")
  #write.table(cbind(rownames(processedData[[1]]),c(1:351)),"numberstest.txt")
  
  #cluster
  # replaced agnes by hclust from mclust
  disthclust <-
    stats::dist(processedData$BinaryWordList, method = "binary")
  modelclust <- stats::hclust(disthclust, method = "ward.D")
  indSpeciesValues <- c()
  
  if (is.na(numberOfClusters)) {
    numberOfClusters = 2
    numberOfSignificantIndicators = 0
    numberOfSignificantIndicatorsNew = 1
    
    cat("Determining the optimal number of clusters...\n")
    
    
    while (numberOfSignificantIndicators < numberOfSignificantIndicatorsNew) {
      numberOfSignificantIndicators <- numberOfSignificantIndicatorsNew
      
      cutmodel <-
        stats::cutree(modelclust, k = numberOfClusters) #assigns a cluster number to every paper
      #table(cutmodel)
      indSpeciesValues <-
        labdsv::indval(processedData$BinaryWordList, cutmodel, numitr = 1000)
      if (longMessages == TRUE) {
        cat(
          paste0(
            "- Number of clusters: ",
            numberOfClusters,
            "; significant words (p < ",
            p,
            "): ",
            length(indSpeciesValues$pval[indSpeciesValues$pval <= p]),
            "\n"
          )
        )
      }
      
      
      numberOfSignificantIndicatorsNew <-
        length(indSpeciesValues$pval[indSpeciesValues$pval <= p])
      
      # this if statements corrects the numberOfClusters if we exeeded the maximum of
      # significant indicators
      if (numberOfSignificantIndicators < numberOfSignificantIndicatorsNew) {
        numberOfClusters <- numberOfClusters + 1
      } else{
        numberOfClusters <- numberOfClusters - 1
      }
      
    }
    
    cat(paste0("- the optimal number of clusters is: ", numberOfClusters),
        "\n")
  }
  
  
  cutmodel <-
    stats::cutree(modelclust, k = numberOfClusters) #assigns a cluster number to every paper
  Cluster <- as.numeric(cutmodel)
  
  
  
  # Performs a Dufrene-Legendre Indicator Species Analysis that calculates the indicator value
  # (fidelity and relative abundance) of species in clusters or types.
  indSpeciesValues <-
    labdsv::indval(processedData$BinaryWordList, cutmodel, numitr = 1000)
  
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
  
  
  # #Backup
  # # this part of the codes adds an additional column to the dataframe that shows in which
  # # cluster the most important words are.
  # # this information is useful to assign colors while plotting
  # signIndSpeciesValuesInclSubsetRow <- data.frame()
  # signIndSpeciesValuesInclSubsetRow <-
  #      signIndSpeciesValues[order(signIndSpeciesValues[,1],decreasing=T)[c(1: wordsPerClusterArm)],]
  # signIndSpeciesValuesInclSubsetRow$subset <- paste0("Cluster ", rep.int(1, wordsPerClusterArm))
  
  # for (i in 2:numberOfClusters) {
  #   subset <-
  #      signIndSpeciesValues[order(signIndSpeciesValues[,i],decreasing=T)[c(1: wordsPerClusterArm)],]
  #   subset$subset <- paste0("Cluster ", rep.int(i, wordsPerClusterArm))
  #   signIndSpeciesValuesInclSubsetRow <- rbind(signIndSpeciesValuesInclSubsetRow, subset)
  # }
  
  # it also determines the optimal amount of words to print per cluster.
  # it checks the maximal indicator values per cluster.
  # the smallest of those becomes the benchmark and if one word has a higher indval than
  # this it is also drawn on the plot
  
  
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
  
  
  # colnames(processedData[[2]])
  # modeledData[[2]] <- cbind(processedData[[2]], as.numeric(Cluster))
  # colnames(modeledData[[2]]) <- c(colnames(processedData[[2]]), "Cluster")
  modeledData[[2]] <-
    cbind(processedData$MetaMatrix, "Cluster" = Cluster)
  
  
  # Representative papers based on the percentage of significant words in each paper
  # only the papers from signIndSpeciesValues from processed data
  ClusterContent <-
    signIndSpeciesValues[, "names(indSpeciesValues$pval)"]
  ClusterContent <- as.data.frame(ClusterContent)
  
  # select said columns (words) from processedData which is a 0-1 matrix of papers and words
  representativePapers <-
    as.data.frame(processedData$BinaryWordList[, ClusterContent[, 1]])
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
  
  modeledData[[4]] <- processedData$numberOfWords
  names(modeledData) <-
    c("IndVal",
      "MetaMatrix",
      "RepresentativePapers",
      "numberOfWords")
  
  
  ### save each paper into one new folder
  if (dir.exists("PdfsPerCluster/"))
  {
    warning("The existing paper-cluster folders have been overwritten")
  }
  dir.create("PdfsPerCluster/", showWarnings = FALSE)
  for (i in 1:nlevels(as.factor(cutmodel))) {
    dir.create(paste("PdfsPerCluster/", i))
    file.copy(
      paste0("PDFs/", rownames(representativePapersEasyToOpen[representativePapersEasyToOpen[, 2] ==
                                                                i,])),
      to = paste("PdfsPerCluster/", i),
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
    modeledDataFile <-
      paste0("modeledData", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"))
    saveRDS(modeledData, file = modeledDataFile)
    
    if (ordinationFunction == FALSE) {
      cat(
        paste0(
          "\nThe modeled Data is now in your global environment.
          It is also saved as a file in your working directory.
          If you work with the same data again, you can
          skip this step in future analysis by reading in the file:\nmodeledData <- readRDS(file= '",
          modeledDataFile,
          "')\n\n"
        )
      )
    } else {
      cat(
        paste0(
          "Modeled Data saved. You can read it in using:\nmodeledData <- readRDS(file= '",
          modeledDataFile,
          "')\n###############################################################
          ####################################\n\n"
        )
      )
      
    }
    
  }
  return(modeledData)
}
