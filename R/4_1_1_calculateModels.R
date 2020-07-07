#' @title calculateModels
#'
#' @description The fourth function to the word analysis with scicloud.
#'     It takes the cleaned data from \code{\link{processMetaDataMatrix}}
#'     and calculates clusters using \code{\link[stats]{hclust}}. The clusters
#'     are publication communities based on the words used in the papers.
#'     To then identify the words relevant to the communities, it runs an
#'     indicator species analysis.
#'     
#'     Each word receives an indicator species value by \code{\link[labdsv]{indval}}
#'     for each cluster, showing how representative a word is for a cluster. The top
#'     representative words will be used in the further process for \code{
#'     \link{createOrdinationPlot}}.
#'
#' @author Matthias Nachtmann, \email{matthias.nachtmann@@stud.leuphana.de},
#'     Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de}
#' @param processedMetaDataMatrix result of \code{\link{processMetaDataMatrix}}
#' @param numberOfClusters integer or NA; \cr
#'     an integer sets the number of clusters manually \cr
#'     for NA, the function automatically calculates results for 1 till 12 clusters
#' @param minWordsPerCluster minimum number of words to be plotted per cluster
#'     in \code{\link{createOrdinationPlot}}.
#' @param maxWordsPerCluster maximum number of words to be plotted per cluster
#'     in \code{\link{createOrdinationPlot}}.
#' @param p the p-value that sets the significance level of individual words for
#'     the indicator species analysis. Only significant words will be plotted
#'     in \code{\link{createOrdinationPlot}}.
#' @param dendrogram logical, whether or not to show a dendrogram of the calculated
#'     clusters.
#' @param dendroLabels allows "truncated" or "break". This either truncates the
#'     labels of the dendrogram leaves or puts a line break. Line breaks are not
#'     recommended for a large number of PDFs.
#' @param generateWordlist logical, if set to \code{TRUE}, it generates a wordlist
#'     called "scicloudWordlist.csv" in your working directory. The list
#'     contains all significant words from the indicator species analysis
#'     that are representative for the respective paper clusters. To work with the
#'     new wordlist, read it in using \code{keepWordsFile} as an argument
#'     to \code{\link{processMetaDataMatrix}} or \code{\link{ordinationCluster}}.
#' @param saveToWd a logical parameter whether or not to save the output of the
#'     function to the working directory. This is especially useful for later
#'     analysis steps. The file can be read in by using \code{\link[base]{readRDS}}.
#' @param ordinationFunction internal variable
#' @seealso \itemize{
#'     \item \code{\link{processMetaDataMatrix}} for the preceding step
#'     \item \code{\link{createOrdinationPlot}} for the next step and
#'     \item \code{\link{mostImportantPaperPerCluster}} for the next step
#'     \item \code{\link{inspectScicloud}} for a summary of the analysis
#'     }
#'     
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
#' @family scicloud functions
#' @export
#' @examples
#' \dontrun{
#' 
#' ### The normal workflow of scicloud
#' myAPIKey <- "YOUR_API_KEY"
#' metaMatrix <- createTextMatrixFromPDF()
#' 
#' 
#' # instead of ordinationCluster(), we can also run this
#' # workflow step by step.
#' 
#' # 1) pull article metadata from scopus
#' metaMatrix <- getScopusMetaData(metaMatrix, myAPIKey)
#' 
#' # 2) process the full texts
#' processedMetaDataMatrix <- processMetaDataMatrix(
#'           metaMatrix,
#'           list(language = "SMART",
#'           stemWords = TRUE,
#'           saveToWd = FALSE),
#'           ignoreWords = c("Abstract", "Bulletin", "Editor"))
#'                                   
#' # 3) run the cluster analysis to determine publication communities
#' scicloudAnalysis <- calculateModels(processedMetaDataMatrix)
#' 
#' # 4) visualize the results
#' createOrdinationPlot(scicloudAnalysis)
#' 
#' # 5) a list of the most important papers per cluster
#' mostImportantPaperPerCluster(scicloudAnalysis)
#' 
#' # 6) a summary of the analysis
#' scicloudSpecs <- inspectScicloud(scicloudAnalysis)
#'     }
  
calculateModels <- function(processedMetaDataMatrix,
                            numberOfClusters = NA,
                            minWordsPerCluster = 5,
                            maxWordsPerCluster = 10,
                            p = 0.05,
                            dendrogram = TRUE,
                            dendroLabels = "truncated",
                            generateWordlist = FALSE,
                            saveToWd = TRUE,
                            ordinationFunction = FALSE) {
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
  

  cat("Calculating models\n")

  
  model <-
    vegan::decorana(processedMetaDataMatrix$Tf_Idf, iweigh = 0) #all row sums must be >0 in the community matrix

  axisPositions <- vegan::scores(model, display = c("species"))
  
  
  #cluster
  # replaced agnes by hclust from mclust
  disthclust <-
    stats::dist(processedMetaDataMatrix$Tf_Idf, method = "euclidian")
  modelclust <- stats::hclust(disthclust, method = "ward.D")
  indSpeciesValues <- c()
  
  if (is.na(numberOfClusters)) {
    
    cat("Determining the optimal number of clusters...\n")
    
    
    numberOfSignificantIndicators <- vector(length = 12)
    for (cluster in 1:12) {# test up till 12 clusters
      
      cutmodel <-
        stats::cutree(modelclust, k = cluster) #assigns a cluster number to every paper
      indSpeciesValues <-
        labdsv::indval(processedMetaDataMatrix$Tf_Idf, cutmodel, numitr = 1000)
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
    labdsv::indval(processedMetaDataMatrix$Tf_Idf, cutmodel, numitr = 1000)
  
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
  
  
  
  scicloudAnalysis <- list()
  scicloudAnalysis[[1]] <- signIndSpeciesValuesInclSubsetRow
  
  
  
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
        labels = sapply(processedMetaDataMatrix$metaMatrix[, "FileName"], wordwrap, len =
                          15),
        padj = 1
      )
      #18th column is the filename, strwrap() splits the labels
    }
    if (dendroLabels == "truncated") {
      # this truncates the labels to 18 characters, followed by "..."
      longLabels <- processedMetaDataMatrix$metaMatrix[, "FileName"]
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
  
  
  scicloudAnalysis[[2]] <-
    cbind(processedMetaDataMatrix$metaMatrix, "Cluster" = Cluster)
  
  
  # Representative papers based on the percentage of significant words in each paper
  # only the papers from signIndSpeciesValues from processed data
  ClusterContent <-
    signIndSpeciesValues[, "names(indSpeciesValues$pval)"]
  ClusterContent <- as.data.frame(ClusterContent)
  
  # select said columns (words) from processedMetaDataMatrix which is a tf_idf matrix of papers and words
  representativePapers <-
    as.data.frame(processedMetaDataMatrix$Tf_Idf[, ClusterContent[, 1]])
  rownames(representativePapers) <-
    processedMetaDataMatrix$metaMatrix[, "FileName"] # take the filenames as row names
  
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
    trimws(processedMetaDataMatrix$metaMatrix[, "FileName"]) # take the filenames as row names
  colnames(representativePapersEasyToOpen) <-
    c("percentageOfSignWordsInPaper", "Cluster")
  
  scicloudAnalysis[[3]] <- representativePapersEasyToOpen
  
  scicloudAnalysis[[4]] <- processedMetaDataMatrix$wordList
  names(scicloudAnalysis) <-
    c("IndVal",
      "metaMatrix",
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
    save_data(scicloudAnalysis, "scicloudAnalysis", long_msg = !ordinationFunction)
  }
  return(scicloudAnalysis)
}
