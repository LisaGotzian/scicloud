# The fourth function to the word analysis with scicloud.
#
calculateModels <- function(processedMetaDataMatrix,
                            numberOfClusters = NA,
                            minWordsPerCluster = 5,
                            maxWordsPerCluster = 10,
                            p = 0.05,
                            dendrogram = TRUE,
                            dendroLabels = c("truncated", "break")) {
  
  dendroLabels <- match.arg(dendroLabels) #pick the argument input by user
  
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
    if(numberOfClusters > 14){
      ArgumentCheck::addError(
        msg = "Please enter a number of cluster that is not more than 14", 
        argcheck = Check
      )
    }
  }
  ArgumentCheck::finishArgCheck(Check)
  
  cat("Calculating models\n")

  #all row sums must be >0 in the community matrix
  model <- vegan::decorana(processedMetaDataMatrix$Tf_Idf, iweigh = 0) 
  axisPositions <- vegan::scores(model, display = c("species"))
  
  #cluster
  # replaced agnes by hclust from mclust
  disthclust <-stats::dist(processedMetaDataMatrix$Tf_Idf, method = "euclidian")
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

    ANSWER <- readline("With how many clusters would you like to proceed?\nDefine 'numberOfClusters = YOURANSWER' as an argument to skip this next time.\n")
    numberOfClusters <- as.numeric(ANSWER)
    if(is.na(numberOfClusters)){
      stop("Invalid numberOfClusters! It is not a numeric input!")
    }
    if(numberOfClusters > dim(metaMatrix)[1]){
        stop("Invalid input for numberOfClusters! It must be less than total no. of papers available!")
    }
    if(numberOfClusters > 14){
      stop("Please enter a number of cluster that is not more than 14")
    }
  }
  
  #assigns a cluster number to every paper
  cutmodel <- stats::cutree(modelclust, k = numberOfClusters) 
  Cluster <- as.numeric(cutmodel)
  
  # Performs a Dufrene-Legendre Indicator Species Analysis that calculates the indicator value
  # (fidelity and relative abundance) of species in clusters or types.
  indSpeciesValues <-labdsv::indval(processedMetaDataMatrix$Tf_Idf, cutmodel, numitr = 1000)
  
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
  if(!dim(signIndSpeciesValues)[1]){
    stop("No cluster has p-value less than default value = 0.05, set a higher confidence level by defining the p argument e.g. p=0.1")
  }

  
  highestIndValPerCluster <- apply(signIndSpeciesValues[,c(1:numberOfClusters)], 2, max)
  
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
    if(any(!is.na(processedMetaDataMatrix$metaMatrix[,"Authors"]))){
      label<-paste0(processedMetaDataMatrix$metaMatrix[,"Authors"],"_", processedMetaDataMatrix$metaMatrix[,"Year"])
      # when author is not available, use pdf file name instead
      label[which(grepl("NA",label))] <- 
        sub(".*[/]", "", processedMetaDataMatrix$metaMatrix[,"FileName"][which(grepl("NA",label))])
      plotDendrogram(modelclust, label, numberOfClusters)
    } 
    else{
      if (dendroLabels == "truncated") {
        # exclude the file extension as part of the label
        label <- tools::file_path_sans_ext(sub(".*[/]", "", processedMetaDataMatrix$metaMatrix[, "FileName"]))
        # for labels with length > 20:
        # replaced a truncated labels of characters from 1:18 followed by ...
        label[nchar(label)>20] <- paste(substr(label[nchar(label)>20], 1,18),"...", sep = "")
        plotDendrogram(modelclust, label, numberOfClusters)
      }
      else if (dendroLabels == "break") {
        label <- sub(".*[/]", "", processedMetaDataMatrix$metaMatrix[, "FileName"])
        plotDendrogram(modelclust, label, numberOfClusters)
      } 
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
  representativePapers[c(1:nrow(representativePapers))] <- 
    as.matrix(representativePapers)*t(as.matrix(signIndSpeciesValues[,c(1:numberOfClusters)][Cluster]))
  
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
  PdfsPerCluster <- file.path(getwd(), "PdfsPerCluster")
  #PdfsPerCluster <- file.path(do.call(file.path, as.list(strsplit(getwd(), "/")[[1]])), "PdfsPerCluster")
  if (dir.exists(PdfsPerCluster)){
    message("The existing paper-cluster folders have been overwritten")
    nestedFolders <- list.files(PdfsPerCluster, full.names = TRUE)
    do.call(function(x) unlink(x, recursive = TRUE), list(nestedFolders))
  } else{
    dir.create(PdfsPerCluster) 
    cat("PdfsPerCluster folder is created in your working directory")
  }
  # file.copy function does not support file copying of list of file to list of different directory
  # manage the files copying in a loop 
  for(i in 1:numberOfClusters){
    clusterFolder <- file.path(PdfsPerCluster, paste("Cluster", i))
    dir.create(clusterFolder)
    file.copy(from = rownames(representativePapersEasyToOpen[representativePapersEasyToOpen$Cluster==i,]),
              to = clusterFolder,
              copy.mode = TRUE)
  }
  cat(
    paste0(
      "\nAll PDFs have been copied to different subfolders in the new folder 'PdfsPerCluster'
      according to the cluster they belong to.\n"
    )
  )
  
  return(scicloudAnalysis)
}

plotDendrogram <- function(modelclust, label, numberOfClusters){
  palCol <- c("#08306b", "#08519c", "#2171b5", "#4292c6", "#6baed6", "#9ecae1", "#c6dbef", "#ccece6", "#99d8c9", "#66c2a4", "#41ae76", "#238b45", "#006d2c", "#00441b")
  dend <-stats::as.dendrogram(modelclust)
  dendextend::labels(dend) <- label
  par(font = 2, mar=c(1,1,1,10))
  dend <- dendextend::color_labels(dend,k = numberOfClusters, col=grDevices::colorRampPalette(palCol, bias = 1)(numberOfClusters))
  dend <- dendextend::color_branches(dend, k = numberOfClusters, groupLabels=TRUE, 
                                     col=grDevices::colorRampPalette(palCol, bias = 1)(numberOfClusters))
  dend <- dendextend::highlight_branches_lwd(dend)
  graphics::plot(dend, adj = 0.5, main = "Word cluster dendrogram of papers", horiz=TRUE, axes=FALSE)
}