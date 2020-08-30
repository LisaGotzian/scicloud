#' @title createOrdinationPlot
#'
#' @description The fifth function to the word analysis with scicloud. It takes
#'     the \code{scicloudAnalysis} and creates five different plots: a wordcloud of
#'     the publication communities and four visualizations of the communities
#'     by year and number of citations.
#'
#' @author Matthias Nachtmann, \email{matthias.nachtmann@@stud.leuphana.de},
#'     Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de},
#'     Jia Yan Ng, \email{Jia.Y.Ng@@stud.leuphana.de}
#' @param scicloudAnalysis result of \code{\link{calculateModels}}
#' @param exactPosition logical, the plot function tries to avoid overlapping
#'     labels for the sake of visual simplicity over perfect
#'     precision. When set to \code{TRUE}, the words position will be marked by
#'     a dot and the label will be connected with a line to it.
#' @family scicloud functions
#' @seealso \itemize{
#'     \item \code{\link{calculateModels}} for the preceding step
#'     \item \code{\link{mostImportantPaperPerCluster}} for the proceeding step
#'     \item \code{\link{inspectScicloud}} for a summary of the analysis
#'     }
#' @return This function plots a graphic based on the clustered publication
#'     communities. The citation count and publication dates were fetched
#'     from the Scopus API by \code{\link{getScopusMetaData}}.
#' @importFrom rlang .data
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
#' # 1) pull article metadata from Scopus
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
#'     
createOrdinationPlot <- function(scicloudAnalysis,
                                 exactPosition = FALSE) {
  
  Sys.sleep(1)
  cat("Creating graphic\n")
  Sys.sleep(1)
  
  # If we have metadata, so if the column has less than 25% NA, we do the other plots.
  if(sum(is.na(scicloudAnalysis$metaMatrix[, "CitedBy"]))<nrow(scicloudAnalysis$metaMatrix)/4){
    na_row_idx <- which(rowSums(is.na(scicloudAnalysis$metaMatrix[, c("CitedBy", "Year")])) != 0)
    excluded_file <- scicloudAnalysis$metaMatrix[,"FileName"][na_row_idx]
    if(length(excluded_file) != 0){
      cat(crayon::red("\nCitedBy and/or Year info are not found in", excluded_file))
      cat(crayon::red("\nThe files(s) will be excluded in the following plots!"))
    }
    # omit the rows where CitedBy and Year are NA
    naFreeData <- as.data.frame(scicloudAnalysis$metaMatrix[rowSums(is.na(scicloudAnalysis$metaMatrix[, c("CitedBy", "Year")])) == 0,])
    # CAUTION: to convert the factors level correctly, need to use as.numeric(as.character())
    # aggregate the sum of citations by year and then join the sub data frame by year to calculate the percentage of citation for each row
    citationSum_df <- stats::aggregate(as.numeric(as.character(naFreeData$CitedBy)), by = list(naFreeData$Year), sum)
    colnames(citationSum_df) <- c("Year", "SumByYear")
    citationSum_df <- plyr::join(naFreeData[,c("CitedBy", "Year")], by="Year", citationSum_df)
    naFreeData$citePercent <- as.numeric(as.character(citationSum_df$CitedBy))/as.numeric(as.character(citationSum_df$SumByYear))*100
      
    if (nrow(naFreeData) == 0) {
      cat(
        paste0(
          "Please use your API key to obtain the metadata of your papers from Scopus.\n
          Plots of citations/no. of papers across different years can be generated.\n"
        )
      )
    }
    # create a sub data frame that aggregate the count of each cluster by year
    # then calculate the percentage of no. of cluster for each row
    clusterCount_df <- stats::aggregate(as.numeric(as.character(naFreeData$Cluster)), by = list(naFreeData$Year), length)
    colnames(clusterCount_df) <- c("Year", "CountByYear")
    clusterCount_df<- plyr::join(naFreeData[,c("Cluster", "Year")], by="Year", clusterCount_df)
    naFreeData$ClusterPercent <- 1/clusterCount_df$CountByYear*100
    # create new column to store the cluster in string 
    naFreeData$ClusterString <- paste("Cluster", as.character(naFreeData$Cluster))
    # set ClusterString as factor in order to have a legend arranged in the order of Cluster1, Cluster2... Cluster9, Cluster10 
    naFreeData$ClusterString <- factor(naFreeData$ClusterString, 
                                       levels = unique(naFreeData$ClusterString[gtools::mixedorder(naFreeData$ClusterString)]))
  } # end of if-metadata-there-loop
  
  numberOfClusters <- length(unique(naFreeData$Cluster))
  palCol <- c("#08306b", "#08519c", "#2171b5", "#4292c6", "#6baed6", "#9ecae1", "#c6dbef", "#ccece6", "#99d8c9", "#66c2a4", "#41ae76", "#238b45", "#006d2c", "#00441b")
  
  # Prompt user whether or not to plot the following plots
  ANS <- readline("Plot a word cloud analysis?(y/n)")
  if (substr(ANS, 1, 1) == "y") {
    wordCloudPlot(scicloudAnalysis, exactPosition, palCol)
  }
  ANS <- readline("Plot a stacked Barplot of the No. of citations across different years?(y/n)")
  if (substr(ANS, 1, 1) == "y") {
    StackedBarplot(naFreeData, palCol, "No. of citations of each cluster across years", "Year", "Citations",1)
  }
  ANS <- readline("Plot a stacked Barplot of the percentage of No. of citations across different years?(y/n)")
  if (substr(ANS, 1, 1) == "y") {
    StackedBarplot(naFreeData, palCol, "Percentage of citations of each cluster across years", "Year", "Citations[%]",2)
  }
  ANS <- readline("Plot a stacked Barplot of the No. of papers across different years?(y/n)")
  if (substr(ANS, 1, 1) == "y") {
    StackedBarplot(naFreeData, palCol, "No. of papers of each cluster across years", "Year", "Amount of papers",3)
  }
  ANS <- readline("Plot a stacked Barplot of the percentage of no. of papers across different years?(y/n)")
  if (substr(ANS, 1, 1) == "y") {
    StackedBarplot(naFreeData, palCol, "Percentage of No. of papers of each cluster across years", "Year", "Amount of papers[%]",4)
  }
}

wordCloudPlot <- function(scicloudAnalysis, exact, palCol){
  n <- length(unique(scicloudAnalysis$metaMatrix[,"Cluster"])) # no. of cluster
  plt <- ggplot2::ggplot(scicloudAnalysis[[1]]) + ggplot2::ggtitle("Cluster plot of the publication communities")+ 
    ggplot2::scale_fill_manual(values = grDevices::colorRampPalette(palCol, bias = 1)(n))+  
    ggplot2::labs(x = "DCA 1", y = "DCA 2") +
    # override.aes remove "a" label in the legend
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL, override.aes = list(label = ""))) +
    ggplot2::theme_classic(base_size = 16) 
  
  if(exact){
    plt <- plt + ggplot2::geom_point(
      ggplot2::aes(x = scicloudAnalysis[[1]]$DCA1, y = scicloudAnalysis[[1]]$DCA2),
      size = 4,
      color = 'grey'
    ) +
      ggrepel::geom_label_repel(
        ggplot2::aes(
          scicloudAnalysis[[1]]$DCA1,
          scicloudAnalysis[[1]]$DCA2,
          # order the legend label in ascending order 
          fill = factor(subset, levels = unique(subset[gtools::mixedorder(subset)])),
          label = scicloudAnalysis[[1]][, "names(indSpeciesValues$pval)"]
        ),
        fontface = 'bold',
        color = 'white',
        box.padding = ggplot2::unit(0.35, "lines"),
        point.padding = ggplot2::unit(0.3, "lines"),
        segment.color = 'grey50'
      ) 
  }
  else{
    plt <- plt + ggrepel::geom_label_repel(
      ggplot2::aes(
        scicloudAnalysis[[1]]$DCA1,
        scicloudAnalysis[[1]]$DCA2,
        # order the legend label in ascending order 
        fill = factor(subset, levels = unique(subset[gtools::mixedorder(subset)])),
        label = scicloudAnalysis[[1]][, "names(indSpeciesValues$pval)"]
      ),
      fontface = 'bold',
      color = 'white',
      box.padding = ggplot2::unit(0.15, "lines"),
      segment.color = NA
    )
  }
  graphics::plot(plt)
}

StackedBarplot <- function(data, palCol, title, xlabel, ylabel, plot=c(1,2,3,4)){
  n <- length(unique(data$Cluster)) # calculate the number of cluster
  plt <- ggplot2::ggplot(data) +
    ggplot2::ggtitle(title)+ 
    ggplot2::scale_fill_manual(values = grDevices::colorRampPalette(palCol, bias = 1)(n))+  
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))+
    ggplot2::theme_classic(base_size = 16)
  # somehow if added ggplot2::labs(x = xlabel, y = ylabel) here doesn't work, add within ifelse
  
  #citations per year - stacked bar plot
  if(plot==1){
    plt <- plt + ggplot2::aes(
      x = .data$Year,
      y = as.numeric(.data$CitedBy),
      fill = .data$ClusterString
    ) + ggplot2::geom_bar(stat = "identity") + ggplot2::labs(x = xlabel, y = ylabel)
  }
  # citations per year - stacked bar plot, percentage
  else if(plot==2){
    plt <- plt + ggplot2::aes(
      x = .data$Year,
      y = as.numeric(.data$citePercent),
      fill = .data$ClusterString
    ) + ggplot2::geom_bar(stat = "identity") + ggplot2::labs(x = xlabel, y = ylabel)  
  }
  # paper per cluster per year - stacked bar plot
  else if(plot==3){
    plt <- plt + ggplot2::aes(
      x = .data$Year,
      fill = .data$ClusterString
    ) + ggplot2::geom_bar(width = .9) + ggplot2::labs(x = xlabel, y = ylabel) 
  }
  # paper per cluster per year - stacked bar plot, percentage
  else if(plot==4){
    plt <- plt + ggplot2::aes(
      x = .data$Year,
      y = as.numeric(.data$ClusterPercent),
      fill = .data$ClusterString
    ) + ggplot2::geom_bar(stat = "identity") +  ggplot2::labs(x = xlabel, y = ylabel) 
  }
  graphics::plot(plt)
}


