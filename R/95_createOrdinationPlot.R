# The fifth function to the word analysis with scicloud. 
#
#' @importFrom rlang .data
  
createOrdinationPlot <- function(scicloudAnalysis,
                                 exactPosition = FALSE) {
  
  Sys.sleep(1)
  cat("Creating graphic\n")
  Sys.sleep(1)
  
  # omit the rows where CitedBy and Year are NA
  naFreeData <- as.data.frame(scicloudAnalysis$metaMatrix[rowSums(is.na(scicloudAnalysis$metaMatrix[, c("CitedBy", "Year")])) == 0,c("Year", "Cluster", "CitedBy")])
  
  if (nrow(naFreeData) == 0) {
    stop("Info needed to make plots for analysis is missing. Please make sure you can connect to Scopus with your API key to retrieve the update in metaMatrix.\n")
  }
  
  # CAUTION: to convert the factors level correctly, need to use as.numeric(as.character())
  # aggregate the sum of citations by year and then join the sub data frame by year to calculate the percentage of citation for each row
  citation_SumByYearNCluster <- stats::aggregate(as.numeric(as.character(naFreeData$CitedBy)), by=list(naFreeData$Year, naFreeData$Cluster), sum)
  citation_SumByYear <- stats::aggregate(as.numeric(as.character(naFreeData$CitedBy)), by = list(naFreeData$Year), sum)
  
  df_citation <- plyr::join(citation_SumByYearNCluster, by="Group.1", citation_SumByYear)
  colnames(df_citation) <- c("Year", "Cluster", "CitedBy", "SumByYear")
  df_citation['CitePercent'] <- paste0(round(df_citation$CitedBy/df_citation$SumByYear*100,2), '%')
  df_citation['ClusterString'] <- paste("Cluster", as.character(df_citation$Cluster))
  
  # create a sub data frame that aggregate the count of each cluster by year
  # then calculate the percentage of no. of cluster for each row
  paperCount_byYear <- stats::aggregate(as.numeric(as.character(naFreeData$Cluster)), by = list(naFreeData$Year), length)
  colnames(paperCount_byYear) <- c("Year", "CountByYear")
  paperCount_byyearNCluster <- stats::aggregate(naFreeData$Cluster, by = list(naFreeData$Year,naFreeData$Cluster), length)
  colnames(paperCount_byyearNCluster) <- c("Year", "Cluster", "Counts")
  
  df_PaperCount<- plyr::join(paperCount_byyearNCluster, by="Year", paperCount_byYear)
  df_PaperCount['ClusterPercent'] <- paste0(round(df_PaperCount$Counts/df_PaperCount$CountByYear*100,2), '%')
  df_PaperCount['ClusterString'] <- paste("Cluster", as.character(df_PaperCount$Cluster))
  
  #} # end of if-metadata-there-loop
  
  
  numberOfClusters <- length(unique(naFreeData$Cluster))
  palCol <- c("#08306b", "#08519c", "#2171b5", "#4292c6", "#6baed6", "#9ecae1", "#c6dbef", "#ccece6", "#99d8c9", "#66c2a4", "#41ae76", "#238b45", "#006d2c", "#00441b")
  
  # Prompt user whether or not to plot the following plots
  ANS <- readline("Plot a word cloud analysis?(y/n)")
  if (substr(ANS, 1, 1) == "y") {
    wordCloudPlot(scicloudAnalysis, exactPosition, palCol)
  }
  ANS <- readline("Plot a stacked barplot of the no. of citations (with percentage) across years?(y/n)")
  if (substr(ANS, 1, 1) == "y") {
    StackedBarplot(df_citation, palCol, "Citations of each cluster across years", "Year", "Number of citations",1)
  }
  ANS <- readline("Plot a stacked barplot of the no. of papers (with percentage) across years?(y/n)")
  if (substr(ANS, 1, 1) == "y") {
    StackedBarplot(df_PaperCount, palCol, "Scientific papers of each cluster across years", "Year", "Number of papers",2)
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
  
  #citations count per year - stacked bar plot
  if(plot==1){
    plt <- plt + ggplot2::aes(
      x = .data$Year,
      y = .data$CitedBy,
      fill = .data$ClusterString
    ) + ggplot2::geom_bar(stat = "identity") + ggplot2::labs(x = xlabel, y = ylabel) + ggplot2::geom_text(ggplot2::aes(label = .data$CitePercent), position = ggplot2::position_stack(vjust = 0.5))
  }
  # papers count per year - stacked bar plot, percentage
  else if(plot==2){
    plt <- plt + ggplot2::aes(
      x = .data$Year,
      y = .data$Counts,
      fill = .data$ClusterString
    ) + ggplot2::geom_bar(stat = "identity") + ggplot2::labs(x = xlabel, y = ylabel) + ggplot2::geom_text(ggplot2::aes(label = .data$ClusterPercent), position = ggplot2::position_stack(vjust = 0.5))  
  }
  graphics::plot(plt)
}


