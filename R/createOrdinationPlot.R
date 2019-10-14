#' @title createOrdinationPlot
#'
#' @description The fourth function to the word analysis with ginko. It creates
#'     five different plots that set the results in context to the years
#'     published and more. To show meaningful graphics, the use of
#'     \code{\link[ginko]{getScopusMetaData}} is recommended.
#'
#' @author Matthias Nachtmann, \email{matthias.nachtmann@@stud.leuphana.de},
#'     Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de}
#' @param modeledData result of \code{\link[ginko]{calculateModels}}
#' @param exactPosition \code{TRUE} or \code{FALSE}, the plot function tries
#'     to avoid overlapping labels for the sake of visual siplicity over perfect
#'     precission. When set to \code{TRUE}, the words position will be marked by
#'     a dot and the label will be connected with a line to it.
#' @param ordinationFunction for internal use.
#' @family ginko functions
#' @seealso \code{\link{calculateModels}} for the preceding step,
#'     \code{\link{mostImportantPaperPerCluster}} and
#'     \code{\link{inspectGinko}} for a summary of the analysis
#' @return a graphic based on the calculated model and some additional barplot
#'     to deepen the understanding of the dataset.
#' @export
#' @examples \dontrun{
#' placeholder}
createOrdinationPlot <- function(modeledData,
                                 exactPosition = FALSE,
                                 ordinationFunction = FALSE) {
  # if(colorPalette == "default"){
  #   colorPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
  # }
  
  Sys.sleep(1)
  cat("Creating graphic\n")
  Sys.sleep(1)
  
  if (exactPosition == TRUE) {
    # This is the cluster cloud plot with labels to exact positions
    ordinationPlot <- ggplot2::ggplot(modeledData[[1]]) +
      ggplot2::geom_point(
        ggplot2::aes(x = modeledData[[1]]$DCA1, y = modeledData[[1]]$DCA2),
        size = 4,
        color = 'grey'
      ) +
      ggrepel::geom_label_repel(
        ggplot2::aes(
          modeledData[[1]]$DCA1,
          modeledData[[1]]$DCA2,
          fill = factor(subset),
          label = modeledData[[1]][, "names(indSpeciesValues$pval)"]
        ),
        fontface = 'bold',
        color = 'white',
        box.padding = ggplot2::unit(0.35, "lines"),
        point.padding = ggplot2::unit(0.3, "lines"),
        segment.color = 'grey50'
      ) +
      ggplot2::labs(x = "DCA 1", y = "DCA 2") +
      ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
      #ggplot2::scale_x_continuous(breaks=c(seq(-100,100,0.5))) +
      #ggplot2::scale_y_continuous(breaks=c(seq(-100,100,0.5))) +
      ggplot2::theme_classic(base_size = 16)
    
    
  } else{
    # the plot without labels to exact positions
    ordinationPlot <- ggplot2::ggplot(modeledData[[1]]) +
      ggrepel::geom_label_repel(
        ggplot2::aes(
          modeledData[[1]]$DCA1,
          modeledData[[1]]$DCA2,
          fill = factor(subset),
          label = modeledData[[1]][, "names(indSpeciesValues$pval)"]
        ),
        fontface = 'bold',
        color = 'white',
        box.padding = ggplot2::unit(0.15, "lines"),
        segment.color = NA
      ) +
      ggplot2::labs(x = "DCA 1", y = "DCA 2") +
      ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
      #ggplot2::scale_x_continuous(breaks=c(seq(-100,100,0.5))) +
      #ggplot2::scale_y_continuous(breaks=c(seq(-100,100,0.5))) +
      ggplot2::theme_classic(base_size = 16)
  }
  
  
  
  if (ordinationFunction == TRUE) {
    readline("Show next plot?")
  }
  
  graphics::plot(ordinationPlot)
  # If we have metadata, so if the column has less than 25% NA, we do the other plots.
  if(sum(is.na(modeledData$MetaMatrix[, "CitedBy"]))<nrow(modeledData$MetaMatrix)/4){
    
    
    #preparing the data for meta data plots
    naFreeData1 <-
      subset(modeledData$MetaMatrix,!is.na(modeledData$MetaMatrix[, "Year"]))
    naFreeData2 <- subset(naFreeData1,!is.na(naFreeData1[, "CitedBy"]))
    #naFreeData <- as.data.frame(naFreeData1) # to only have the years
    naFreeData <- as.data.frame(naFreeData2)
    
    citePercent <- c()
    for (i in 1:nrow(naFreeData)) {
      intermediateCitedBy <- as.numeric(naFreeData[i, "CitedBy"])
      intermediateSum <-
        sum(as.numeric(naFreeData$CitedBy[naFreeData[, "Year"] == naFreeData[i, "Year"]]))
      percentage <- intermediateCitedBy / intermediateSum * 100
      citePercent <- c(citePercent, percentage)
    }
    if (nrow(naFreeData) == 0) {
      cat(
        paste0(
          "Please use your API key to obtain the metadata of your papers from scopus. Then, I'll be able to generate plots of citations and years.\n"
        )
      )
    }
    naFreeData$citePercent <- citePercent
    
    
    ClusterPercent <- c()
    for (i in 1:nrow(naFreeData)) {
      intermediateCluster <- as.numeric(naFreeData[i, "Cluster"])
      intermediateSum <-
        sum(as.numeric(naFreeData$Cluster[naFreeData[, "Year"] == naFreeData[i, "Year"]]))
      percentage <- intermediateCluster / intermediateSum * 100
      ClusterPercent <- c(ClusterPercent, percentage)
    }
    naFreeData$ClusterPercent <- ClusterPercent
    
    
    
    naFreeData$ClusterString <-
      apply(naFreeData["Cluster"], 1, function(x)
        paste0("Cluster ", x))
    
    
    
    # #stacked area chart
    # citationsAreaChart <- ggplot2::ggplot(naFreeData,
    #   ggplot2::aes(
    #     x=naFreeData$Year,
    #     y=as.numeric(naFreeData$CitedBy),
    #     fill=naFreeData$Cluster)
    # ) +
    #    ggplot2::geom_area()
    
    
    
    
    
    # #stacked area chart, percentage
    # citationsAreaChartPercent <- ggplot2::ggplot(naFreeData,
    #   ggplot2::aes(
    #     x=as.numeric(naFreeData$Year),
    #     y=as.numeric(naFreeData$citePercent),
    #     fill=naFreeData$Cluster
    #   )
    # ) +
    #    ggplot2::geom_area()
    
    
    
    
    
    #citations per year - stacked bar plot
    citationsStackedBarPlot <- ggplot2::ggplot(
      naFreeData,
      ggplot2::aes(
        x = naFreeData$Year,
        y = as.numeric(naFreeData$CitedBy),
        fill = naFreeData$ClusterString
      )
    ) +
      ggplot2::labs(x = "Year", y = "Citations") +
      ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::theme_classic(base_size = 16)
    
    
    
    
    # citations per year - stacked bar plot, percentage
    citationsStackedBarPlotPercent <- ggplot2::ggplot(
      naFreeData,
      ggplot2::aes(
        x = naFreeData$Year,
        y = as.numeric(naFreeData$citePercent),
        fill = naFreeData$ClusterString
      )
    ) +
      ggplot2::labs(x = "Year", y = "Citations [%]") +
      ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::theme_classic(base_size = 16)
    
    
    
    # paper per cluster per year - stacked bar plot
    paperPerClusterPerYearStackedBarPlot <-
      ggplot2::ggplot(
        naFreeData,
        ggplot2::aes(
          x = naFreeData$Year,
          y = as.numeric(naFreeData$Cluster),
          fill = naFreeData$ClusterString
        )
      ) +
      ggplot2::labs(x = "Year", y = "Amount of papers") +
      ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::theme_classic(base_size = 16)
    
    
    # paper per cluster per year - stacked bar plot, percentage
    paperPerClusterPerYearStackedBarPlotPercent <-
      ggplot2::ggplot(
        naFreeData,
        ggplot2::aes(
          x = naFreeData$Year,
          y = as.numeric(naFreeData$ClusterPercent),
          fill = naFreeData$ClusterString
        )
      ) +
      ggplot2::labs(x = "Year", y = "Amount of papers [%]") +
      ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::theme_classic(base_size = 16)
    
    # #map experiments
    
    # modeledData[[2]] <- cbind(processedData[[2]], "AffiliationComplete"=rep(NA, nrow(modeledData[[2]])))
    
    # for (i in 1:nrow(modeledData[[2]])) {
    #   Affiliation <- as.vector(unlist(strsplit(modeledData[[2]][i,"Affiliation"], ", ")))
    #   AffiliationCity <- as.vector(unlist(strsplit(modeledData[[2]][i,"Affiliation-City"], ", ")))
    #   AffiliationCountry <- as.vector(unlist(strsplit(modeledData[[2]][i,"Affiliation-Country"], ", ")))
    
    
    
    #   intermediateAffiliationComplete <- c()
    #   if((length(Affiliation) == length(AffiliationCity))&(length(Affiliation) == length(AffiliationCountry))){
    
    #     for(j in 1:length(Affiliation)){
    
    #       currentAffiliation <- paste(Affiliation[j], AffiliationCity[j], AffiliationCountry[j], sep = ", ")
    #       if(currentAffiliation == "NA, NA, NA"){currentAffiliation<-NA}
    
    #       intermediateAffiliationComplete <- c(intermediateAffiliationComplete, currentAffiliation)
    
    #     }
    
    #     modeledData[[2]][i,"AffiliationComplete"] <- paste(intermediateAffiliationComplete, collapse="; ")
    
    
    #   }else{
    #     modeledData[[2]][i,"AffiliationComplete"] <- NA
    #   }
    # }
    
    # locationMatrix
    
    # modeledData[[2]] <- cbind(processedData[[2]], "Longitude"=rep(NA, nrow(modeledData[[2]])))
    # modeledData[[2]] <- cbind(processedData[[2]], "Latitide"=rep(NA, nrow(modeledData[[2]])))
    
    # for (i in 1:nrow(modeledData[[2]])) {
    
    #   latlon = geocode(naFreeData[i,1])
    #   modeledData[[2]][i,"Longitude"] <- as.numeric(latlon[1])
    #   modeledData[[2]][i,"Latitide"] <- as.numeric(latlon[2])
    # }
    readline("Show next plot?")
    
    graphics::plot(citationsStackedBarPlot)
    
    readline("Show next plot?")
    
    graphics::plot(citationsStackedBarPlotPercent)
    
    readline("Show next plot?")
    
    graphics::plot(paperPerClusterPerYearStackedBarPlot)
    
    readline("Show next plot?")
    
    graphics::plot(paperPerClusterPerYearStackedBarPlotPercent)
    
  } # end of if-metadata-there-loop
}