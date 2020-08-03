#' @title createOrdinationPlot
#'
#' @description The fifth function to the word analysis with scicloud. It takes
#'     the \code{scicloudAnalysis} and creates five different plots: a wordcloud of
#'     the publication communities and four visualizations of the communities
#'     by year and number of citations.
#'
#' @author Matthias Nachtmann, \email{matthias.nachtmann@@stud.leuphana.de},
#'     Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de}
#' @param scicloudAnalysis result of \code{\link{calculateModels}}
#' @param exactPosition logical, the plot function tries to avoid overlapping
#'     labels for the sake of visual simplicity over perfect
#'     precision. When set to \code{TRUE}, the words position will be marked by
#'     a dot and the label will be connected with a line to it.
#' @param ordinationFunction internal variable
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
                                 exactPosition = FALSE,
                                 ordinationFunction = FALSE) {
  
  Sys.sleep(1)
  cat("Creating graphic\n")
  Sys.sleep(1)
  
  if (exactPosition == TRUE) {
    # This is the cluster cloud plot with labels to exact positions
    ordinationPlot <- ggplot2::ggplot(scicloudAnalysis[[1]]) +
      ggplot2::geom_point(
        ggplot2::aes(x = scicloudAnalysis[[1]]$DCA1, y = scicloudAnalysis[[1]]$DCA2),
        size = 4,
        color = 'grey'
      ) +
      ggrepel::geom_label_repel(
        ggplot2::aes(
          scicloudAnalysis[[1]]$DCA1,
          scicloudAnalysis[[1]]$DCA2,
          fill = factor(subset),
          label = scicloudAnalysis[[1]][, "names(indSpeciesValues$pval)"]
        ),
        fontface = 'bold',
        color = 'white',
        box.padding = ggplot2::unit(0.35, "lines"),
        point.padding = ggplot2::unit(0.3, "lines"),
        segment.color = 'grey50'
      ) +
      ggplot2::labs(x = "DCA 1", y = "DCA 2") +
      ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
      ggplot2::theme_classic(base_size = 16)
    
    
  } else {
    # the plot without labels to exact positions
    ordinationPlot <- ggplot2::ggplot(scicloudAnalysis[[1]]) +
      ggrepel::geom_label_repel(
        ggplot2::aes(
          scicloudAnalysis[[1]]$DCA1,
          scicloudAnalysis[[1]]$DCA2,
          fill = factor(subset),
          label = scicloudAnalysis[[1]][, "names(indSpeciesValues$pval)"]
        ),
        fontface = 'bold',
        color = 'black',
        box.padding = ggplot2::unit(0.15, "lines"),
        segment.color = NA
      ) +
      ggplot2::ggtitle("Cluster plot of the publication communities")+ 
      ggplot2::scale_fill_brewer(palette = "BuGn")+
      ggplot2::labs(x = "DCA 1", y = "DCA 2") +
      ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
      ggplot2::theme_classic(base_size = 16)
  }
  
  
  
  if (ordinationFunction == TRUE) {
    ANS <- readline("Plot a wordcloud analysis of the publication communities?(y/n)")
    if (substr(ANS, 1, 1) == "y") {
      graphics::plot(ordinationPlot)
    }
  }
  
  # If we have metadata, so if the column has less than 25% NA, we do the other plots.
  if(sum(is.na(scicloudAnalysis$metaMatrix[, "CitedBy"]))<nrow(scicloudAnalysis$metaMatrix)/4){
    
    
    #preparing the data for meta data plots
    naFreeData1 <-
      subset(scicloudAnalysis$metaMatrix,!is.na(scicloudAnalysis$metaMatrix[, "Year"]))
    naFreeData2 <- subset(naFreeData1,!is.na(naFreeData1[, "CitedBy"]))
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
          "Please use your API key to obtain the metadata of your papers from Scopus. Then, I'll be able to generate plots of citations and years.\n"
        )
      )
    }
    naFreeData$citePercent <- citePercent
    
    
    ClusterPercent <- c()
    for (i in 1:nrow(naFreeData)) {
      intermediateSum <-
        length(naFreeData$Cluster[naFreeData[, "Year"] == naFreeData[i, "Year"]])
      percentage <- 1 / intermediateSum * 100
      ClusterPercent <- c(ClusterPercent, percentage)
    }
    naFreeData$ClusterPercent <- ClusterPercent
    
    
    
    naFreeData$ClusterString <-
      apply(naFreeData["Cluster"], 1, function(x)
        paste0("Cluster ", x))
    
    
    
    #citations per year - stacked bar plot
    citationsStackedBarPlot <- ggplot2::ggplot(
      naFreeData,
      ggplot2::aes(
        x = .data$Year,
        y = as.numeric(.data$CitedBy),
        fill = .data$ClusterString
      )
    ) +
      ggplot2::ggtitle("No. of citations of each cluster across years")+ 
      ggplot2::scale_fill_brewer(palette = "BuGn")+
      ggplot2::labs(x = "Year", y = "Citations") +
      ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::theme_classic(base_size = 16)
      
    
    
    # citations per year - stacked bar plot, percentage
    citationsStackedBarPlotPercent <- ggplot2::ggplot(
      naFreeData,
      ggplot2::aes(
        x = .data$Year,
        y = as.numeric(.data$citePercent),
        fill = .data$ClusterString
      )
    ) +
      ggplot2::ggtitle("Percentage of citations of each cluster across years")+ 
      ggplot2::scale_fill_brewer(palette = "BuGn")+
      ggplot2::labs(x = "Year", y = "Citations [%]") +
      ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::theme_classic(base_size = 16)
    
    
    
    # paper per cluster per year - stacked bar plot
    paperPerClusterPerYearStackedBarPlot <- 
            ggplot2::ggplot(
              naFreeData,
              ggplot2::aes(
				      x = .data$Year,
				      fill = .data$ClusterString
			)
		) +
    ggplot2::ggtitle("No. of papers of each cluster across years")+ 
    ggplot2::scale_fill_brewer(palette = "BuGn")+
		ggplot2::labs(x = "Year", y = "Amount of papers") +
		ggplot2::guides(fill=ggplot2::guide_legend(title = NULL)) + 
		ggplot2::geom_bar(width = .9) +
    ggplot2::scale_x_discrete()+
		ggplot2::theme_classic(base_size = 16)
    
    # paper per cluster per year - stacked bar plot, percentage
    paperPerClusterPerYearStackedBarPlotPercent <-
      ggplot2::ggplot(
        naFreeData,
        ggplot2::aes(
          x = .data$Year,
          y = as.numeric(.data$ClusterPercent),
          fill = .data$ClusterString
        )
      ) +
      ggplot2::ggtitle("Percentage of no. of papers of each cluster across years")+ 
      ggplot2::scale_fill_brewer(palette = "BuGn")+
      ggplot2::labs(x = "Year", y = "Amount of papers [%]") +
      ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::theme_classic(base_size = 16)

    ANS <- readline("Plot a stacked Barplot of the no. of citations across different years?(y/n)")
    if (substr(ANS, 1, 1) == "y") {
      graphics::plot(citationsStackedBarPlot)
    }
    ANS <- readline("Plot a stacked Barplot of the percentage of no. of citations across different years?(y/n)")
    if (substr(ANS, 1, 1) == "y") {
      graphics::plot(citationsStackedBarPlotPercent)
    }
    ANS <- readline("Plot a stacked Barplot of the no. of papers across different years?(y/n)")
    if (substr(ANS, 1, 1) == "y") {
      graphics::plot(paperPerClusterPerYearStackedBarPlot)
    }
    ANS <- readline("Plot a stacked Barplot of the percentage of no. of papers across different years?(y/n)")
    if (substr(ANS, 1, 1) == "y") {
      graphics::plot(paperPerClusterPerYearStackedBarPlotPercent)
    }
  } # end of if-metadata-there-loop
}

