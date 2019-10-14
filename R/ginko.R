########################################################################
# Package help
########################################################################
#' ginko: A Package For Word Analysis Of Scientific Papers
#'
#' The ginko package provides a word analysis for scientific papers as PDFs.
#' It analyzes them using indicator values and clustering or network analysis.
#' Includes easy-to-grasp graphics.
#'
#' @section Pre-requisites:
#' Make sure to have all PDFs in a folder called 'PDFs' in your working
#'     directory.
#' Metadata of the papers allow for a more complete analysis. Metadata will be
#'     downloaded from Elsevier and it requires you to have an API key.
#'     To acquire your API key, go to \url{https://dev.elsevier.com/user/login}.
#' See examples on how to use the package.
#'
#' @section Ginko functions:
#' The ginko functions are built in the following order:
#' \enumerate{
#'     \item \code{\link{createTextMatrixFromPDF}}
#'     \item \code{\link{ordinationCluster}}
#'     \item \code{\link{inspectGinko}}
#' }
#' The wrapper function \code{\link{ordinationCluster}} can also be done step
#'     by step:
#' \enumerate{
#'     \item \code{\link{processMetaDataMatrix}}
#'     \item \code{\link{getScopusMetaData}}
#'     \item \code{\link{calculateModels}}
#'     \item \code{\link{calculateNetwork}}
#'     \item \code{\link{createOrdinationPlot}}
#'     \item \code{\link{mostImportantPaperPerCluster}}
#' }
#'
#' @author Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de},
#'     Matthias Nachtmann, \email{matthias.nachtmann@@stud.leuphana.de}
#' @examples
#' \dontrun{
#' myAPIKey <- "YOUR_API_KEY"
#'
#' metaMatrix <- createTextMatrixFromPDF(saveToWd = TRUE)
#' # Don't give up! Keep running it even if you get errors.
#'
#' # metaMatrix <- readRDS(file= 'metaMatrix2019_______FILL_IN_HERE________')
#' GinkoAnalysis <- ordinationCluster(metaMatrix, myAPIKey = myAPIKey,
#'                            stemWords = FALSE, longMessages = FALSE,
#'                            saveToWd = TRUE, method = "hclust")
#'
#' GinkoSpecs <- inspectGinko(modeledData = GinkoAnalysis)
#'                                    }
#'
#' @docType package
#' @name ginko
NULL
