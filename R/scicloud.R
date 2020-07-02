########################################################################
# Package help
########################################################################
#' scicloud: A Package For Word Analysis Of Scientific Papers
#'
#' The scicloud package provides a word analysis for scientific papers as PDFs.
#' It clusters publication communities based on the words used in the papers.
#' To then identify the words relevant to the communities, it uses an indicator
#' species analysis. In the end, it produces a wordcloud and visualizes the
#' communities by year and number of citations.
#' The package also offers a network analysis approach for which
#' Gephi or a different graphing software is required.
#'
#' @section Pre-requisites:
#' Make sure to have the PDFs of your choice in a folder called 'PDFs' in
#'     your working directory.
#'     
#'     Alternatively, you run an analysis of search results from scopus.
#'     All papers and their abstracts that match your search using
#'     \code{\link{searchScopus}} are then used for the analysis.
#'     
#' Metadata of the papers allow for a more complete analysis. Metadata will be
#'     downloaded from Elsevier and it requires you to have an API key.
#'     To acquire your API key, go to \url{https://dev.elsevier.com/index.jsp}.
#' See the examples below on how to use the package.
#'
#' @section scicloud functions:
#' The scicloud functions are built in the following order:
#' \enumerate{
#'     \item \code{\link{createTextMatrixFromPDF}}
#'     \item \code{\link{ordinationCluster}}
#'     \item \code{\link{inspectScicloud}}
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
#'     Matthias Nachtmann, \email{matthias.nachtmann@@stud.leuphana.de},
#'     Jia Yan Ng, \email{jia.y.ng@@stud.leuphana.de},
#'     Johann Julius Beeck,
#'     Henrik von Wehrden, Prabesh Dhakal, \email{prabesh.dhakal@@stud.leuphana.de}
#' @examples
#' \dontrun{
#' 
#' ### The normal workflow of scicloud
#' myAPIKey <- "YOUR_API_KEY"
#' metaMatrix <- createTextMatrixFromPDF()
#'
#' # optional: read in previously run models
#' # metaMatrix <- readRDS(file= 'metaMatrix2019_______FILL_IN_HERE________')
#' 
#' # run the analysis, see ordinationCluster()
#' # for more arguments
#' scicloudAnalysis <- ordinationCluster(metaMatrix,
#'                            myAPIKey = myAPIKey)
#'
#' # inspect the analysis
#' scicloudSpecs <- inspectScicloud(scicloudAnalysis)
#' 
#' 
#' 
#' ### Alternative workflow: Generate a wordlist-csv to
#' ### revise the words before the analysis
#' 
#' createTextMatrixFromPDF()
#' scicloudAnalysis <- ordinationCluster(metaMatrix, 
#'                                       generateWordlist = TRUE,
#'                                       stemWords = TRUE)
#'                                       
#' # work with the wordlist and read the wordlist in again
#' scicloudAnalysis <- ordinationCluster(metaMatrix,
#'                                       keepWordsFile = "scicloudWordlist.csv",
#'                                       stemWords = TRUE)
#'
#'  scicloudSpecs <- inspectScicloud(scicloudAnalysis)
#'                                    }
#'
#' @docType package
#' @name scicloud
NULL
