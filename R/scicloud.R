########################################################################
# Package help
########################################################################
#' A Package For Word Analysis Of Scientific Papers
#'
#' The scicloud package provides a word analysis for scientific papers which
#' saved as PDFs. It clusters publication communities based on the words used in
#' the papers. To identify the words relevant to the communities, it uses an indicator
#' species analysis. In the end, it produces a wordcloud and visualizes the
#' communities by year and number of citations with barplots.
#' The package also offers a network analysis approach for which
#' Gephi or a different graphing software is required.
#'
#' @section Prerequisites:
#' Locate all the PDFs that you want to analyze in a folder called "PDFs"
#' in your working directory, otherwise please provide a specific path to the
#' directory of your PDFs when calling \code{\link{createScicloudList}} function.
#'
#'     An API key from Elsevier is also required in order to retrieve additional
#'     information needed from Scopus. All papers and their abstracts that match
#'     in the search are then used for the analysis.
#'
#'     To acquire your API key, go to \url{https://dev.elsevier.com/}.
#'     In addition, make sure that you are connected to your institution's
#'     network (e.g. via VPN) when executing \code{\link{createScicloudList}} function.
#'
#' Refer to the examples below to know how to use the package.
#'
#' @section Workflow of running analysis with scicloud:
#' The scicloud functions should be executed in the following order:
#' \enumerate{
#'     \item \code{\link{createScicloudList}}
#'     \item \code{\link{runAnalysis}}
#'     \item \code{\link{inspectScicloud}}
#' }
#' @section Managing RDS files:
#' The result of \code{\link{createScicloudList}} and \code{\link{runAnalysis}}
#' can be saved as a RDS file in your working directory by setting the parameter "saveToWd"
#' to TRUE when executing these functions. To delete the RDS files in the working directory,
#' simply execute \code{\link{deleteRDS}} and choose one of the delete options. Check the
#' function's documentation for more details.
#'
#' @author Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de},
#'     Matthias Nachtmann, \email{matthias.nachtmann@@stud.leuphana.de},
#'     Jia Yan Ng, \email{jia.y.ng@@stud.leuphana.de},
#'     Johann Julius Beeck, \email{johann.j.beeck@@stud.leuphana.de},
#'     Henrik von Wehrden, Prabesh Dhakal, \email{prabesh.dhakal@@stud.leuphana.de}
#'
#' @examples
#' \dontrun{
#'
#' ### Workflow of performing analysis using scicloud
#' myAPIKey <- "YOUR_API_KEY"
#'
#' # retrieving data from PDFs and Scorpus website using API
#' scicloudList <- createScicloudList(myAPIKey = myAPIKey)
#'
#' # Run the analysis with a specified no. of cluster
#' scicloudAnalysis <- runAnalysis(scipusList = scipusList, numberOfClusters = 4)
#'
#' # Generate a summary of the analysis
#' scicloudSpecs <- inspectScicloud(scicloudAnalysis)
#' }
#' @docType package
#' @name scicloud
NULL
