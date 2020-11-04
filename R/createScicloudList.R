#' @title Create a scicloud list
#' @description The first function to be called to perform analysis with scicloud. 
#' It outputs a list of 3 components: metaMatrix, Tf_Idf and wordList. 
#' The function takes all scientific papers as PDF files from the "PDFs" folder 
#' in your working directory or any other specified directory to create a metaMatrix. 
#' Then it further pre-processes the full text retrieved with the method specified 
#' (eg. stemWords, language) in order to output a tf-idf matrix. 
#' All the words that appear in the PDFs and have been included in the analysis 
#' (could be specified with the argument 'keepWordsFile') is saved as the wordList. 
#' 
#' @author Jia Yan Ng, \email{Jia.Y.Ng@@stud.leuphana.de}, 
#' Johann Julius Beeck, \email{johann.j.beeck@@stud.leuphana.de}
#' 
#' @param directory per default, the PDFs are expected to be in a folder named
#'     "PDFs", can be changed ad. lib.
#' @param myAPIKey your private Elsevier API key for communicating with the
#'     API. You can request one at \url{https://dev.elsevier.com/index.jsp}.
#' @param language this defines the stopwords to be filtered. the default is
#'     "SMART". Look at \code{\link[tm]{stopwords}} for more information.\cr        
#' @param stemWords logical variable which is passed to processMetaDataMatrix.
#' @param saveToWd  a logical parameter whether or not to save the output of the
#'     function to the working directory. This is especially useful for later
#'     analysis steps. The file can be read in by using \code{\link[base]{readRDS}}.
#' @param ignoreWords a vector of words to be ignored which is passed to processMetaDataMatrix.
#' @param keepWordsFile path to a .csv-file that specifies which words to keep
#'     during the analysis. Accepts 0/1 behind each word or takes the words
#'     as they are and disregards all other words of the analysis.
#' @return Returns a list with the following components:
#' \itemize{
#'     \item \code{Tf_Idf}: the tf-idf document term matrix.
#'     \item \code{wordList}: a list of all words that have been used in the analysis.
#'     \item \code{metaMatrix}: a matrix with 21 columns that contains information (DOI, Year, Authors, etc.) \cr
#'     and its FullText from each pdf that has been pre-processed by method \cr
#'     defined in the arguments. Information (Title, Abstract, Journal, etc.) are retrieved
#'     through the API of Scopus. Please note that without a proper API and a valid connection 
#'     to Scopus within a recognized network, these information will not be retrieved successfully 
#'     }
#' @family scicloud functions
#' @examples
#' \dontrun{
#' 
#' ### Workflow of performing analysis using scicloud
#' myAPIKey <- "YOUR_API_KEY"
#' # retrieving data from PDFs and Scorpus website using API  
#' scicloudList <- createScicloudList(myAPIKey = myAPIKey)
#'
#' # Run the analysis with a specified no. of cluster
#' scicloudAnalysis <- runAnalysis(scipusList = scipusList, numberOfClusters = 4)
#'                            
#' # Generate a summary of the analysis 
#' scicloudSpecs <- inspectScicloud(scicloudAnalysis)
#' }
#' @export
createScicloudList <- function(directory = file.path(".", "PDFs"),
                           myAPIKey = NA,
                           language = "SMART",
                           stemWords = TRUE,
                           saveToWd = FALSE,
                           ignoreWords = c(),
                           keepWordsFile = NA) {
  
  # Argument Checks
  Check <- ArgumentCheck::newArgCheck()
  
  # Check API Key
  if(is.na(myAPIKey)){
    ArgumentCheck::addError(
      msg = "Please input your API key from Elsevier!",
      argcheck = Check
    )
  }
  ArgumentCheck::finishArgCheck(Check)
  
  # create matrix by reading PDFs 
  metaMatrix <- createTextMatrixFromPDF(directory)
  
  # preprocessing the corpus from the PDFs to create Tf-Idf and WordList
  scicloudList <-
    createTfIdf(
      metaMatrix = metaMatrix,
      control = list(
        language = language, 
        stemWords = stemWords),
      ignoreWords = ignoreWords,
      keepWordsFile = keepWordsFile
    )
  
  # update info in the metaMatrix from Scopus using API
  scicloudList$metaMatrix <-
    getScopusMetaData(
      metaMatrix = metaMatrix, 
      myAPIKey = myAPIKey)
  
  if (saveToWd){
    save_data(scicloudList, "scicloudList")
  }
  return(scicloudList)
}
