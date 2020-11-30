#' @title Create a scicloud list
#' @description The first function to be called to perform the analysis with
#'   scicloud. It outputs a list of 3 components: metaMatrix, Tf_Idf and
#'   wordList for further use with \code{\link{runAnalysis}}. \cr
#'   The function takes all scientific papers as PDF files from the
#'   "PDFs" folder in your working directory or any other specified directory to
#'   create a metaMatrix. It then further pre-processes the text (eg. by stemming
#'   words with stemWords) and outputs a tf-idf matrix. As a last step, it fetches the papers' metadata
#'   from Scopus for which you'll need an Elsevier API key
#'   (\url{https://dev.elsevier.com/index.jsp}). \cr
#'   You have the option to limit the words to be used in the analysis with the
#'   argument 'keepWordsFile'.
#'
#' @author Creator of the scicloud workflow: Henrik von Wehrden,
#'   \email{henrik.von_wehrden@@leuphana.de} \cr \cr
#'   Code by: Jia Yan Ng, \email{Jia.Y.Ng@@stud.leuphana.de},
#'   Johann Julius Beeck, \email{johann.j.beeck@@stud.leuphana.de},
#'   Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de},
#'   Prabesh Dhakal, \email{prabesh.dhakal@@stud.leuphana.de} \cr \cr
#'   First version of scicloud: Matthias Nachtmann,
#'   \email{matthias.nachtmann@@stud.leuphana.de}
#'
#' @param directory per default, the PDFs are expected to be in a folder named
#'   "PDFs", can be changed ad. lib.
#' @param myAPIKey your private API key for communicating with the Scopus API.
#'   You can request one at \url{https://dev.elsevier.com/index.jsp}.
#' @param language this defines the language of the stopwords to be filtered.
#'   The default is "SMART". Look at \code{\link[tm]{stopwords}} for more
#'   information.\cr
#' @param stemWords logical variable which is passed to processMetaDataMatrix.
#' @param saveToWd  a logical parameter whether or not to save the output of the
#'   function to the working directory. This is especially useful for later
#'   analysis steps. The file can be read in by using
#'   \code{\link[base]{readRDS}}.
#' @param ignoreWords a vector of words to be ignored which is passed to
#'   processMetaDataMatrix.
#' @param keepWordsFile path to a .csv-file that specifies which words to keep
#'   for the analysis. Accepts 0/1 behind each word or takes the words as
#'   they are and disregards all other words for the analysis. If no word list is
#'   provided, all words are used.\cr
#'   You can generate a list with all words used in the current analysis by
#'   setting \code{generateWordlist} to \code{TRUE}. If you intend to use this
#'   option, delete all words you don't need and re-run the function with the
#'   updated word list by specifying \code{keepWordsFile}.
#' @param generateWordlist logical, if set to \code{TRUE}, it generates a
#'   wordlist in your working directory. You can now add a 0/1 behind each word
#'   or delete rows you don't consider important to the analysis.
#' @return Returns a list with the following components:
#'   \itemize{ \item \code{Tf_Idf}: the tf-idf document term matrix.
#'   \item \code{wordList}: a list of all words that have been used in the
#'   analysis.
#'   \item \code{metaMatrix}: a matrix with 21 columns that contains
#'   information (DOI, Year, Authors, etc.) and each pdf's full text
#'   that has been pre-processed and filtered.
#'   Information (Title, Abstract, Journal, etc.) are retrieved through the 
#'   Scopus API. Please note that without a proper API and a valid connection to
#'   Scopus within a recognized network these information will not be retrieved
#'   successfully }
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
#' scicloudAnalysis <- runAnalysis(scicloudList = scicloudList, numberOfClusters = 4)
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
                           keepWordsFile = NA,
                           generateWordlist = FALSE) {
  
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
      keepWordsFile = keepWordsFile,
      generateWordlist = generateWordlist
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
