#' @title processMetaDataMatrix
#'
#' @description The third function to the word analysis with scicloud. This function
#'     accepts a \code{metaMatrix} filled with metadata and constructs a tf-idf matrix.
#'
#' @author Jia Yan Ng, \email{jia.y.ng@@stud.leuphana.de}
#' @param metaMatrix metaMatrix created through \code{\link{getScopusMetaData}}
#'     or \code{\link{createTextMatrixFromPDF}}. Equations, symbols, all words
#'     in parentheses and all references are removed.
#' @param control a list of parameters used to determine preprocessing methods.
#'     Error will be thrown if language & stemWords are not defined.\cr
#'     \strong{language}: this defines the stopwords to be filtered. the default is
#'     "english". Look at \code{\link[tm]{stopwords}} for more information.\cr
#'     \strong{stemWords}: can be \code{TRUE} of \code{FALSE}. Transforms every word
#'     to its stem, so variants of the same words are treated equally. Look
#'     at \code{\link[tm]{stemDocument}} for more information.\cr
#'     \strong{saveToWd}: a logical parameter whether or not to save the output of the
#'     function to the working directory. This is especially useful for later
#'     analysis steps. The file can be read in by using \code{\link[base]{readRDS}}.\cr
#'     e.g. control = list(language = "SMART", stemWords = FALSE, saveToWd = TRUE)
#' @param ignoreWords a vector of words to be ignored.
#' @param keepWordsFile path to a .csv-file that specifies which words to keep
#'     during the analysis. Accepts 0/1 behind each word or takes the words
#'     as they are and disregards all other words of the analysis. A template
#'     for this can be generated with \code{generateWordlist} in
#'     \code{\link{ordinationCluster}} or \code{\link{calculateModels}}.
#' @seealso \itemize{
#'     \item \code{\link{createTextMatrixFromPDF}} and \code{\link{getScopusMetaData}}
#'     for the preceding steps
#'     \item \code{\link{calculateModels}} for the proceeding step
#'     }
#' @return returns a list object with \code{[["Tf_idf"]]} as the tf-idf document
#'     term matrix, \code{[["metaMatrix"]]} as passed to the function and
#' \code{[["wordList"]]} is the list of all words found in the papers.
#' @family scicloud functions
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
#' # 1) pull article metadata from scopus
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
#' 
#' }
#' @export


processMetaDataMatrix <- function(metaMatrix, control = list(),
                                  ignoreWords = c(),
                                  keepWordsFile){
  
  ######## Argument checks
  #* Establish a new 'ArgCheck' object
  Check <- ArgumentCheck::newArgCheck()

  
  ## control arguments (the list of arguments we intend the user to define)
  if(!is.list(control)) # if it's not a list, output error
    ArgumentCheck::addError(
      msg = "Please define the arguments of 'control' as a list!",
      argcheck = Check
      
    )else{ # if it's a list check if it has been specified correctly
      
      # Users have to define methods to be used in text preprocessing 
      if(is.null(control$language))
        ArgumentCheck::addError(
          msg = "Please define which language to be used in control list!",
          argcheck = Check
        )
      if(is.null(control$stemWords))
        ArgumentCheck::addError(
          msg = "Please define the stemWords in control list!",
          argcheck = Check
        )
      if(is.null(control$saveToWd))
        ArgumentCheck::addError(
          msg = "Please define the saveToWd in control list!",
          argcheck = Check
        )

    }
  
  ## metaMatrix warnings
  if (any(duplicated(metaMatrix[, "ID"]))){
    ArgumentCheck::addWarning(
      msg = "Entries do not have unique IDs, assigning new ones",
      argcheck = Check
    )
    metaMatrix[, "ID"] <- 1:length(metaMatrix[, "ID"])
  }
  
  ## keepWordsFile errors
  if(methods::hasArg(keepWordsFile)) # if the argument has been specified
    if(!file.exists(keepWordsFile)) # but the file path doesn't exist
      ArgumentCheck::addError(
        msg = "If you'd like to use a file to specify the words to keep, please provide a correct file path.",
        argcheck = Check
      )
  
  #* Return errors and warnings (if any)
  ArgumentCheck::finishArgCheck(Check)
  
  # define ordinationFunction
  if(is.null(control$ordinationFunction)) {control$ordinationFunction <- FALSE}
    
  
  
  # read the keepWordsFile in correctly as a csv or csv2
  if (methods::hasArg(keepWordsFile)) { # if the argument has been specified
    keepWords <- utils::read.csv(file = keepWordsFile) # it's a csv
    if (grepl(";", keepWords[1, ])) {
      #if a semicolon is in there...
      keepWords <- utils::read.csv2(file = keepWordsFile) #it's a csv2
    }
    if (nrow(keepWords) < 10) {
      stop(
        "Please provide the .csv-file with an appropriate number of words in the following format:\n\nwords;select\nabandon;1\nabbott;0\n...\n(',' instead of ';' is fine as well.)\n\nor just have a list of words you'd like to keep."
      )
    }
  }
  
  ### This part checks for results without abstract or fulltext and excludes them
  ### use keepNA = FALSE in nchar(), so that value 2 is returned when it is an empty string
  ### set threshold for string characters in Abstract & FullText > 100 for the row to be included
  index <-
    which(rowSums(nchar(metaMatrix, keepNA = FALSE)[, c("Abstract", "FullText")]) > 104)
  metaMatrix <- metaMatrix[index,]
  
  ### This part is to select the non-NA text from either Abstract/FullText columns for all rows 
  ### use case: either Abstratc or FullText contains non-NA value; not both exists at the same time 
  ### vectorText is a vector where each vector element is a non-NA text of each row
  subMatrix <- metaMatrix[,c("Abstract", "FullText")] 
  col_idx <- apply(subMatrix, 1, function(x){which(!is.na(x))}) # select non-NA column index
  all_idx <- c(1:nrow(subMatrix), col_idx) # combine row & col indexes 
  mat_idx <- matrix(all_idx, nrow = nrow(subMatrix), ncol = ncol(subMatrix), byrow = FALSE)
  vectorText <- subMatrix[mat_idx] # subsetting non-NA matrix cell 

  
  ### save all texts as corpora structure from tm library
  df <- data.frame(doc_id = metaMatrix[, "ID"], text = vectorText)
  docs_corpus <- tm::Corpus(tm::DataframeSource(df))
  
  ### This part is to preprocessing texts
  # generic function for matching regex pattern 
  regf <- tm::content_transformer(function(x, pattern) gsub(pattern, " ", x))
  xregf <- tm::content_transformer(function(x, pattern) gsub(pattern, "", x))
  rmb_regf <- tm::content_transformer(function(x, pattern) gsub(pattern, "\\1", x)) # replace with the first remembered pattern
  regf_perl <- tm::content_transformer(function(x, pattern) gsub(pattern, " ", x, perl = T))
  
  docs_corpus <- tm::tm_map(docs_corpus, regf, "\\s?(http)(s?)(://)([^\\.]*)[\\.|/](\\S*)") #remove website URL 
  docs_corpus <- tm::tm_map(docs_corpus, regf, "\\S+@\\S+") # remove email address 
  docs_corpus <- tm::tm_map(docs_corpus, regf, "\\S+\\.com") #remove URL (not start with http)
  docs_corpus <- tm::tm_map(docs_corpus, regf_perl, "\\(((?:[^()]++|(?R))*)\\)") # remove string with parenthesis (incl. nested parenthesis)
  docs_corpus <- tm::tm_map(docs_corpus, rmb_regf, "^(.*)References.*$") # remove all references 
#  docs_corpus <- tm::tm_map(docs_corpus, rmb_regf, "\\n(.*?)\\n(.*?)[\u00BD|\u00BC](.*?)\\n") # remove math equation (--> is this still necessary?)
  docs_corpus <- tm::tm_map(docs_corpus, regf, "[\r\n\f]+") # remove newline/carriage return
  docs_corpus <- tm::tm_map(docs_corpus, xregf, "- ") # undo hypen, eg. embar-rassment 
  docs_corpus <- tm::tm_map(docs_corpus, regf, "[^[:alpha:]]") # remove non-standard characters/punctuation
  docs_corpus <- tm::tm_map(docs_corpus, regf, "[[:blank:]]+") # remove all blank spaces/tab
  
  docs_corpus <- tm::tm_map(docs_corpus, tm::content_transformer(tolower))
  docs_corpus <- tm::tm_map(docs_corpus, tm::stripWhitespace) # remove extra whitespace 
  docs_corpus <- tm::tm_map(docs_corpus, tm::removeWords, tm::stopwords(control$language)) # remove stopwords
  

  # extract authors & publishers' name to be added to the list of ignoreWords 
  excludeWords <- paste0(paste(metaMatrix[,"Authors"], collapse = ", "), #concatenate authors & publishers as one string
                         ", ", paste(metaMatrix[,"Publisher"], collapse = ", ")) 
  excludeWords <- unique(unlist(strsplit(excludeWords, ", "))) # split the raw text into vector
  ignoreWords <- append(sort(unique(tolower(ignoreWords))), excludeWords)
  docs_corpus <- tm::tm_map(docs_corpus, tm::removeWords, ignoreWords)
  
  if(isTRUE(control$stemWords)){
    if(control$language == "SMART"){
      docs_corpus <- tm::tm_map(docs_corpus, tm::stemDocument) 
    }
    else{
      docs_corpus <- tm::tm_map(docs_corpus, tm::stemDocument, control$language)

    }
  }
  tf <- as.matrix(tm::TermDocumentMatrix(docs_corpus, control = list(removePunctuation = TRUE, stopwords = TRUE)))
  idf <- log(ncol(tf)/(1 + rowSums(tf != 0))) +1 # add 1 to avoid zero division, see "idf smooth"
  tf_idf <- crossprod(tf, diag(idf)) # tf(t,d)×idf(t,D)  
  colnames(tf_idf) <- rownames(tf)
  tf_idf <- tf_idf /sqrt(rowSums(tf_idf^2)) # normalize doc vector to address bias in long doc
  tf_idf <- tf_idf[, colSums(tf_idf) != 0] # omit non-representative words 
  
  # Apply keepWordsFile if specified
  if (methods::hasArg(keepWordsFile)) {
    # keep only the words denoted with "1" if two columns are given
    if (ncol(keepWords) == 2){
      keepWordsVector <- keepWords[keepWords[, 2] == 1, 1]
    }else{
      keepWordsVector = unlist(keepWords, use.names = FALSE)
    }
    unknownWords <- keepWordsVector[!keepWordsVector %in% colnames(tf_idf)]
    
    errorMessage <- function(cond) {
      message(paste0(length(unknownWords),
                     " words in keepWordsFile could not be found in ",
                     "the data. Please make sure no words were altered and you ",
                     "are using the same arguments as when generating the word ",
                     "list. The first ten words that could not be found are: "))
      cat(paste(unknownWords[1:10]))
      stop("Process stopped.",call. = FALSE)
    }
    tryCatch(tf_idf <- tf_idf[,keepWordsVector], error = errorMessage)
  }
  
  processedMetaDataMatrix <- list()
  processedMetaDataMatrix[[1]] <- tf_idf
  processedMetaDataMatrix[[2]] <- metaMatrix
  processedMetaDataMatrix[[3]] <- as.factor(sort(colnames(tf_idf)))
  
  names(processedMetaDataMatrix) <-
    c("Tf_Idf", "metaMatrix", "wordList")
  
  if (isTRUE(control$saveToWd)){
    save_data(processedMetaDataMatrix, "processedMetaDataMatrix", long_msg = !control$ordinationFunction)

  }
  return(processedMetaDataMatrix)
}
