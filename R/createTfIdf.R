# title createTfIdf
#
# description The third function to the word analysis with scicloud. This function
#     accepts a \code{metaMatrix} filled with metadata and constructs a tf-idf matrix.
#
# author Jia Yan Ng, \email{jia.y.ng@@stud.leuphana.de}
# param metaMatrix metaMatrix created through \code{\link{getScopusMetaData}}
#     or \code{\link{createTextMatrixFromPDF}}. Equations, symbols, all words
#     in parentheses and all references are removed.
# param control a list of parameters used to determine preprocessing methods.
#     Error will be thrown if language & stemWords are not defined.\cr
#     \strong{language}: this defines the stopwords to be filtered. the default is
#     "SMART". Look at \code{\link[tm]{stopwords}} for more information.\cr
#     \strong{stemWords}: can be \code{TRUE} of \code{FALSE}. Transforms every word
#     to its stem, so variants of the same words are treated equally. Look
#     at \code{\link[tm]{stemDocument}} for more information.\cr
#     \strong{saveToWd}: a logical parameter whether or not to save the output of the
#     function to the working directory. This is especially useful for later
#     analysis steps. The file can be read in by using \code{\link[base]{readRDS}}.\cr
#     e.g. control = list(language = "SMART", stemWords = FALSE, saveToWd = TRUE)
# param ignoreWords a vector of words to be ignored.
# param keepWordsFile path to a .csv-file that specifies which words to keep
#     during the analysis. Accepts 0/1 behind each word or takes the words
#     as they are and disregards all other words of the analysis. A template
#     for this can be generated with \code{generateWordlist} in
#     \code{\link{ordinationCluster}} or \code{\link{calculateModels}}.
# seealso \itemize{
#     \item \code{\link{createTextMatrixFromPDF}} and \code{\link{getScopusMetaData}}
#     for the preceding steps
#     \item \code{\link{calculateModels}} for the proceeding step
#     }
# return returns a list object with \code{[["Tf_idf"]]} as the tf-idf document
#     term matrix, \code{[["metaMatrix"]]} as passed to the function and
# \code{[["wordList"]]} is the list of all words found in the papers.
# family scicloud functions

createTfIdf <- function(metaMatrix, control = list(),
                                  ignoreWords = c(),
                                  keepWordsFile = NA){
  
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
  if(!is.na(keepWordsFile)){
    if(!file.exists(keepWordsFile)) # but the file path doesn't exist
      ArgumentCheck::addError(
        msg = "If you'd like to use a file to specify the words to keep, please provide a correct file path.",
        argcheck = Check
      )
    else{
      # read the keepWordsFile in correctly as a csv or csv2
      csvCheck <- readLines(keepWordsFile, n = 1)
      if (grepl(";", csvCheck)) 
        keepWords <- utils::read.csv2(file = keepWordsFile) 
      else 
        keepWords <- utils::read.csv(file = keepWordsFile)
      if (nrow(keepWords) < 10) {
          ArgumentCheck::addError(
            msg = "Please provide a list of word that is not less than 10 words. ",
            argcheck = Check
          )
      }
    }
  } 
  
  #* Return errors and warnings (if any)
  ArgumentCheck::finishArgCheck(Check)
  
  # define ordinationFunction
  if(is.null(control$long_msg)) {control$long_msg <- FALSE}
  
  
  ### This part checks for results without abstract or fulltext and excludes them
  ### use keepNA = FALSE in nchar(), so that value 2 is returned when it is an empty string
  ### set threshold for string characters in Abstract & FullText > 100 for the row to be included
  index <-
    which(rowSums(nchar(metaMatrix, keepNA = FALSE)[, c("Abstract", "FullText")]) > 104)
  metaMatrix <- metaMatrix[index,]
  
  ### This part is to select the non-NA text from either Abstract or FullText columns for all rows 
  ### use case: either Abstract or FullText contains non-NA value; not both exists at the same time 
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
  if (!is.na(keepWordsFile)) {
    # keep only the words denoted with "1" if two columns are given
    if (ncol(keepWords) == 2){
      keepWordsVector <- as.character(keepWords[,1][as.logical(keepWords[,2])])
    }else{
      keepWordsVector = as.character(unlist(keepWords, use.names = FALSE))
    }
    # remove empty trailing spaces in the characters 
    keepWordsVector <- sub("\\s+", "", keepWordsVector)
    
    # find the unmatched words from the .csv with the word list in the analysis 
    unmatched <- setdiff(keepWordsVector, colnames(tf_idf))
    # Words to be included in the analysis 
    updateWord <- setdiff(keepWordsVector, unmatched)
    if(length(unmatched) != 0){
      utils::write.table(unmatched, file="./unmatched.csv", 
                         row.names=FALSE, col.names=FALSE, na = "", sep =",")
      cat("\nThere are word(s) in the .csv provided that are not found in the analysis\nCheck unmatched.csv saved in the current working directory in details\n")
      
      if(length(updateWord) != 0){
        tf_idf <- tf_idf[,updateWord]  
      }
      else{
        cat("All words in CSV file does not match with words in the analysis.\n
                All orginal words found in the analysis are kept in TF-IDF.\n 
                Please check and update your CSV file.")
      }
    }
  }
  
  scipusList <- list()
  scipusList$Tf_Idf <- tf_idf
  scipusList$wordList <- as.factor(sort(colnames(tf_idf)))
  
  if (isTRUE(control$saveToWd)){
    save_data(scipusList, "metaData", long_msg = control$long_msg)
    
  }
  return(scipusList)
}
