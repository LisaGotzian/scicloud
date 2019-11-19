#############################################################################
# input: metaMatrix with a control list (to determine pre-processing methods)
# output: a tf-idf matrix 
#############################################################################

#' @title processMetaDataMatrix
#'
#' @description Constructs a tf-idf matrix. 
#'
#' @author Jia Yan Ng, \email{jia.y.ng@@stud.leuphana.de}
#' @param metaMatrix metaMatrix created through \code{\link[ginko]{getScopusMetaData}}
#'     or \code{\link[ginko]{createTextMatrixFromPDF}}.
#' @param control a list of parameters used to determine pre-processing methods. 
#' Error will be thrown if language & stemWords are not defined. 
#' e.g. control = list(language = "SMART, stemWords = FALSE)
#' @family ginko functions

processMetaDataMatrix <- function(metaMatrix, control = list()){
  
  # Users have to define methods to be used in text pre-processing 
  if(is.null(control$language))
    stop("Please define which language to be used in control list!")
  if(is.null(control$stemWords))
    stop("Please define the stemWords in control list!")
  
  stopifnot(is.list(control))
  
  #Error handling
  if (any(duplicated(metaMatrix[, "ID"]))) {
    Sys.sleep(1)
    print("Warning: entries do not have unique IDs, assigning new ones")
    Sys.sleep(1)
    
    metaMatrix[, "ID"] <- 1:length(metaMatrix[, "ID"])
  }
  
  ### This part checks for results without abstract or fulltext and excludes them
  ### use keepNA = FALSE in nchar(), so that value 2 is returned when it is an empty string
  ### set threshold for string characters in Abstract & FullText > 100 for the row to be included  
  index <- which(rowSums(nchar(metaMatrix, keepNA = FALSE)[,c("Abstract", "FullText")]) > 104)
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
  df <- data.frame(doc_id = metaMatrix[,"ID"], text = vectorText)
  docs_corpus <- tm::Corpus(tm::DataframeSource(df))
  
  ### This part is to pre-processing texts
  # generic function for matching regex pattern 
  regf <- tm::content_transformer(function(x, pattern) gsub(pattern, " ", x))
  xregf <- tm::content_transformer(function(x, pattern) gsub(pattern, "", x))
  
  docs_corpus <- tm::tm_map(docs_corpus, regf, "\\s?(http)(s?)(://)([^\\.]*)[\\.|/](\\S*)") #remove website URL 
  docs_corpus <- tm::tm_map(docs_corpus, regf, "\\S+@\\S+") # remove email address 
  docs_corpus <- tm::tm_map(docs_corpus, regf, "\\S+\\.com") #remove URL (not start with http)
  docs_corpus <- tm::tm_map(docs_corpus, regf, "[\r\n\f]+") # remove newline/carriage return
  docs_corpus <- tm::tm_map(docs_corpus, regf, "[[:digit:]]+") # remove digits
  docs_corpus <- tm::tm_map(docs_corpus, xregf, "- ") # undo hypen, eg. embar-rassment 
  docs_corpus <- tm::tm_map(docs_corpus, regf, "[[:cntrl:]]+") # remove other control characters 
  docs_corpus <- tm::tm_map(docs_corpus, regf, "[[:punct:]]+") # remove punctuaton 
  docs_corpus <- tm::tm_map(docs_corpus, regf, "[[:blank:]]+") # remove all blank spaces/tab

  docs_corpus <- tm::tm_map(docs_corpus, tm::content_transformer(tolower))
  docs_corpus <- tm::tm_map(docs_corpus, tm::stripWhitespace) # remove extra whitespace 
  docs_corpus <- tm::tm_map(docs_corpus, tm::removeWords, tm::stopwords(control$language)) # remove stopwords
  
  if(isTRUE(control$stemWords)){
    if(control$language == "SMART"){
      docs_corpus <- tm::tm_map(docs_corpus, tm::stemDocument) 
    }
    else{
      docs_corpus <- tm::tm_map(docs_corpus, tm::stemDocument, control$language)
    }
  }
  tf <- as.matrix(tm::TermDocumentMatrix(docs_corpus, control = list(removePunctuation = TRUE, stopwords = TRUE)))
  idf <- log(ncol(tf)/(1 + rowSums(tf != 0))) # add 1 to avoid zero division
  tf_idf <- crossprod(tf, diag(idf)) # tf(t,d)×idf(t,D)  
  colnames(tf_idf) <- rownames(tf)
  tf_idf <- tf_idf /sqrt(rowSums(tf_idf^2)) # normalize doc vector to address bias in long doc
  tf_idf <- tf_idf[, colSums(tf_idf) != 0] # omit non-representative words 
  
  return(tf_idf)
}
