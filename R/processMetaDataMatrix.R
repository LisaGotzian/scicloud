########################################################################
# Workflow to create a List of frequency tables.
# Accepts input of a two column matrix with title of text in col 1 and text in col2
# (see createTextMatrixFromPDF() or retrieveAbstractsfromScopus())
########################################################################

#' @title processMetaDataMatrix
#'
#' @description The second function to the word analysis with ginko. This
#'     function accepts a metaDataMatrix from the \code{\link[ginko]{getScopusMetaData}}
#'     or the basic processed pdf file data frame of \code{\link[ginko]{createTextMatrixFromPDF}}
#'     and restructures the data for later use.
#'
#' @author Matthias Nachtmann, \email{matthias.nachtmann@@stud.leuphana.de},
#'     Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de}
#' @param metaMatrix metaMatrix created through \code{\link[ginko]{getScopusMetaData}}
#'     or \code{\link[ginko]{createTextMatrixFromPDF}}.
#' @param language this defines the stopwords to be filtered. the default is
#'     "english". Look at \code{\link[tm]{stopwords}} for more information.
#' @param stemWords can be \code{TRUE} of \code{FALSE}. Transforms every word
#'     to its stem, so variants of the same words are treated equally. Look
#'     at \code{\link[tm]{stemDocument}} for more information.
#' @param ignoreWords a vector of words to be ignored.
#' @param useMoritz allows to specify the exact words that will be kept. Should
#'     only be used with caution. The list can be specified in \code{moritzFile}.
#' @param moritzFile the list specifying which exact words will be kept.
#'     \code{useMoritz} has to be set to \code{TRUE}
#' @param saveToWd a logical parameter whether or not to save the output of the
#'     function to the working directory. This is especially useful for later
#'     analysis steps and can be read in by using \code{\link[base]{saveRDS}}.
#' @param ordinationFunction placeholder
#' @param longMessages placeholder
#' @family ginko functions
#' @seealso \code{\link{createTextMatrixFromPDF}} for the preceding step,
#'     \code{\link{calculateModels}} for the proceeding step
#' @return returns a list with entry one being a binary matrix with all occuring
#'     words as columns and all papers as rows. The second list entry is an
#'     updated version of the input \code{metaMatrix}, excluding entries that
#'     have been filtered out throughout for various reasons.
#' @export
#' @examples \dontrun{
#' placeholder}
processMetaDataMatrix <- function(metaMatrix,
                                  language = "SMART",
                                  stemWords = TRUE,
                                  ignoreWords = c(),
                                  useMoritz = FALSE,
                                  moritzFile = "moritz.csv",
                                  saveToWd = TRUE,
                                  ordinationFunction = FALSE,
                                  longMessages = FALSE) {
  #Error handling
  if (any(duplicated(metaMatrix[, "ID"]))) {
    Sys.sleep(1)
    print("Warning: entries do not have unique IDs, assigning new ones")
    Sys.sleep(1)
    
    metaMatrix[, "ID"] <- 1:length(metaMatrix[, "ID"])
  }
  
  ### reading in moritz correctly
  if (all(c(useMoritz == TRUE,!moritzFile %in% list.files()))) {
    cat(
      "If you'd like to use a file to specify the words you'd like to keep, please have it in your working directory."
    )
  }
  
  # is it a csv or csv2?
  if (useMoritz == TRUE) {
    moritz <- utils::read.csv(file = moritzFile)
    if (grepl(";", moritz[1, ])) {
      #if a semicolon is in there...
      moritz <- utils::read.csv2(file = moritzFile) #it's a csv2
    }
    if (nrow(moritz) < 10) {
      cat(
        "Please provide the .csv-file with an appropriate number of words in the following format:\n\nwords;select\nabandon;1\nabbott;0\n...\n(',' instead of ';' is fine as well.)\n\nor just have a list of words you'd like to keep."
      )
    }
  }
  
  
  
  if (longMessages == TRUE) {
    Sys.sleep(1)
    cat("Checking for entries without abstract or fulltext\n")
    Sys.sleep(1)
    
    
    pb <-
      utils::txtProgressBar(min = 1,
                            max = nrow(metaMatrix),
                            style = 3)
  }
  
  #This part checks for results without abstract or fulltext and excludes them
  textlessIDs <- c()
  
  
  for (j in 1:nrow(metaMatrix)) {
    if ((nchar(metaMatrix[j, "FullText"], keepNA = FALSE) < 100) &
        (nchar(metaMatrix[j, "Abstract"], keepNA = FALSE) < 100)) {
      textlessIDs <- c(textlessIDs, metaMatrix[j, "ID"])
    }
    
    if (longMessages == TRUE) {
      utils::setTxtProgressBar(pb, j)
    }
    
  }
  
  
  rowsOfTextlessIDs <- c()
  if (length(textlessIDs) != 0) {
    for (k in 1:length(textlessIDs)) {
      intermediateTextlessIDs <-
        which(grepl(textlessIDs[k], metaMatrix[, "ID"]))
      rowsOfTextlessIDs <-
        c(rowsOfTextlessIDs, intermediateTextlessIDs)
    }
    
    metaMatrix <- metaMatrix[-rowsOfTextlessIDs,]
  }
  if (longMessages == TRUE) {
    close(pb)
  }
  
  
  
  
  
  
  
  
  
  
  if (longMessages == TRUE) {
    Sys.sleep(1)
    cat("Building a list of word frequencies per document\n")
    Sys.sleep(1)
    pb <-
      utils::txtProgressBar(min = 1,
                            max = nrow(metaMatrix),
                            style = 3)
  }
  
  wordTableList <- list()
  
  
  for (i in c(1:nrow(metaMatrix))) {
    try({
      doc <- c()
      
      if ((!is.na(metaMatrix[i, "FullText"])) &
          (length(metaMatrix[i, "FullText"]) == 1)) {
        doc <- metaMatrix[i, "FullText"]
      } else if ((!is.na(metaMatrix[i, "Abstract"])) &
                 (length(metaMatrix[i, "Abstract"]) == 1)) {
        doc <- metaMatrix[i, "Abstract"]
      } else{
        #print(paste0("Document nr. ", i, "appears to have neither a fulltext nor an abstract."))
        doc <- NA
      }
      
      textBody <- as.character(doc)
      textBody <-
        paste(textBody, collapse = " ")  #takes the vector and pastes it into a single element, seperated by a " "
      textBody <- gsub("[\r\n]+", " ", textBody)
      # textBody <- gsub("- ", "-", textBody)
      textBody <-
        gsub("[^ -abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-]+",
             " ",
             textBody)  ############## Added 10.10.2018: Replace every character with an empty space except for those in the squared brackets
      
      textBody <- gsub("- ", "", textBody)  # undo hyphenation
      textBody <-
        gsub('\f', " ", textBody) # seems to be definition for page brake or something eg brake from footer/header to text
      #textBody <- gsub('\', " ", textBody)
      #punctuations
      textBody <- gsub("[[:punct:]]", " ", textBody)
      textBody <- gsub("[0-9]", " ", textBody)
      textBody <-
        gsub("", "", textBody)    ############### Added 10.10.2018
      textBody <- gsub("    ", " ", textBody)
      textBody <- gsub("   ", " ", textBody)
      textBody <- gsub("  ", " ", textBody)
      
      textBody <-
        tolower(textBody) #at this point the data is a vector with a single element, containing the fulltext of the source
      
      #additional commands using the tm package, to process the words even further
      #these commands
      textBody <- gsub("\\|", " ", textBody)
      textBody <- gsub("@", " ", textBody)
      textBody <- gsub("/", " ", textBody)
      
      textBody <- tm::removeWords(textBody, tm::stopwords(language))
      
      #textBody <- tm::removeWords(textBody, "na") #a fix to avoid the conversion of NA into the statistical process
      #textBody <- tm::removeWords(textBody, ignoreWords) #removal of ignored words is transfered to a later stage to improve the performance
      
      if (stemWords == TRUE) {
        if (language == "SMART") {
          textBody <- tm::stemDocument(textBody)
        } else{
          #this command requires the loaded "SnowballC" package
          textBody <- tm::stemDocument(textBody, language = language)
        }
      }
      
      textBody <- tm::stripWhitespace(textBody)
      
      
      
      words <-
        strsplit(textBody, " ") #creating a vector with every singe word being one element
      wordTableList[[i]] <-
        data.frame(table(words)) #creates a dataframe, that has all the words in col1 and their frequency in col2
      
      
      names(wordTableList)[i] <-
        as.character(metaMatrix[i, "ID"]) #current workaround to avoid strange truncation behaviour, gives the list entry the actual name of the paper
      
      
      
      
      
      
      #Progress Bar
      if (longMessages == TRUE) {
        utils::setTxtProgressBar(pb, i)
      }
    })
  }
  if (longMessages == TRUE) {
    close(pb)
  }
  
  
  
  #make an x y matrix
  textobject <- c()#<-as.character(wordTableList[[2]][,1])
  
  if (longMessages == TRUE) {
    Sys.sleep(1)
    cat("Building a complete list of words\n")
    Sys.sleep(1)
    
    pb <-
      utils::txtProgressBar(min = 1,
                            max = length(wordTableList),
                            style = 3)
  }
  
  for (i in 1:length(wordTableList)) {
    textobject <- c(textobject, as.character(wordTableList[[i]][, 1]))
    if (longMessages == TRUE) {
      utils::setTxtProgressBar(pb, i)
    }
  }
  
  if (longMessages == TRUE) {
    close(pb)
  }
  
  completeListOfWords <- textobject
  
  
  
  
  # The following section adds all words that are supposed to be included like smaller than 16 or
  # the ones that are not authors and puts them into one dataframe called "wordssub".
  # After this, The final command will be "from the big metaMatrix, keep only the words that
  # are also in wordssub". Julius suggests calling this variable "wordleftovers".
  # The final line in approx. 475:  datasub1 <- datasub[datasub$words %in% wordssub,]
  #this line removes the words, that have been removed while creating the "wordsub" variable from the individual list
  
  
  #includes
  includes <- unique(as.factor(sort(completeListOfWords)))
  numberOfWords <-
    includes #How many words where there originally? For later use.
  
  # Apply moritz
  if (useMoritz == TRUE) {
    # keep only the words denoted with "1"
    if (ncol(moritz) == 2)
      moritzwords <- moritz[moritz[, 2] == 1, 1]
    
    moritzwords <- as.character(moritzwords) # for convenience
    
    wordssub <- includes[includes %in% moritzwords]
    
  } else{
    # if useMoritz = FALSE
    
    
    wordssub <-
      includes[which(nchar(as.character(includes)) < 16)] #only keep entries with less than 16 letters
    wordssub <-
      wordssub[which(nchar(as.character(wordssub)) > 2)] #only keep entries with more than 2 letters
    
    
    
    
    
    
    #This code excludes the publishers and the autors from the models to decrease bias
    allAuthors <- c()
    for (i in c(1:nrow(metaMatrix))) {
      if (!is.na(metaMatrix[i, "Authors"])) {
        authors <-
          as.character(unlist(strsplit(metaMatrix[i, "Authors"], ", ")))
        allAuthors <- c(allAuthors, authors)
      }
    }
    allAuthors <- sort(unique(tolower(allAuthors)))
    
    wordssub <-
      wordssub[!wordssub %in% allAuthors] #filtering the authors here and not through the "ignoreWords" variable, because of performance reasons
    
    if (stemWords == TRUE) {
      #this command requires the loaded "SnowballC" package
      stemmedAuthors <- tm::stemDocument(allAuthors)
      wordssub <- wordssub[!wordssub %in% stemmedAuthors]
    }
    
    
    
    allPublishers <- c()
    for (i in c(1:nrow(metaMatrix))) {
      if (!is.na(metaMatrix[i, "Publisher"])) {
        publisher <-
          as.character(unlist(strsplit(metaMatrix[i, "Publisher"], ", ")))
        allPublishers <- c(allPublishers, publisher)
      }
    }
    
    allPublishers <- sort(unique(tolower(allPublishers)))
    
    wordssub <-
      wordssub[!wordssub %in% allPublishers] #filtering the publishers here and not through the "ignoreWords" variable, because of performance reasons
    
    if (stemWords == TRUE) {
      #this command requires the loaded "SnowballC" package
      stemmedPublishers <- tm::stemDocument(allPublishers)
      wordssub <- wordssub[!wordssub %in% stemmedPublishers]
    }
    
    
    
    ignoreWords <- sort(unique(tolower(ignoreWords)))
    wordssub <-
      wordssub[!wordssub %in% ignoreWords] #filtering the ignored Words here and not through the "ignoreWords" variable, because of performance reasons
    
    if (stemWords == TRUE) {
      #this command requires the loaded "SnowballC" package
      stemmedIgnoredWords <- tm::stemDocument(ignoreWords)
      wordssub <- wordssub[!wordssub %in% stemmedIgnoredWords]
    }
  } # end the useMoritz = FALSE if loop
  
  
  
  
  
  if (longMessages == TRUE) {
    Sys.sleep(1)
    cat("Building a data frame with all documents as rows and all occuring words as columns\n")
    Sys.sleep(1)
    
    pb <-
      utils::txtProgressBar(min = 1,
                            max = length(wordTableList),
                            style = 3)
  }
  
  wordMatrix <- data.frame(words = wordssub)
  colnames(wordMatrix) <-
    "words" #at this point, wordMatrix is one single dataframe with all the words in one column called words
  #PapersNotReadIn <- NULL
  
  for (j in c(1:length(wordTableList))) {
    #if (is.null(wordTableList[[j]]$words) != TRUE){ #? keep it?
    # the line above throws out PDFs that couldn't be read like PDFs that are an image
    datasub <- wordTableList[[j]]
    datasub1 <-
      datasub[datasub$words %in% wordssub, ] #this line removes the words, that have been removed while creating the "wordsub" variable from the individual list
    wordMatrix <-
      merge(wordMatrix, datasub1, by = "words", all.x = T) # generates a dataframe with a row per word, and a col per paper including the amount of appearances
    if (longMessages == TRUE) {
      utils::setTxtProgressBar(pb, j)
    }
    #} #else {
    # if the full text is empty, delete the row from wordTableList b/c we will need it later
    #PapersNotReadIn <- c(j, PapersNotReadIn)
    #}
  }
  if (longMessages == TRUE) {
    close(pb)
  }
  
  # for (i in 1:length(PapersNotReadIn)){
  # wordTableList[[PapersNotReadIn[i]]] <- NULL # this now deletes the identified papers from wordTableList
  # }
  
  wordMatrix2 <-
    wordMatrix[, 2:(length(wordMatrix))] #the first col are the words, followed by the individual papers
  rownames(wordMatrix2) <-
    wordssub #correcting the shift in numbers, and assigning the words to the corresponding rowname
  
  
  #write.table(names(which(apply(wordMatrix2,1,sum,na.rm=T)>30)),"words.txt")
  #wordMatrix2$words <- NULL
  #dim(wordMatrix2)
  for (k in 1:length(wordMatrix2))
    wordMatrix2[is.na(wordMatrix2[, k]), k] <-
    0 #change all NA in dataframe to 0
  colnames(wordMatrix2) <-
    names(wordTableList)[c(1:length(wordTableList))] #set title as colname
  
  
  #apply(wordMatrix2,1,sum)>1 #this checks, whether a word appears in more than one paper
  #moritz<-wordMatrix2[apply(ifelse(wordMatrix2>0,1,0),1,sum)>10,]
  #write.table(rownames(moritz),"moritz.txt")
  wordMatrix3 <-
    wordMatrix2[apply(ifelse(wordMatrix2 > 0, 1, 0), 1, sum) > (length(wordTableList) /
                                                                  100 * 5), ] #keep only words that appear in more than 5% of the papers
  
  #rownames(wordMatrix3);dim(wordMatrix3)
  wordMatrix3Binary <-
    ifelse(wordMatrix3 > 0, 1, 0) #transform wordMatrix3 to binary matrix
  wordMatrix3BinaryTransposed <- t (wordMatrix3Binary)
  #dim(wordMatrix3BinaryTransposed)
  #colnames(wordMatrix3BinaryTransposed)
  
  #dim(wordMatrix3BinaryTransposed)
  #ordination
  #which(apply(wordMatrix3BinaryTransposed,1,sum)==0)
  
  #A list is necessary here, because some papers might be excluded be the next line of code. In order to match the paper
  # names with the clusters, both need the same length. this is why we export the orignial dataset without the excluded ones
  processedData <- list()
  processedData[[1]] <-
    wordMatrix3BinaryTransposed[apply(wordMatrix3BinaryTransposed, 1, sum) >=
                                  1, ] #excludes all rows, with a sum of 0. (papers with no matching words)
  row.names(processedData[[1]]) <-
    metaMatrix[, "FileName"] #rename the rows to use the adjacency matrix
  
  excludedPapers <-
    rownames(wordMatrix3BinaryTransposed[apply(wordMatrix3BinaryTransposed, 1, sum) <
                                           1, ])
  #wordm3t<-t(wordMatrix3BinaryTransposed[1:c(dim(wordMatrix3BinaryTransposed)[1]),])
  
  processedData[[2]] <- metaMatrix
  
  
  if (!is.null(excludedPapers)) {
    excludedRows <- c()
    for (l in 1:length(excludedPapers)) {
      intermediateExcludedRows <-
        which(grepl(excludedPapers[l], metaMatrix[, "ID"]))
      excludedRows <- c(excludedRows, intermediateExcludedRows)
    }
    
    processedData[[2]] <- metaMatrix[-excludedRows,]
  }
  
  processedData[[3]] <- numberOfWords # for later use
  names(processedData) <-
    c("BinaryWordList", "MetaMatrix", "numberOfWords")
  
  if (longMessages == TRUE) {
    cat("Done\n\n")
  }
  
  #beep(3)
  
  if (saveToWd == TRUE) {
    processedDataFile <-
      paste0("processedData", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"))
    saveRDS(processedData, file = processedDataFile)
    
    if (ordinationFunction == FALSE) {
      cat(
        paste0(
          "\nThe processed Data is now in your global environment. It is also saved as a file in your working directory. If you work with the same data again, you can skip this step in future analysis by reading in the file:\nprocessedMetaMatrix <- readRDS(file= '",
          processedDataFile,
          "')\n\n"
        )
      )
    } else {
      cat(
        paste0(
          "Processed Data saved. You can read it in using:\nprocessedMetaMatrix <- readRDS(file= '",
          processedDataFile,
          "')\n###################################################################################################\n\n"
        )
      )
      
    }
  }
  
  return(processedData)
}