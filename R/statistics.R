

################################
# 3.10. (Ng Jia Yan) *While Loop in function xy*
#           - changed...

########################################################################
# Package loading message
########################################################################

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to ginko. Have a folder 'PDFs' within your working directory that contains all scientific papers you intend to use. Run ?ginko to get started."
  )
}

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

########################################################################
# Workflow for folders containing PDF files
# generates a matrix of name in col1 and text in row2 like the scopus function
########################################################################

#' @title createTextMatrixFromPDF
#'
#' @description First function of the word analysis with ginko. It takes all
#'     scientific papers as PDF files from a "PDFs" folder you'll have to
#'     create. It then creates a DocumentTerm matrix
#'     similar to \code{\link[ginko]{searchScopus}} for further processing.
#' @param filepath per default, the PDFs are expected to be in a folder named
#'     "PDFs", can be changed ad. lib.
#' @param saveToWd a logical parameter whether or not to save the output of the
#'     function to the working directory. This is especially useful for later
#'     analysis steps and can be read in by using \code{\link[base]{saveRDS}}.
#' @family ginko functions
#' @seealso \code{\link{ordinationCluster}} for the next step in ginko,
#'     a wrapper of all steps or \code{\link{processMetaDataMatrix}} for the
#'     next step if you intend to run it step by step,
#'     \code{\link{getScopusMetaData}} to fill in paper metadata (needed for
#'     future plots) from scopus
#'
#' @author Matthias Nachtmann, \email{matthias.nachtmann@@stud.leuphana.de},
#'     Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de}
#' @return A data frame containing the file name and full text of the pdf, as
#'     well as scraped DOI numbers from the text and some placeholder columns for later.
#' @examples
#' \dontrun{
#' metaMatrix <- createTextMatrixFromPDF(saveToWd = TRUE)
#'
#' myAPIKey <- "YOUR API KEY"
#' # go to https://dev.elsevier.com/user/login to get yours
#'
#' GinkoAnalysis <- ordinationCluster(metaMatrix, myAPIKey = myAPIKey, stemWords = FALSE,
#'     longMessages = TRUE, saveToWd = TRUE, method = "hclust")
#' GinkoSpecs <- inspectGinko(modeledData = GinkoAnalysis)
#' }
#' @export
createTextMatrixFromPDF <-
  function(filepath = "./PDFs/",
           saveToWd = TRUE) {
    PDFFiles <- paste0(filepath, list.files(path = filepath))
    PDFcontent <- matrix(NA, nrow = length(PDFFiles), ncol = 20)
    colnames(PDFcontent) <-
      c(
        "Title",
        "Year",
        "Month",
        "Day",
        "Authors",
        "Journal",
        "Volume",
        "Issue",
        "Pages",
        "CitedBy",
        "CitationPerYear",
        "DOI",
        "Scopus-ID",
        "Publisher",
        "Affiliation",
        "Affiliation-City",
        "Affiliation-Country",
        "FileName",
        "Abstract",
        "FullText"
      )
    
    
    start = 1
    end = length(PDFFiles)
    pb <- utils::txtProgressBar(min = start, max = end, style = 3)
    
    for (i in c(start:end)) {
      intermediateResultFileName <- PDFFiles[i]
      
      intermediateResultText <- tryCatch({
        reader <-
          tm::readPDF(control = list(text = "-raw")) # using the default
        suppressWarnings(reader(
          elem = list(uri = intermediateResultFileName),
          language = "en",
          id = "id1"
        ))
      },
      error = function(cond) {
        message(
          paste(
            "PDF File caused an error while retrieving the full text",
            intermediateResultFileName
          )
        )
        message("Here's the original error message:")
        message(cond)
        # Choose a return value in case of error
        return(NA)
      },
      warning = function(cond) {
        message(paste("PDF File caused a warning:", intermediateResultFileName))
        message("Here's the original warning message:")
        message(cond)
        # Choose a return value in case of warning
        return(NULL)
      })
      
      intermediateResultText <- as.character(intermediateResultText)
      intermediateResultText <-
        paste(intermediateResultText, collapse = " ")  #takes the vector and pastes it into a single element, seperated by a " "
      
      # get rid of the path for the filename
      PDFcontent[i, "FileName"] <-
        if (length(intermediateResultFileName) > 0) {
          gsub("./PDFs/", " ", intermediateResultFileName)
        } else{
          NA
        }
      
      PDFcontent[i, "FullText"] <-
        if (length(intermediateResultText) > 0) {
          intermediateResultText
        } else{
          NA
        }
      
      #Progress Bar
      utils::setTxtProgressBar(pb, i)
    }
    
    PDFcontent <-
      cbind(PDFcontent, "ID" = 1:nrow(PDFcontent)) #assiging a unique id to avoid collisions along the way
    
    DOIpattern <-
      '\\b(10[.][0-9]{4,}(?:[.][0-9]+)*/(?:(?!["&\'<>])[[:graph:]])+)\\b'
    DOInumbers <-
      stringr::str_extract(PDFcontent[, "FullText"], DOIpattern)
    PDFcontent[, "DOI"] <- DOInumbers
    
    #this filters double DOI entries in the PDFcontent The perfect similarity of entries has a huge effect on the models later on in the process
    PDFcontent <-
      subset(PDFcontent,!duplicated(PDFcontent[, "DOI"], incomparables = NA))
    #PDFcontent <- subset(PDFcontent, !is.na(PDFcontent[, "FullText"])) #doesn't work b/c it finds something
    
    close(pb)
    
    if (saveToWd == TRUE) {
      MetaMatrixFile <-
        paste0("metaMatrix", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"))
      saveRDS(PDFcontent, file = MetaMatrixFile)
      
      cat(
        paste0(
          "\nThe metaMatrix is now in your global environment. It is also saved as a file in your working directory. If you work with the same data again, you can skip this step in future analysis by reading in the file:\nmetaMatrix <- readRDS(file= '",
          MetaMatrixFile,
          "')\n\n"
        )
      )
    }
    
    return(PDFcontent)
  }






















# ########################################################################
# Essential Wrapper Function
# ########################################################################


#' @title ordinationCluster
#'
#' @description The essential wrapper function that runs all steps of the word
#'     analysis.
#'
#' @author Matthias Nachtmann, \email{matthias.nachtmann@@stud.leuphana.de},
#'     Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de}
#' @param metaMatrix placeholder
#' @param language placeholder
#' @param numberOfClusters placeholder
#' @param minWordsPerCluster placeholder
#' @param maxWordsPerCluster placeholder
#' @param stemWords placeholder
#' @param ignoreWords placeholder
#' @param exactPosition placeholder
#' @param p placeholder
#' @param dendrogram placeholder
#' @param dendroLabels placeholder
#' @param myAPIKey placeholder
#' @param method takes "network", "hclust" or "both" as a method
#' @param saveToWd a logical parameter whether or not to save the output of
#'     the function to the working directory. This is especially useful for
#'     later analysis steps and can be read in by using \code{\link[base]{saveRDS}}.
#' @param longMessages by default \code{FALSE} to keep the output short.
#' @family ginko functions
#' @return placeholder
#' @export
#' @examples \dontrun{
#' placeholder}
ordinationCluster <- function(metaMatrix,
                              language = "SMART",
                              numberOfClusters = NA,
                              minWordsPerCluster = 5,
                              maxWordsPerCluster = 10,
                              stemWords = TRUE,
                              ignoreWords = c(),
                              myAPIKey = NA,
                              p = 0.05,
                              exactPosition = FALSE,
                              dendrogram = TRUE,
                              dendroLabels = "truncated",
                              saveToWd = TRUE,
                              method = "hclust",
                              longMessages = FALSE) {
  if (sum(c(method == "hclust",
            method == "network",
            method == "both")) != 1) {
    cat(
      "Please use one of the possible inputs for 'method': 'hclust', 'network' or 'both'. Default is 'hclust'."
    )
  }
  
  # to change minor things in other functions when running the big function, such as using readline() after the Dendrogram.
  ordinationFunction <-  TRUE
  
  processedMetaMatrix <-
    processMetaDataMatrix(
      metaMatrix,
      language = language,
      stemWords = stemWords,
      ignoreWords = ignoreWords,
      ordinationFunction = ordinationFunction,
      saveToWd = saveToWd,
      longMessages = longMessages
    )
  
  if(!is.na(myAPIKey)){
    processedMetaMatrix$MetaMatrix <-
      getScopusMetaData(processedMetaMatrix$MetaMatrix,
                        myAPIKey,
                        ordinationFunction = TRUE)
  }

  
  if (any(c(method == "hclust"), (method == "both"))) {
    modeledData <-
      calculateModels(
        processedMetaMatrix,
        numberOfClusters = numberOfClusters,
        minWordsPerCluster = minWordsPerCluster,
        maxWordsPerCluster = maxWordsPerCluster,
        p = p,
        dendrogram = dendrogram,
        dendroLabels = dendroLabels,
        ordinationFunction = ordinationFunction,
        saveToWd = saveToWd,
        longMessages = longMessages
      )
    createOrdinationPlot(modeledData,
                         exactPosition = exactPosition,
                         ordinationFunction = ordinationFunction)
    
    if(!is.na(myAPIKey)){ # if there's no API key, we don't need the influencial papers
      ANSWER <-
        readline("Show most influencial papers per cluster? (y/n)")
      if (substr(ANSWER, 1, 1) == "y") {
        mostImportantPaperPerCluster(modeledData)
      }
    }
    
  }
  if (method == "both") {
    readline("Proceed with network analysis?")
  }
  if (any(c(method == "network"), (method == "both"))) {
    modeledNetwork <-
      calculateNetwork(
        processedMetaMatrix,
        saveToWd = saveToWd,
        ordinationFunction = ordinationFunction,
        longMessages = longMessages
      )
  }
  
  if (method == "hclust")
    return(modeledData)
  if (method == "network")
    return(modeledNetwork)
  if (method == "both")
    return(c(modeledData, modeledNetwork))
  
  
  
}























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
    print("Warning: entries are not having unique ID's, assiging new ones")
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
  }
  
  if (useMoritz == TRUE) {
    if (nrow(moritz) < 10) {
      cat(
        "Please provide the .csv-file in the following format:\n\nwords;select\nabandon;1\nabbott;0\n...\n(',' instead of ';' is fine as well.)\n\nor just have a list of words you'd like to keep."
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















#' @title calculateModels
#'
#' @description The third function to the word analysis with ginko. It takes the
#'     output of \code{\link[ginko]{processMetaDataMatrix}} and calculates
#'     ordination and cluster models. Each paper is assigned to one cluster
#'     while each word receives an indicator value with \code{\link[labdsv]{indval}}
#'     for each cluster, showing how representative word is for a cluster. The top
#'     representative words will be used in the further process with \code{\link[ginko]{createOrdinationPlot}}.
#'
#' @author Matthias Nachtmann, \email{matthias.nachtmann@@stud.leuphana.de},
#'     Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de}
#' @param processedData result of \code{\link[ginko]{processMetaDataMatrix}}
#' @param numberOfClusters integer or NA; an integer forces the number of clusters,
#'     NA results in an automatic iteration to determine the optimal amount.
#' @param minWordsPerCluster minimum number of words to be plotted per cluster.
#' @param maxWordsPerCluster maximum number of words to be plotted per cluster.
#'     this is only hapening if the indicator values are higher than the maximum
#'     value of the weakest cluster arm.
#' @param p p-Value to determine the significance of individual words. Only
#'     significant words will be plotted.
#' @param dendrogram shows a dendrogram of the calculated cluster if set to \code{TRUE}.
#' @param dendroLabels allows "truncated" or "break" to either truncate the
#'     labels of the dendrogram leaves or put a line break. Line breaks are not
#'     recommend for a large number of PDFs.
#' @param saveToWd a logical parameter whether or not to save the output of the
#'     function to the working directory. This is especially useful for later
#'     analysis steps and can be read in by using \code{\link[base]{saveRDS}}.
#' @param ordinationFunction placeholder
#' @param longMessages placeholder
#' @seealso \code{\link{processMetaDataMatrix}} for the preceding step,
#'     \code{\link{createOrdinationPlot}} for the graphics,
#'     \code{\link{mostImportantPaperPerCluster}} and
#'     \code{\link{inspectGinko}} for a summary of the analysis
#' @family ginko functions

#' @return list of two entries. First entry contains all the information to
#'     generate the ordination/cluster plot. Entry two is an updated version
#'     of the metaMatrix, which now included the assigned cluster number per
#'     paper to allow additional statistical analyses.
#' @export
#' @examples \dontrun{
#' placeholder}
calculateModels <- function(processedData,
                            numberOfClusters = NA,
                            minWordsPerCluster = 5,
                            maxWordsPerCluster = 10,
                            p = 0.05,
                            dendrogram = TRUE,
                            dendroLabels = "truncated",
                            saveToWd = TRUE,
                            ordinationFunction = FALSE,
                            longMessages = FALSE) {
  # error handling
  # ignore to tiny p values. labdvs::inval only provides three decimal digits
  if (p < 0.001) {
    p <- 0.001
  }
  
  if (maxWordsPerCluster < minWordsPerCluster) {
    print("Warning: maxWordsPerCluster < minWordsPerCluster")
    print(paste0("Adjusting minWordsPerCluster to: ", maxWordsPerCluster))
    minWordsPerCluster <- maxWordsPerCluster
  }
  
  if (length(numberOfClusters) != 1) {
    print(paste0("numberOfClusters has an invalid value: ", numberOfClusters))
    print("switching to automatic calculation")
    numberOfClusters <- NA
  }
  
  
  if (!is.na(numberOfClusters)) {
    if (!is.numeric(numberOfClusters)) {
      print(paste0(
        "numberOfClusters has an invalid value: ",
        numberOfClusters
      ))
      print("switching to automatic calculation")
      numberOfClusters <- NA
    }
  }
  
  
  
  
  
  if (longMessages == TRUE) {
    Sys.sleep(1)
    cat("Calculating models\n")
    Sys.sleep(1)
  }
  
  model <-
    vegan::decorana(processedData$BinaryWordList, iweigh = 0) #all row sums must be >0 in the community matrix
  axisPositions <- vegan::scores(model, display = c("species"))
  
  #plot(model,type="text")
  #write.table(cbind(rownames(processedData[[1]]),c(1:351)),"numberstest.txt")
  
  #cluster
  # replaced agnes by hclust from mclust
  disthclust <-
    stats::dist(processedData$BinaryWordList, method = "binary")
  modelclust <- stats::hclust(disthclust, method = "ward.D")
  indSpeciesValues <- c()
  
  if (is.na(numberOfClusters)) {
    numberOfClusters = 2
    numberOfSignificantIndicators = 0
    numberOfSignificantIndicatorsNew = 1
    
    cat("Determining the optimal number of clusters...\n")
    
    
    while (numberOfSignificantIndicators < numberOfSignificantIndicatorsNew) {
      numberOfSignificantIndicators <- numberOfSignificantIndicatorsNew
      
      cutmodel <-
        stats::cutree(modelclust, k = numberOfClusters) #assigns a cluster number to every paper
      #table(cutmodel)
      indSpeciesValues <-
        labdsv::indval(processedData$BinaryWordList, cutmodel, numitr = 1000)
      if (longMessages == TRUE) {
        cat(
          paste0(
            "- Number of clusters: ",
            numberOfClusters,
            "; significant words (p < ",
            p,
            "): ",
            length(indSpeciesValues$pval[indSpeciesValues$pval <= p]),
            "\n"
          )
        )
      }
      
      
      numberOfSignificantIndicatorsNew <-
        length(indSpeciesValues$pval[indSpeciesValues$pval <= p])
      
      #this if statements corrects the numberOfClusters if we exeeded the maximum of significant indicators
      if (numberOfSignificantIndicators < numberOfSignificantIndicatorsNew) {
        numberOfClusters <- numberOfClusters + 1
      } else{
        numberOfClusters <- numberOfClusters - 1
      }
      
    }
    
    cat(paste0("- the optimal number of clusters is: ", numberOfClusters),
        "\n")
  }
  
  
  cutmodel <-
    stats::cutree(modelclust, k = numberOfClusters) #assigns a cluster number to every paper
  Cluster <- as.numeric(cutmodel)
  
  
  
  # Performs a Dufrene-Legendre Indicator Species Analysis that calculates the indicator value
  # (fidelity and relative abundance) of species in clusters or types.
  indSpeciesValues <-
    labdsv::indval(processedData$BinaryWordList, cutmodel, numitr = 1000)
  
  # Combines all relevant values for the indicator species analysis (including axis positions)
  combIndSpeciesValues <-
    cbind(
      round(indSpeciesValues$indval, digits = 2),
      indSpeciesValues$pval,
      names(indSpeciesValues$pval),
      axisPositions
    )
  
  # Only takes the p values that are smaller than the given p value passed as an argument (0.05 per default)
  signIndSpeciesValues <-
    combIndSpeciesValues[combIndSpeciesValues["indSpeciesValues$pval"] <= p, ]
  
  
  # #Backup
  # # this part of the codes adds an additional column to the dataframe that shows in which cluster the most important words are.
  # # this information is useful to assign colors while plotting
  # signIndSpeciesValuesInclSubsetRow <- data.frame()
  # signIndSpeciesValuesInclSubsetRow <- signIndSpeciesValues[order(signIndSpeciesValues[,1],decreasing=T)[c(1: wordsPerClusterArm)],]
  # signIndSpeciesValuesInclSubsetRow$subset <- paste0("Cluster ", rep.int(1, wordsPerClusterArm))
  
  # for (i in 2:numberOfClusters) {
  #   subset <- signIndSpeciesValues[order(signIndSpeciesValues[,i],decreasing=T)[c(1: wordsPerClusterArm)],]
  #   subset$subset <- paste0("Cluster ", rep.int(i, wordsPerClusterArm))
  #   signIndSpeciesValuesInclSubsetRow <- rbind(signIndSpeciesValuesInclSubsetRow, subset)
  # }
  
  # it also determines the optimal amount of words to print per cluster. it checks the maximal indicator values per cluster.
  # the smallest of those becomes the benchmark and if one word has a higher indval than this it is also drawn on the plot
  
  
  highestIndValPerCluster <- c()
  for (i in 1:numberOfClusters) {
    highestRankedWord <-
      signIndSpeciesValues[order(signIndSpeciesValues[, i], decreasing = T)[c(1)], ]
    highestIndValPerCluster <-
      c(highestIndValPerCluster, highestRankedWord[, i])
  }
  
  
  numberOfWords <- c()
  
  signIndSpeciesValuesInclSubsetRow <- data.frame()
  
  
  highValues <-
    (signIndSpeciesValues[, 1] > min(highestIndValPerCluster))
  highValues <- length(highValues[highValues == TRUE])
  
  if (highValues > maxWordsPerCluster) {
    numberOfWords <- maxWordsPerCluster
  } else if (highValues > minWordsPerCluster) {
    numberOfWords <- highValues
  } else{
    numberOfWords <- minWordsPerCluster
  }
  
  signIndSpeciesValuesInclSubsetRow <-
    signIndSpeciesValues[order(signIndSpeciesValues[, 1], decreasing = T)
                         [c(1:numberOfWords)], ]
  signIndSpeciesValuesInclSubsetRow$subset <-
    paste0("Cluster ", rep.int(1, numberOfWords))
  
  
  
  for (i in 2:numberOfClusters) {
    highValues <-
      (signIndSpeciesValues[, i] > min(highestIndValPerCluster))
    highValues <- length(highValues[highValues == TRUE])
    
    if (highValues > maxWordsPerCluster) {
      numberOfWords <- maxWordsPerCluster
    } else if (highValues > minWordsPerCluster) {
      numberOfWords <- highValues
    } else{
      numberOfWords <- minWordsPerCluster
    }
    
    subset <-
      signIndSpeciesValues[order(signIndSpeciesValues[, i], decreasing = T)[c(1:numberOfWords)], ]
    subset$subset <- paste0("Cluster ", rep.int(i, numberOfWords))
    signIndSpeciesValuesInclSubsetRow <-
      rbind(signIndSpeciesValuesInclSubsetRow, subset)
  }
  
  
  
  modeledData <- list()
  modeledData[[1]] <- signIndSpeciesValuesInclSubsetRow
  
  
  
  # add a dendrogram of the papers
  if (dendrogram == TRUE) {
    if (dendroLabels == "break") {
      # this breaks the file names
      wordwrap <-
        function(x, len)
          paste(strwrap(x, width = len), collapse = "\n")
      graphics::plot(
        modelclust,
        cex = 0.6,
        hang = -1,
        main = "Word cluster dendrogram of papers",
        labels = sapply(processedData$MetaMatrix[, "FileName"], wordwrap, len =
                          15),
        padj = 1
      )
      #18th column is the filename, strwrap() splits the labels
    }
    if (dendroLabels == "truncated") {
      # this truncates the labels to 18 characters, followed by "..."
      longLabels <- processedData$MetaMatrix[, "FileName"]
      shortenedLabels <- NULL
      for (i in 1:length(longLabels)) {
        shortenedLabels[i] <-
          ifelse(
            nchar(longLabels[i]) > 20,
            # shorten labels from 1:18
            paste(
              paste(
                unlist(strsplit(longLabels[i], ""))[1:18],
                sep = "",
                collapse = ""
              ),
              "...",
              sep = "",
              collapse = ""
            ),
            longLabels[i]
          )
      }
      graphics::plot(
        modelclust,
        cex = 0.6,
        hang = -1,
        main = "Word cluster dendrogram of papers",
        labels = shortenedLabels
      )
    }
  }
  
  
  # colnames(processedData[[2]])
  # modeledData[[2]] <- cbind(processedData[[2]], as.numeric(Cluster))
  # colnames(modeledData[[2]]) <- c(colnames(processedData[[2]]), "Cluster")
  modeledData[[2]] <-
    cbind(processedData$MetaMatrix, "Cluster" = Cluster)
  
  
  # Representative papers based on the percentage of significant words in each paper
  # only the papers from signIndSpeciesValues from processed data
  ClusterContent <-
    signIndSpeciesValues[, "names(indSpeciesValues$pval)"]
  ClusterContent <- as.data.frame(ClusterContent)
  
  # select said columns (words) from processedData which is a 0-1 matrix of papers and words
  representativePapers <-
    as.data.frame(processedData$BinaryWordList[, ClusterContent[, 1]])
  rownames(representativePapers) <-
    processedData$MetaMatrix[, "FileName"] # take the filenames as row names
  
  # Extracting the percentage
  # give each paper a percentage value and call the column percentageOfSignWordsInPaper
  # this dataframe also has the words in it in case you'd like to further investigate the words used
  
  # A weighted percentage
  # This is done by adding up the indicator species value of word i in cluster j if the word exists
  # for paper 1 which is in cluster j.
  
  for (i in 1:nrow(representativePapers)) {
    ClusterOfPaper <- Cluster[i] # the cluster paper i is in
    representativePapers[i, ] <-
      as.numeric(representativePapers[i, ]) * #take the 0/1 if the word exists
      signIndSpeciesValues[[ClusterOfPaper]] # and multiply it by the indicator species value for
    # said cluster
    
  }
  
  
  representativePapers$percentageOfSignWordsInPaper <-
    rowSums(representativePapers) / ncol(representativePapers)
  
  representativePapersEasyToOpen <-
    cbind(representativePapers$percentageOfSignWordsInPaper,
          Cluster)
  representativePapersEasyToOpen <-
    as.data.frame(representativePapersEasyToOpen)
  rownames(representativePapersEasyToOpen) <-
    trimws(processedData$MetaMatrix[, "FileName"]) # take the filenames as row names
  colnames(representativePapersEasyToOpen) <-
    c("percentageOfSignWordsInPaper", "Cluster")
  
  modeledData[[3]] <- representativePapersEasyToOpen
  
  modeledData[[4]] <- processedData$numberOfWords
  names(modeledData) <-
    c("IndVal",
      "MetaMatrix",
      "RepresentativePapers",
      "numberOfWords")
  
  
  ### save each paper into one new folder
  dir.create("PdfsPerCluster/")
  for (i in 1:nlevels(as.factor(cutmodel))) {
    dir.create(paste("PdfsPerCluster/", i))
    file.copy(
      paste0("PDFs/", rownames(representativePapersEasyToOpen[representativePapersEasyToOpen[, 2] ==
                                                                i, ])),
      to = paste("PdfsPerCluster/", i),
      copy.mode = T
    )
  }
  
  cat(
    paste0(
      "\nAll PDFs have been copied to different subfolders in the new folder 'PdfsPerCluster' according to the cluster they belong to.\n"
    )
  )
  
  if (saveToWd == TRUE) {
    modeledDataFile <-
      paste0("modeledData", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"))
    saveRDS(modeledData, file = modeledDataFile)
    
    if (ordinationFunction == FALSE) {
      cat(
        paste0(
          "\nThe modeled Data is now in your global environment. It is also saved as a file in your working directory. If you work with the same data again, you can skip this step in future analysis by reading in the file:\nmodeledData <- readRDS(file= '",
          modeledDataFile,
          "')\n\n"
        )
      )
    } else {
      cat(
        paste0(
          "Modeled Data saved. You can read it in using:\nmodeledData <- readRDS(file= '",
          modeledDataFile,
          "')\n###################################################################################################\n\n"
        )
      )
      
    }
    
  }
  
  return(modeledData)
  
}












#' @title createOrdinationPlot
#'
#' @description The fourth function to the word analysis with ginko. It creates
#'     five different plots that set the results in context to the years
#'     published and more. To show meaningful graphics, the use of
#'     \code{\link[ginko]{getScopusMetaData}} is recommended.
#'
#' @author Matthias Nachtmann, \email{matthias.nachtmann@@stud.leuphana.de},
#'     Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de}
#' @param modeledData result of \code{\link[ginko]{calculateModels}}
#' @param exactPosition \code{TRUE} or \code{FALSE}, the plot function tries
#'     to avoid overlapping labels for the sake of visual siplicity over perfect
#'     precission. When set to \code{TRUE}, the words position will be marked by
#'     a dot and the label will be connected with a line to it.
#' @param ordinationFunction for internal use.
#' @family ginko functions
#' @seealso \code{\link{calculateModels}} for the preceding step,
#'     \code{\link{mostImportantPaperPerCluster}} and
#'     \code{\link{inspectGinko}} for a summary of the analysis
#' @return a graphic based on the calculated model and some additional barplot
#'     to deepen the understanding of the dataset.
#' @export
#' @examples \dontrun{
#' placeholder}
createOrdinationPlot <- function(modeledData,
                                 exactPosition = FALSE,
                                 ordinationFunction = FALSE) {
  # if(colorPalette == "default"){
  #   colorPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
  # }
  
  Sys.sleep(1)
  cat("Creating graphic\n")
  Sys.sleep(1)
  
  if (exactPosition == TRUE) {
    # This is the cluster cloud plot with labels to exact positions
    ordinationPlot <- ggplot2::ggplot(modeledData[[1]]) +
      ggplot2::geom_point(
        ggplot2::aes(x = modeledData[[1]]$DCA1, y = modeledData[[1]]$DCA2),
        size = 4,
        color = 'grey'
      ) +
      ggrepel::geom_label_repel(
        ggplot2::aes(
          modeledData[[1]]$DCA1,
          modeledData[[1]]$DCA2,
          fill = factor(subset),
          label = modeledData[[1]][, "names(indSpeciesValues$pval)"]
        ),
        fontface = 'bold',
        color = 'white',
        box.padding = ggplot2::unit(0.35, "lines"),
        point.padding = ggplot2::unit(0.3, "lines"),
        segment.color = 'grey50'
      ) +
      ggplot2::labs(x = "DCA 1", y = "DCA 2") +
      ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
      #ggplot2::scale_x_continuous(breaks=c(seq(-100,100,0.5))) +
      #ggplot2::scale_y_continuous(breaks=c(seq(-100,100,0.5))) +
      ggplot2::theme_classic(base_size = 16)
    
    
  } else{
    # the plot without labels to exact positions
    ordinationPlot <- ggplot2::ggplot(modeledData[[1]]) +
      ggrepel::geom_label_repel(
        ggplot2::aes(
          modeledData[[1]]$DCA1,
          modeledData[[1]]$DCA2,
          fill = factor(subset),
          label = modeledData[[1]][, "names(indSpeciesValues$pval)"]
        ),
        fontface = 'bold',
        color = 'white',
        box.padding = ggplot2::unit(0.15, "lines"),
        segment.color = NA
      ) +
      ggplot2::labs(x = "DCA 1", y = "DCA 2") +
      ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
      #ggplot2::scale_x_continuous(breaks=c(seq(-100,100,0.5))) +
      #ggplot2::scale_y_continuous(breaks=c(seq(-100,100,0.5))) +
      ggplot2::theme_classic(base_size = 16)
  }
  
  
  
  if (ordinationFunction == TRUE) {
    readline("Show next plot?")
  }
  
  graphics::plot(ordinationPlot)
  # If we have metadata, so if the column has less than 25% NA, we do the other plots.
  if(sum(is.na(modeledData$MetaMatrix[, "CitedBy"]))<nrow(modeledData$MetaMatrix)/4){
    
  
    #preparing the data for meta data plots
    naFreeData1 <-
      subset(modeledData$MetaMatrix,!is.na(modeledData$MetaMatrix[, "Year"]))
    naFreeData2 <- subset(naFreeData1,!is.na(naFreeData1[, "CitedBy"]))
    #naFreeData <- as.data.frame(naFreeData1) # to only have the years
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
          "Please use your API key to obtain the metadata of your papers from scopus. Then, I'll be able to generate plots of citations and years.\n"
        )
      )
    }
    naFreeData$citePercent <- citePercent
    
    
    ClusterPercent <- c()
    for (i in 1:nrow(naFreeData)) {
      intermediateCluster <- as.numeric(naFreeData[i, "Cluster"])
      intermediateSum <-
        sum(as.numeric(naFreeData$Cluster[naFreeData[, "Year"] == naFreeData[i, "Year"]]))
      percentage <- intermediateCluster / intermediateSum * 100
      ClusterPercent <- c(ClusterPercent, percentage)
    }
    naFreeData$ClusterPercent <- ClusterPercent
    
    
    
    naFreeData$ClusterString <-
      apply(naFreeData["Cluster"], 1, function(x)
        paste0("Cluster ", x))
    
    
    
    # #stacked area chart
    # citationsAreaChart <- ggplot2::ggplot(naFreeData,
    #   ggplot2::aes(
    #     x=naFreeData$Year,
    #     y=as.numeric(naFreeData$CitedBy),
    #     fill=naFreeData$Cluster)
    # ) +
    #    ggplot2::geom_area()
    
    
    
    
    
    # #stacked area chart, percentage
    # citationsAreaChartPercent <- ggplot2::ggplot(naFreeData,
    #   ggplot2::aes(
    #     x=as.numeric(naFreeData$Year),
    #     y=as.numeric(naFreeData$citePercent),
    #     fill=naFreeData$Cluster
    #   )
    # ) +
    #    ggplot2::geom_area()
    
    
    
    
    
    #citations per year - stacked bar plot
    citationsStackedBarPlot <- ggplot2::ggplot(
      naFreeData,
      ggplot2::aes(
        x = naFreeData$Year,
        y = as.numeric(naFreeData$CitedBy),
        fill = naFreeData$ClusterString
      )
    ) +
      ggplot2::labs(x = "Year", y = "Citations") +
      ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::theme_classic(base_size = 16)
    
    
    
    
    # citations per year - stacked bar plot, percentage
    citationsStackedBarPlotPercent <- ggplot2::ggplot(
      naFreeData,
      ggplot2::aes(
        x = naFreeData$Year,
        y = as.numeric(naFreeData$citePercent),
        fill = naFreeData$ClusterString
      )
    ) +
      ggplot2::labs(x = "Year", y = "Citations [%]") +
      ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::theme_classic(base_size = 16)
    
    
    
    # paper per cluster per year - stacked bar plot
    paperPerClusterPerYearStackedBarPlot <-
      ggplot2::ggplot(
        naFreeData,
        ggplot2::aes(
          x = naFreeData$Year,
          y = as.numeric(naFreeData$Cluster),
          fill = naFreeData$ClusterString
        )
      ) +
      ggplot2::labs(x = "Year", y = "Amount of papers") +
      ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::theme_classic(base_size = 16)
    
    
    # paper per cluster per year - stacked bar plot, percentage
    paperPerClusterPerYearStackedBarPlotPercent <-
      ggplot2::ggplot(
        naFreeData,
        ggplot2::aes(
          x = naFreeData$Year,
          y = as.numeric(naFreeData$ClusterPercent),
          fill = naFreeData$ClusterString
        )
      ) +
      ggplot2::labs(x = "Year", y = "Amount of papers [%]") +
      ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::theme_classic(base_size = 16)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # #map experiments
    
    # modeledData[[2]] <- cbind(processedData[[2]], "AffiliationComplete"=rep(NA, nrow(modeledData[[2]])))
    
    # for (i in 1:nrow(modeledData[[2]])) {
    #   Affiliation <- as.vector(unlist(strsplit(modeledData[[2]][i,"Affiliation"], ", ")))
    #   AffiliationCity <- as.vector(unlist(strsplit(modeledData[[2]][i,"Affiliation-City"], ", ")))
    #   AffiliationCountry <- as.vector(unlist(strsplit(modeledData[[2]][i,"Affiliation-Country"], ", ")))
    
    
    
    #   intermediateAffiliationComplete <- c()
    #   if((length(Affiliation) == length(AffiliationCity))&(length(Affiliation) == length(AffiliationCountry))){
    
    #     for(j in 1:length(Affiliation)){
    
    #       currentAffiliation <- paste(Affiliation[j], AffiliationCity[j], AffiliationCountry[j], sep = ", ")
    #       if(currentAffiliation == "NA, NA, NA"){currentAffiliation<-NA}
    
    #       intermediateAffiliationComplete <- c(intermediateAffiliationComplete, currentAffiliation)
    
    #     }
    
    #     modeledData[[2]][i,"AffiliationComplete"] <- paste(intermediateAffiliationComplete, collapse="; ")
    
    
    #   }else{
    #     modeledData[[2]][i,"AffiliationComplete"] <- NA
    #   }
    # }
    
    
    
    # locationMatrix
    
    
    
    
    
    
    
    
    
    
    
    # modeledData[[2]] <- cbind(processedData[[2]], "Longitude"=rep(NA, nrow(modeledData[[2]])))
    # modeledData[[2]] <- cbind(processedData[[2]], "Latitide"=rep(NA, nrow(modeledData[[2]])))
    
    # for (i in 1:nrow(modeledData[[2]])) {
    
    #   latlon = geocode(naFreeData[i,1])
    #   modeledData[[2]][i,"Longitude"] <- as.numeric(latlon[1])
    #   modeledData[[2]][i,"Latitide"] <- as.numeric(latlon[2])
    # }
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    readline("Show next plot?")
    
    graphics::plot(citationsStackedBarPlot)
    
    readline("Show next plot?")
    
    graphics::plot(citationsStackedBarPlotPercent)
    
    readline("Show next plot?")
    
    graphics::plot(paperPerClusterPerYearStackedBarPlot)
    
    readline("Show next plot?")
    
    graphics::plot(paperPerClusterPerYearStackedBarPlotPercent)
  
  } # end of if-metadata-there-loop
}








#' @title mostImportantPaperPerCluster
#'
#' @description placeholder
#'
#' @author Matthias Nachtmann, \email{matthias.nachtmann@@stud.leuphana.de},
#'     Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de}
#' @param modeledData result of \code{\link[ginko]{calculateModels}}
#' @family ginko functions
#' @seealso \code{\link{calculateModels}} for the preceding step
#'     \code{\link{createOrdinationPlot}} for the graphics,
#'     \code{\link{inspectGinko}} for a summary of the analysis
#' @return console output of the papers with the hightest citation per year
#'     count per cluster.
#' @export
#' @examples \dontrun{
#' placeholder}
mostImportantPaperPerCluster <- function(modeledData) {
  numberOfClusters <-
    nrow(as.data.frame(table(modeledData$MetaMatrix[, "Cluster"]))) #get the number of clusters from the provided data
  
  for (i in 1:numberOfClusters) {
    paperPerCluster = 5
    
    newSubset <-
      subset(modeledData$MetaMatrix, modeledData$MetaMatrix[, "Cluster"] == i)
    
    newSubset[, "CitedBy"] <- as.numeric(newSubset[, "CitedBy"])
    newSubset[, "CitationPerYear"] <-
      as.numeric(newSubset[, "CitationPerYear"])
    
    if (nrow(newSubset) < paperPerCluster) {
      cat(
        paste0(
          "*** Cluster ",
          i,
          " only has ",
          nrow(newSubset),
          " papers. Please consider a bigger dataset for more reliable results. ***\n"
        )
      )
      
    } else{
      #Order Subset by two levels. Main filter is citesPerYear and secondary is overall citations ==> if the citesPerYear are the same, younger is better
      newSubset2 <-
        newSubset[order(as.numeric(newSubset[, "CitedBy"]), decreasing = T), ]
      orderedSubset <-
        newSubset2[order(as.numeric(newSubset2[, "CitationPerYear"]), decreasing =
                           T)[c(1:paperPerCluster)], ]
      
      # # Sort by column index [1] then [3]
      # dataframe[
      # order( dataframe[,1], dataframe[,3] ),
      # ]
      
      
      
      
      
      paperNames <- c()
      CitedBy <- as.vector(unlist(orderedSubset[, "CitedBy"]))
      CitationPerYear <-
        as.vector(unlist(orderedSubset[, "CitationPerYear"]))
      
      
      for (j in 1:paperPerCluster) {
        Title   <- as.vector(unlist(orderedSubset[j, "Title"]))
        Year    <- as.vector(unlist(orderedSubset[j, "Year"]))
        Volume  <- as.vector(unlist(orderedSubset[j, "Volume"]))
        Issue   <- as.vector(unlist(orderedSubset[j, "Issue"]))
        Journal <- as.vector(unlist(orderedSubset[j, "Journal"]))
        Pages   <- as.vector(unlist(orderedSubset[j, "Pages"]))
        DOI     <- as.vector(unlist(orderedSubset[j, "DOI"]))
        Authors <-
          as.vector(unlist(strsplit(orderedSubset[j, "Authors"], ", ")))
        Authors <- if (length(Authors) == 1) {
          Authors
        } else if (length(Authors) == 2) {
          paste(Authors[1], "and", Authors[2])
        } else if (length(Authors) > 2) {
          paste(Authors[1], "et al.")
        } else{
          NA
        }
        
        paperName <-
          paste0(
            Authors,
            ' (',
            Year,
            '). "',
            Title,
            '" In: ',
            Journal,
            " ",
            if (!is.na(Volume)) {
              Volume
            } else{
              ""
            },
            if (!is.na(Issue)) {
              "."
            } else{
              ""
            },
            if (!is.na(Issue)) {
              Issue
            } else{
              ""
            },
            if (!is.na(Pages)) {
              ", pp."
            } else{
              ""
            },
            if (!is.na(Pages)) {
              Pages
            } else{
              ""
            },
            if (!is.na(DOI)) {
              ". DOI: "
            } else{
              ""
            },
            if (!is.na(DOI)) {
              DOI
            } else{
              ""
            }
          )
        
        paperNames <- c(paperNames, paperName)
        
      }
      
      printWithBlankLines <- function(x) {
        cat(x, sep = "\n\n")
      }
      
      writeLines(c(
        "",
        "",
        paste0("Most influencial papers in cluster ", i, ":"),
        paste0(
          "Citations per Year: ",
          paste(CitationPerYear, collapse = ", ")
        ),
        paste0("Citation Count: ", paste(CitedBy, collapse = ", ")),
        "",
        ""
      ))
      
      printWithBlankLines(paperNames)
    } # this is only done if there are 5 papers in the cluster.
    
  }
  
  
  
  
}

########################################################################
# Workflow for working with networks
########################################################################

#' @title calculateNetwork
#'
#' @description This function is an alternative to
#'     \code{\link[ginko]{calculateModels}} and clusters based on network theory.
#' @param processedMetaMatrix requires the output of
#'     \code{\link[ginko]{processMetaDataMatrix}}.
#' @param sortby allows the following possible inputs: "Eigenvector", "Degree",
#'     "Closeness, "Betweenness". The centrality measure to sort the words by,
#'     default is Eigenvector.
#' @param keep The argument keep keeps by default 0.33 of all the words, sorted
#'     by the argument given by \code{sortby}. Can be adjusted. This easies
#'     computations for later use.
#' @param saveToWd placeholder
#' @param ordinationFunction placeholder
#' @param longMessages placeholder
#' @seealso \code{\link{processMetaDataMatrix}} for the preceding step,
#'     \code{\link{inspectGinko}} for a summary of the analysis
#'
#' @author Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de}, Julius Rathgens,
#'     \email{julius.rathgens@@leuphana.de}
#' @return   output:
#'     $LocalMeasures will return the local measurements for both papers and
#'     words
#'     $ReducedLocalMeasures will return 1/3 of the words (!) with their
#'     centrality measures & clustering according to three different clustering
#'     methods, arranged by eigenvector centrality (can be changed)
#'     $ReducedIncidenceMatrix will return 1/3 of the words arranged by
#'     eigenvector centrality, to be further processed eg in Gephi or with other
#'     clustering functions
#'     $GlobalMeasures will return the global measurements
#' @export


# possible future avances:
# 1) using
#library(tnet)
# 2) clustering in R and giving the user a matrix to do in Gephi (with a short
# description?)
# 3) @Julius! to be improved: check significant values of clusters? I just returned the
# memberships.
# 4) onemode matrices? Or do we not need them?
# 5) networkMatrix[[5]] <- brokeragePoints
# 6) which other global measures do we want?


calculateNetwork <- function(processedMetaMatrix,
                             sortby = "Eigenvector",
                             keep = 0.33,
                             saveToWd = TRUE,
                             ordinationFunction = FALSE,
                             longMessages = FALSE) {
  if (any(c(
    is.null(processedMetaMatrix[[1]]),
    is.null(processedMetaMatrix[[2]])
  ))) {
    cat("Make sure to use processedMetaMatrix from processMetaDataMatrix().")
  } #improve it with tryCatch()
  
  # keep has to be numeric between 0 and 1
  if (any(c(!is.numeric(keep), keep > 1, keep < 0))) {
    cat("Please enter a numeric value between 0 and 1 for 'keep'.")
  }
  
  # sort by the given centralities
  if (sum(
    c(
      sortby == "Eigenvector",
      sortby == "Degree",
      sortby == "Closeness",
      sortby == "Betweenness"
    )
  ) != 1) {
    cat(
      "Please use one of the possible inputs for 'sortby': 'Eigenvector', 'Degree', 'Closeness', 'Betweenness'. Default is 'Eigenvector'."
    )
  }
  
  cat(
    "\n####################################################################################\nNETWORK ANALYSIS\n"
  )
  ###### Generating a network
  sna <- as.matrix(processedMetaMatrix$BinaryWordList)
  papers = nrow(sna)
  sna.transposed <- t(sna)
  snaIncidence <-
    igraph::graph.incidence(sna.transposed, directed = FALSE)
  snaEdgelist <- igraph::as_edgelist(snaIncidence)
  
  
  #snaTnet <- as.tnet(snaEdgelist, type = "binary two-mode tnet")
  snaTnet <-
    bipartite::web2edges(sna.transposed,
                         weight.column = FALSE,
                         return = TRUE)
  
  ###### Generating two weighted one-mode networks
  #wordNetwork <- projecting_tm(snaTnet, method="sum") #took forever, abandoned
  #paperNetwork <- projecting_tm(snaTnet, method="sum") #switch order in edgelist
  
  
  ###### Calculating local measures
  if (longMessages == TRUE) {
    Sys.sleep(1)
    cat("Calculating local measures...\n")
    Sys.sleep(1)
    
    
    pb <- utils::txtProgressBar(min = 1,
                                max = 5,
                                style = 3)
  }
  
  # Degree
  nodeDegree <- igraph::degree(snaIncidence)
  if (longMessages == TRUE) {
    utils::setTxtProgressBar(pb, 1)
  }
  #nodedegreeTM <- degree_tm(snaTnet, measure = "degree")
  # the same result, so one is taken out
  
  # Betweenness
  nodeBetweenness <- igraph::betweenness(snaIncidence)
  if (longMessages == TRUE) {
    utils::setTxtProgressBar(pb, 2)
  }
  
  # Closeness
  nodeCloseness <- igraph::closeness(snaIncidence)
  if (longMessages == TRUE) {
    utils::setTxtProgressBar(pb, 3)
  }
  
  # Eigenvector
  nodeEigenvector <- igraph::eigen_centrality(snaIncidence)$vector
  if (longMessages == TRUE) {
    utils::setTxtProgressBar(pb, 4)
  }
  
  
  networkMatrix <- list()
  networkMatrix[[1]] <-
    cbind(nodeDegree, nodeBetweenness, nodeCloseness, nodeEigenvector) # [[1]] will be the papers
  if (longMessages == TRUE) {
    utils::setTxtProgressBar(pb, 5)
  }
  
  if (longMessages == TRUE) {
    close(pb)
    
    ######## Clustering
    Sys.sleep(1)
    cat("Clustering based on the network...\n")
    Sys.sleep(1)
    
    pb <- utils::txtProgressBar(min = 1,
                                max = 6,
                                style = 3)
    
    # mind the order
    #reinforcementTM <- reinforcement_tm(snaTnet) #took forever, abandoned
    #clusterTM <- clustering_tm(snaTnet) #took forever, abandoned
    #clusteringLocalTM <- clustering_local_tm(snaTnet) #took forever, abandoned
    utils::setTxtProgressBar(pb, 1)
  }
  clusterWalktrap <-
    igraph::membership(igraph::cluster_walktrap(snaIncidence, steps = 10))
  
  
  clusterGreedy <-
    igraph::membership(igraph::fastgreedy.community(snaIncidence))
  if (longMessages == TRUE) {
    utils::setTxtProgressBar(pb, 2)
  }
  
  #clusterEdgeBetweenness <- cluster_edge_betweenness(snaIncidence) #took forever, abandoned
  clusterInfomap <-
    igraph::membership(igraph::cluster_infomap(snaIncidence))
  if (longMessages == TRUE) {
    utils::setTxtProgressBar(pb, 3)
  }
  
  networkMatrix[[1]] <-
    cbind(networkMatrix[[1]],
          clusterWalktrap,
          clusterGreedy,
          clusterInfomap)
  
  ##### Threshold for the words
  # keep row [1:nrow(networkMatrix[[2]])*keep]
  # only 1/3 most important by some chosen centrality
  # possible inputs: "Eigenvector", "Degree", "Closeness, "Betweenness"
  sortbyName <- paste0("node", sortby) # get the right column name
  networkMatrix[[2]] <-
    networkMatrix[[1]][sort.list(networkMatrix[[1]][, sortbyName], decreasing = TRUE), ]
  
  # a parameter used later as well that determines the number of rows to be kept.
  
  namefirstcolumn <-
    rownames(networkMatrix[[2]]) #### Rookie Version: creates an accessible duplicate of the rownames
  networkMatrix[[2]] <- cbind(namefirstcolumn, networkMatrix[[2]])
  test <-
    stringr::str_detect(networkMatrix[[2]][, "namefirstcolumn"], ".pdf") #### Deletes all the papers based on .pdf
  networkMatrix[[2]] <- cbind(test, networkMatrix[[2]])
  networkMatrix[[2]] <-
    networkMatrix[[2]][networkMatrix[[2]][, "test"] == "FALSE", ] #### only keeps the rows that do not contain .pdf
  networkMatrix[[2]] <-
    networkMatrix[[2]] [, 3:9] #### Rookie way of Matrix cleaning (deleting the test and namefirstcolumn thingies)
  if (longMessages == TRUE) {
    utils::setTxtProgressBar(pb, 4)
  }
  nrowMatrix <- round((nrow(networkMatrix[[2]]) * keep))
  
  # Use networkMatrix[[2]] to filter sna, only keeping the words that are the most central
  
  testvector <- colnames(sna) %in% rownames(networkMatrix[[2]])
  networkMatrix[[3]] <-
    sna[, testvector == TRUE] ### this is the reduced incidence matrix based on keep
  if (longMessages == TRUE) {
    utils::setTxtProgressBar(pb, 5)
  }
  
  utils::write.csv(file = "reduced_incidence_matrix.csv", x = networkMatrix[[3]])
  if (longMessages == TRUE) {
    utils::setTxtProgressBar(pb, 6)
    close(pb)
  }
  
  # then let it run in gephi, do all the algorithms that didn't work
  
  ##### Global Measures
  if (longMessages == TRUE) {
    Sys.sleep(1)
    cat("Calculating global measures...\n")
    Sys.sleep(1)
    
    pb <- utils::txtProgressBar(min = 1,
                                max = 2,
                                style = 3)
  }
  
  meanDistance <-
    igraph::mean_distance(snaIncidence, directed = FALSE)
  if (longMessages == TRUE) {
    utils::setTxtProgressBar(pb, 1)
  }
  
  networkMatrix[[4]] <- cbind(meanDistance)
  if (longMessages == TRUE) {
    utils::setTxtProgressBar(pb, 2)
    close(pb)
    
    Sys.sleep(1)
    cat("Done.\n")
    Sys.sleep(1)
  }
  cat(
    "Find the .csv-file reduced based on centrality in your working directory for further use.\n"
  )
  
  if (saveToWd == TRUE) {
    modeledNetworkFile <-
      paste0("modeledNetwork", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"))
    saveRDS(networkMatrix, file = modeledNetworkFile)
    
    if (ordinationFunction == FALSE) {
      cat(
        paste0(
          "\nThe complete modeled Network Data is now in your global environment. It is also saved as a file in your working directory. If you work with the same data again, you can skip this step in future analysis by reading in the file:\nmodeledNetwork <- readRDS(file= '",
          modeledNetworkFile,
          "')\n\n"
        )
      )
    } else {
      cat(
        paste0(
          "Network Data saved. You can read it in using:\nmodeledNetwork <- readRDS(file= '",
          modeledNetworkFile,
          "')\n###################################################################################################\n\n"
        )
      )
      
    }
  }
  names(networkMatrix) <-
    c(
      "LocalMeasures",
      "ReducedLocalMeasures",
      "ReducedIncidenceMatrix",
      "GlobalMeasures"
    )
  return(networkMatrix)
}



########################################################################
# Inspecting the results of the ginko package
########################################################################

#' @title inspectGinko
#'
#' @description placeholder
#' @param modeledData placeholder
#' @family ginko functions
#' @seealso \code{\link{calculateModels}} or \code{\link{calculateNetwork}}
#'     for the preceding step,
#'     \code{\link{createOrdinationPlot}} for the graphics,
#'     \code{\link{mostImportantPaperPerCluster}}
#'
#' @author Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de}
#' @return placeholder
#' @export

inspectGinko <- function(modeledData = modeledData) {
  paperCluster <-
    cbind(modeledData$MetaMatrix[, "Cluster"], modeledData$MetaMatrix[, "FileName"])
  colnames(paperCluster) <- c("Cluster", "FileName")
  
  OriginalPapers <- modeledData$MetaMatrix[, "FileName"]
  excludedPapers <-
    setdiff(trimws(OriginalPapers), trimws(paperCluster[, 2]))
  
  cat(
    paste0(
      "Summary of the ginko analysis\n\nTotal papers: ",
      length(OriginalPapers),
      ", processed papers: ",
      length(paperCluster[, 2]),
      ", excluded papers: ",
      length(excludedPapers),
      ",\nTotal words: ",
      length(modeledData$numberOfWords),
      ", words that are in 5% of all papers that have been used for the analysis: ",
      length(levels(
        modeledData[[1]]$`names(indSpeciesValues$pval)`
      )),
      # that is a very weird way to extract them.
      "\n\nPapers per Cluster:\n"
    )
  )
  
  NumberOfClusters <- max(paperCluster[, 1])
  
  for (i in 1:NumberOfClusters) {
    cat(paste0("Cluster ", i, " with ", sum(paperCluster[, 1] == i), " papers\n"))
  }
  
  
  GinkoSpecs <- list()
  
  # Which papers are in which cluster?
  GinkoSpecs[[1]] <- paperCluster
  
  cat(
    "\nThe following additional specs are available:\n- Paper-Cluster-Table: Each paper belongs to one cluster. Use View(GinkoSpecs$paperCluster) to see which paper belongs to which cluster.\n"
  )
  
  GinkoSpecs[[2]] <- modeledData$IndVal
  cat(
    "- Words-Cluster-Table: Words do not belong to one single cluster. An indicator species analysis shows how representative each word is for each cluster. Use View(GinkoSpecs$IndVal) to view the results of the indicator species analysis.\n"
  )
  
  # So which papers have been excluded in step 2? (step 1 will come)
  GinkoSpecs[[3]] <- excludedPapers
  
  cat(
    "- excluded Papers: use View(GinkoSpecs$excludedPapers) to see which papers have been excluded, possibly because of a PDF error.\n"
  )
  
  GinkoSpecs[[4]] <- modeledData$RepresentativePapers
  cat(
    "- most representative Papers: use View(GinkoSpecs$representativePapers) to see which papers are the most representative ones, weighted with the indicator species values of the words in the paper.\n"
  )
  
  GinkoSpecs[[5]] <-
    modeledData$MetaMatrix[, -which(colnames(modeledData$MetaMatrix) == "FullText")]
  # excluding with - doesn't work with ""
  cat(
    "- Matrix with metadata of the papers: use View(GinkoSpecs$MetaMatrix) to view the original MetaMatrix (without full texts, so it's safe to open)."
  )
  
  names(GinkoSpecs) <-
    c("paperCluster",
      "IndVal",
      "excludedPapers",
      "representativePapers",
      "MetaMatrix")
  
  
  return(GinkoSpecs)
}
