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
            "PDF File caused an error while retrieving the full text:",
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
        paste(intermediateResultText, collapse = " ")  # takes the vector and pastes it
      # into a single element, seperated by a " "
      
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
      cbind(PDFcontent, "ID" = 1:nrow(PDFcontent)) # assiging a unique id to avoid
    # collision along the way
    
    DOIpattern <-
      '\\b(10[.][0-9]{4,}(?:[.][0-9]+)*/(?:(?!["&\'<>])[[:graph:]])+)\\b'
    DOInumbers <-
      stringr::str_extract(PDFcontent[, "FullText"], DOIpattern)
    PDFcontent[, "DOI"] <- DOInumbers
    
    # this filters double DOI entries in the PDFcontent
    # the perfect similarity of entries has a huge effect on the models later on in the process
    PDFcontent <-
      subset(PDFcontent,!duplicated(PDFcontent[, "DOI"], incomparables = NA))
    
    close(pb)
    
    if (saveToWd == TRUE) {
      MetaMatrixFile <-
        paste0("metaMatrix", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"))
      saveRDS(PDFcontent, file = MetaMatrixFile)
      
      cat(
        paste0(
          "\nThe metaMatrix is now in your global environment.
          It is also saved as a file in your working directory.
          If you work with the same data again, you can skip
          this step in future analysis by reading in the file:\nmetaMatrix <- readRDS(file= '",
          MetaMatrixFile,
          "')\n\n"
        )
      )
    }
    
    return(PDFcontent)
  }
