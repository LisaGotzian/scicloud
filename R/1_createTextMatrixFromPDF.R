########################################################################
# Workflow for folders containing PDF files
# generates a matrix of name in col1 and text in row2 like the scopus function
########################################################################

#' @title createTextMatrixFromPDF
#'
#' @description First function of the word analysis with scicloud. It takes all
#'     scientific papers as PDF files from the "PDFs" folder in your working
#'     directory. It then creates a DocumentTerm matrix of it.
#'
#' @param directory per default, the PDFs are expected to be in a folder named
#'     "PDFs", can be changed ad. lib.
#' @param saveToWd  a logical parameter whether or not to save the output of the
#'     function to the working directory. This is especially useful for later
#'     analysis steps. The file can be read in by using \code{\link[base]{readRDS}}.
#' @family scicloud functions
#' @seealso \itemize{
#'     \item \code{\link{ordinationCluster}} for the next step in scicloud
#'     \item or \code{\link{processMetaDataMatrix}} for the
#'     more granular next step if you intend to run it step by step
#'     \item \code{\link{getScopusMetaData}} to fill in paper metadata (needed for
#'     future plots) from Scopus
#'     }
#'
#' @author Matthias Nachtmann, \email{matthias.nachtmann@@stud.leuphana.de},
#'     Lisa Gotzian, \email{lisa.gotzian@@stud.leuphana.de}
#' @return A data frame containing the file name and full text of the pdf, 
#'     the DOI numbers from the text and some empty metadata columns to be
#'     filled by \code{\link{getScopusMetaData}}.
#'     It is analogous to what \code{\link{searchScopus}}
#'     returns.
#' @examples
#' \dontrun{
#' 
#' ### The normal workflow of scicloud
#' myAPIKey <- "YOUR_API_KEY"
#' metaMatrix <- createTextMatrixFromPDF()
#'
#' 
#' # run the analysis, see ordinationCluster()
#' # for more arguments
#' scicloudAnalysis <- ordinationCluster(metaMatrix,
#'                            myAPIKey = myAPIKey)
#'
#' # inspect the analysis
#' scicloudSpecs <- inspectScicloud(scicloudAnalysis)
#' }
#' @export
 
createTextMatrixFromPDF <-
  function(directory = file.path(".", "PDFs"),
           saveToWd = TRUE) {
    
    # filter out non PDF files
    PDFs_FileName <- Sys.glob(file.path(directory, "*.pdf"))
    
    # check for non-standard PDF names
    files_accessed <- file.access(PDFs_FileName)
    
    if (any(files_accessed == -1)) {
      message(paste0(abs(sum(files_accessed)),
      " PDF(s) cannot be accessed.",
      "\n",
      "Please check following PDF(s):"))
      PDFs_wrongname <- sub(".*[/]", "", PDFs_FileName[which(files_accessed == -1)])
      print(PDFs_wrongname)
      stop("Process stopped.")
    }

    # Argument Checks
    Check <- ArgumentCheck::newArgCheck()
    if (!length(PDFs_FileName)){
      ArgumentCheck::addError(
        msg = "The directory contains no PDF file(s)",
        argcheck = Check
      )
    }
    isdir <- file.info(directory)[["isdir"]]
    if (any(is.na(isdir))){
      ArgumentCheck::addError(
        msg ="non-existent directory", 
        argcheck = Check                      
      )
    }
    if (!(isdir)){
      ArgumentCheck::addError(
        msg ="'directory' argument is not a directory path", 
        argcheck = Check                      
      )
    }
    ArgumentCheck::finishArgCheck(Check)
    
    PDFcontent <- matrix(NA, nrow = length(PDFs_FileName), ncol = 20)
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
    
    num_pdf = length(PDFs_FileName)
    # set min = 0 to cater the use case when only read 1 PDF file, max must be > min
    pb <- utils::txtProgressBar(min = 0, max = num_pdf, style = 3) 
    
    for (i in c(1:num_pdf)) {
      intermediateResultFileName <- PDFs_FileName[i]
      
      intermediateResultText <- suppressMessages(pdftools::pdf_text(PDFs_FileName[i]))
      intermediateResultText <- as.character(intermediateResultText)
      intermediateResultText <-
        paste(intermediateResultText, collapse = " ")  # takes the vector and pastes it
      # into a single element, seperated by a " "
      
      # retrieve the filename
      PDFcontent[i, "FileName"] <-
        if (length(intermediateResultFileName) > 0) {
          intermediateResultFileName
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
    
    # Only retrieve the first two pages of the PDFs
    firstTwoPage <- c()
    for(i in PDFs_FileName){
      # concatenate the two vectors of string (each two pages) retrieve from pdf_text(i)[0:2]  
      firstTwoPage <- append(firstTwoPage, paste(suppressMessages(pdftools::pdf_text(i)[0:2]), collapse = ' '))
    }
    DOInumbers <- stringr::str_extract(firstTwoPage, DOIpattern)
    PDFcontent[, "DOI"] <- DOInumbers
    
    # this filters double DOI entries in the PDFcontent
    # the perfect similarity of entries has a huge effect on the models later on in the process
    PDFcontent <-
      subset(PDFcontent,!duplicated(PDFcontent[, "DOI"], incomparables = NA))
    
    close(pb)
    
    if (saveToWd == TRUE) {
      save_data(PDFcontent, "metaMatrix")
    }
    
    return(PDFcontent)
  }
