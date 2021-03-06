# Filter files with specific extension in directory provided 
filter_file <- function(file_path, file_extension){
  if(!grepl("\\.", file_extension)){ # if "." is not included in file_extension, append one
      file_extension <- paste0(".", file_extension)
  }
  files <- list.files(path = file_path)
  return(matched_item(file_extension, files, select_match = TRUE))
}

# Save data in RDS format at current working directory  
save_data <- function(data, data_name){
  file_name <- paste0(data_name, format(Sys.time(), "%Y_%m_%d_%H_%M_%S"), ".RDS")
  saveRDS(data, file = file_name)
    cat(
      paste0(
        data_name, " is saved. You can read it using:\n", data_name, " <- readRDS(file= '",
        file_name,
        "')\n\nTo delete RDS files: run delete_RDS() to select different options from the menu.
        \n###################################################################################\n\n"
      )
    )  
}

#' @title Delete RDS files saved in working directory
#' 
#' @description Delete RDS files from current working directory. Users can
#'      choose 3 delete options from the menu as following:\cr
#'      Option 1: Delete All RDS files in cwd \cr
#'      Option 2: Keep the latest RDS files of each data type \cr
#'      Option 3: Keep only the selected data type of RDS files, delete all others \cr
#'      
#' @author Jia Yan Ng, \email{jia.y.ng@@stud.leuphana.de}
#' @family scicloud functions
#' @examples
#' \dontrun{
#' 
#' ## RDS files created from functions 
#' ## e.g. createTextMatrixFromPDF, calculateModels, etc.  
#' ## which located in the working directory can be 
#' ## deleted with this function by running as following:
#' 
#' deleteRDS()
#' Which RDS files to be deleted in current directory? 
#' 
#' 1: Delete All RDS files
#' 2: Keep the latest of each data, delete all others
#' 3: Keep only a specific data, delete all other type
#' 4: Quit
#' 
#' Selection: 1
#' Done. Deleted All RDS files.
#' }
#' @export

deleteRDS <- function(){
  files <- filter_file(getwd(), "RDS")
  if(length(files) == 0){
    stop("No RDS file(s) in the current working directory!")
  }
  choices <- c("scicloudList", "runAnalysis", "Quit")
  repeat{
    menu_selection = c("Delete All RDS files", 
                       "Keep the latest of each data, delete all others",
                       "Keep only a specific data, delete all other type", 
                       "Quit")
    pick <- utils::menu(menu_selection, title = "\nWhich RDS files to be deleted in current directory?")
    if(pick == 4 | pick ==0)
      return(invisible())
    else if(pick == 1){
      file.remove(files)
      cat("Done. Deleted All RDS files.")
      return(invisible())
    }
    else if(pick == 2){
      to_keep <- NULL
      for(i in 1:(length(choices)-1)){
        to_keep[i] <- latest_RDS(choices[i], files)
      }
      files_to_keep <- to_keep[to_keep != ""] # remove any empty entry 
      files_to_delete <- setdiff(files, files_to_keep)
      file.remove(files_to_delete)
      cat(paste0("Deleted ", files_to_delete, "\n"))
      cat(paste0("Keeping ", files_to_keep, "\n"))
      return(invisible())
    }
    else if(pick == 3){ # Keep only a specific data, delete all other type
      pick <- utils::menu(choices, title = "Which RDS data to keep?")
      if(pick == 1){
        to_delete <- matched_item("scicloudList", files)
        return(delete_files(to_delete))
      }
      else if(pick == 2){
        to_delete <- matched_item("runAnalysis", files)
        return(delete_files(to_delete))
      }
      else if(pick == 3 | pick == 0)
        return(invisible())
    }
    else stop("Invalid option")
  }
}

# Select items that match pattern specified from the match_list (select_match = TRUE)
# Select items that do not match pattern specified from the match_list (select_match = FALSE)
matched_item <- function(regex, match_list, select_match = FALSE){
  if(select_match)
    return(match_list[grepl(regex, match_list)])
  else
    return(match_list[!grepl(regex, match_list)])
}

# delete files specified from current working directory 
delete_files <- function(files_to_delete){
  if(length(files_to_delete) != 0){
    file.remove(files_to_delete)
    cat(paste0("Deleted ", files_to_delete, "\n"))
    return(invisible())
  }
  else stop(paste0("Only ", choices[pick], "_xx_xx.RDS found!"))
}

# find the RDS of specified data_type e.g.(metaMatrix) with the latest date
latest_RDS <- function(data_type, files){
  file_to_match <- matched_item(regex = data_type, match_list = files, select_match = TRUE)
  if(length(file_to_match) != 0){ # if return matched item is not empty 
    dates <- gsub(data_type, "", gsub("\\..*", "", file_to_match)) # extract all dates from file name
    date_to_match <- format(max(strptime(dates, "%Y_%m_%d_%H_%M_%S")), "%Y_%m_%d_%H_%M_%S")
    return (matched_item(regex = date_to_match, match_list = file_to_match, select_match = TRUE))
  }
  else return("")
}