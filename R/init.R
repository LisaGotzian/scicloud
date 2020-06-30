########################################################################
# Package loading message
########################################################################

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to scicloud. Have a folder 'PDFs' within your working directory that contains all scientific papers you intend to use. Run ?scicloud to get started."
  )
}