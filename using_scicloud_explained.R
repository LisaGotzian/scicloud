############ Using the scicloud package #############
# June 2020, Lisa Gotzian, Jia Yan Ng, Johann Julius Beeck

#################### Prerequesites #######################
# Welcome to this guide, please make sure you are all set:
# - a folder "PDFs" within your working directory that contains all PDFs
# - To get Scopus MetaData, please go to Elsevier and get your API key, it is
#   connected to your mail address. Then you can fill the big MetaMatrix based on
#   the DOI. https://dev.elsevier.com/index.jsp

myAPIKey <- "3c7cc08b398980881eee1050d53c5e86"

library(devtools)
install_github("LisaGotzian/scicloud")
library(scicloud)

# Step 1: create ScicloudList which returns a meta List which contains a WordList,
#       a tf-idf matrix, a metaMatrix 
scicloudList <- createScicloudList(myAPIKey = myAPIKey)

# Step 1a: with the option of a manual wordlist
#scicloudList <- createScicloudList(myAPIKey = myAPIKey, generateWordlist = TRUE)
#scicloudList <- createScicloudList(myAPIKey = myAPIKey,
#                                   keepWordsFile = "scicloudWordlist.csv")

# Step 2: run the Analysis on the ScicloudList created and generate plots that
#       illustrate the results of the modeling 
scicloudAnalysis <- runAnalysis(scicloudList = scicloudList, numberOfClusters = 4)

# Step 2a: with the option of running a network analysis
scicloudAnalysis <- runAnalysis(scicloudList = scicloudList,
                                sortby = "Eigenvector", keep = 0.3)

# Step 3: generate a summary of the analysis
scicloudSpecs <- inspectScicloud(scicloudAnalysis)
 

#----------------- Search Scopus by Abstracts ----------------
DOInumbers <- searchScopus(searchString = "sustain", myAPIKey = myAPIKey)

#-------------------- SUPPORT: Got Problems? -----------------
# If there is papers that didn't get read in, you can fill the DOIs in by hand.
# This is important in the later process. If your papers don't have a DOI, let it be,
# but if they do, you should fill them in.
MatrixToWork <- metaMatrix
fix(MatrixToWork) # here, you can fix your matrix make sure to have DOIs everywhere. Be careful, there's no ctrl+Z.
metaMatrix <- MatrixToWork # this assigns the matrix back to our original matrix.


