############ Using the scicloud package #############
# June 2020, Lisa Gotzian, Jia Yan Ng, Johann Julius Beeck

#################### Prerequesites #######################
# Welcome to this guide, please make sure you are all set:
# - a folder "PDFs" within your working directory that contains all PDFs
# - To get Scopus MetaData, please go to Elsevier and get your API key, it is connected to your
#   mail address. Then you can fill the big MetaMatrix based on the DOI. https://dev.elsevier.com/index.jsp 

myAPIKey <- "3c7cc08b398980881eee1050d53c5e86"

library(devtools)
install_github("LisaGotzian/scicloud")
library(scicloud)

# Step 1: create ScipusList which returns a meta List which contains a WordList,
#       a tf-idf matrix, a metaMatrix 
scicloudList <- createScicloudList(myAPIKey = myAPIKey)
scicloudList <- createScicloudList(myAPIKey = myAPIKey, keepWordsFile = "keepWord.csv")

# Step 2: run the Analysis on the ScipusList created and generate plots that
#       illustrate the results of the modeling 
scicloudAnalysis <- runAnalysis(scicloudList = scicloudList, numberOfClusters = 4)

# Step 3: generate a summary of the analysis
scicloudSpecs <- inspectScicloud(scicloudAnalysis)

### The normal workflow of scicloud
metaMatrix <- createTextMatrixFromPDF()
scicloudAnalysis <- ordinationCluster(metaMatrix, myAPIKey = myAPIKey,
                                   stemWords = TRUE, numberOfClusters = 4)

# Insights into scicloud
scicloudSpecs <- inspectScicloud(scicloudAnalysis)


# Only for Henrik: getting a wordlist + feeding it in again
scicloudAnalysis <- ordinationCluster(metaMatrix, 
                                      generateWordlist = TRUE,
                                      stemWords = TRUE, numberOfClusters = 4)
scicloudAnalysis <- ordinationCluster(metaMatrix,
                                      keepWordsFile = "Food_SLR.csv",
                                      stemWords = TRUE, numberOfClusters = 4)
scicloudSpecs <- inspectScicloud(scicloudAnalysis)


################## Step by step ##################
# 0) create a text matrix from your PDF files
metaMatrix <- createTextMatrixFromPDF()

# 1) pull article metadata from Scopus
metaMatrix <- getScopusMetaData(metaMatrix, myAPIKey)

# 2) process the full texts
processedMetaDataMatrix <- processMetaDataMatrix(metaMatrix,
                                  list(language = "SMART",
                                  stemWords = TRUE,
                                  saveToWd = FALSE),
                                  ignoreWords = c("Abstract", "Bulletin", "Editor"))
                                   
# 3) run the cluster analysis to determine publication communities
scicloudAnalysis <- calculateModels(processedMetaDataMatrix, numberOfClusters = 4)
 
# 4) visualize the results
createOrdinationPlot(scicloudAnalysis)
 
# 5) a list of the most important papers per cluster
mostImportantPaperPerCluster(scicloudAnalysis)
 
# 6) a summary of the analysis
scicloudSpecs <- inspectScicloud(scicloudAnalysis)

 
 
 
 
################# The network approach ##################
# $LocalMeasures will return the local measurements for both papers and words
# $ReducedLocalMeasures will return 1/3 of the words (!) with their centrality measures & clustering according
# to three different clustering methods, arranged by eigenvector centrality (can be changed)
# $ReducedIncidenceMatrix will return 1/3 of the words arranged by eigenvector centrality,
# to be further processed eg in Gephi or with other clustering functions
# $GlobalMeasures will return the global measurements
modeledNetwork <- calculateNetwork(processedMetaDataMatrix, sortby = "Eigenvector", keep = 0.3)

#----------------- Search Scopus by Abstracts ----------------
DOInumbers <- searchScopus(searchString = "sustain", myAPIKey = myAPIKey)
DOInumbersMetaData <- getScopusMetaData(DOInumbers, myAPIKey = myAPIKey)

#-------------------- SUPPORT: Got Problems? -----------------
# If there is papers that didn't get read in, you can fill the DOIs in by hand.
# This is important in the later process. If your papers don't have a DOI, let it be,
# but if they do, you should fill them in.
MatrixToWork <- metaMatrix
fix(MatrixToWork) # here, you can fix your matrix make sure to have DOIs everywhere. Be careful, there's no ctrl+Z.
metaMatrix <- MatrixToWork # this assigns the matrix back to our original matrix.


