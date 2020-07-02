############ Using the scicloud package #############
# June 2020, Lisa Gotzian, Jia Yan Ng, Johann Julius Beeck

#################### Prerequesites #######################
# Welcome to this guide, please make sure you are all set:
# - a folder "PDFs" within your working directory that contains all PDFs
# - To get Scopus MetaData, please go to Elsevier and get your API key, it is connected to your
#   mail address. Then you can fill the big MetaMatrix based on the DOI. https://id.elsevier.com/as/yv1lr/resume/as/authorization.ping

myAPIKey <- "3c7cc08b398980881eee1050d53c5e86"

library(devtools)
install_github("LisaGotzian/scicloud")
library(scicloud)

metaMatrix <- createTextMatrixFromPDF(saveToWd = TRUE)
# The following is the "Let your computer work for 20 min" way. It does exactly the same as the lines below.
# You can also add all arguments I introduced below.

scicloudAnalysis <- ordinationCluster(metaMatrix, myAPIKey = myAPIKey,
                                   stemWords = TRUE, numberOfClusters = 4,
                                   longMessages = TRUE, saveToWd = TRUE, method = "hclust")
# you can also access the network using modeledData$LocalMeasures
# other possible methods: "hclust" or "network"

# Insights into Ginko
GinkoSpecs <- inspectGinko(modeledData = GinkoAnalysis)


# Only for Henrik: getting a wordlist + feeding it in again
scicloudAnalysis <- ordinationCluster(metaMatrix, 
                                      generateWordlist = TRUE,
                                      stemWords = TRUE, numberOfClusters = 4)
scicloudAnalysis <- ordinationCluster(metaMatrix,
                                      keepWordsFile = "Food_SLR.csv",
                                      stemWords = TRUE, numberOfClusters = 4)


################## Step by step ##################
# This is the "I want to do it step by step" way. Does exactly the same as the one function above.
processedMetaMatrix <- processMetaDataMatrix(metaMatrix, list(language = "SMART", stemWords = TRUE,
                                                              saveToWd = FALSE, ordinationFunction = FALSE)
                                             #, keepWordsFile = "Food_SLR.csv"
                                             ,ignoreWords = c("Abstract", "Bulletin", "Editor"))


processedMetaMatrix$MetaMatrix <- getScopusMetaData(processedMetaMatrix$MetaMatrix, myAPIKey)
modeledData <- calculateModels(processedMetaMatrix, longMessages = TRUE, numberOfClusters = 4)

################# The network approach ##################
# $LocalMeasures will return the local measurements for both papers and words
# $ReducedLocalMeasures will return 1/3 of the words (!) with their centrality measures & clustering according
# to three different clustering methods, arranged by eigenvector centrality (can be changed)
# $ReducedIncidenceMatrix will return 1/3 of the words arranged by eigenvector centrality,
# to be further processed eg in Gephi or with other clustering functions
# $GlobalMeasures will return the global measurements
modeledNetwork <- calculateNetwork(processedMetaMatrix, sortby = "Eigenvector", keep = 0.3)

######################### The graphics #########################
createOrdinationPlot(modeledData) # only works if you used your API key

mostImportantPaperPerCluster(modeledData)

# Insights into Ginko
GinkoSpecs <- inspectGinko(modeledData = modeledData)


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


