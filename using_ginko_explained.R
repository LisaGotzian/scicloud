############ Using the gingko package #############
# February 20, 2019, Lisa Gotzian

#################### Prerequesites #######################
# Welcome to this guide, please make sure you are all set:
# - you should have a folder "whatevernameyouchoose" where the following things are included:
# - a folder "PDFs" within this folder that contains all PDFs within your working directory
# - To get Scopus MetaData, please go to Elsevier and get your API key, it is connected to your
#   mail address. Then you can fill the big MetaMatrix based on the DOI. https://id.elsevier.com/as/yv1lr/resume/as/authorization.ping

myAPIKey <- "3c7cc08b398980881eee1050d53c5e86"

################# The ginko magic #####################
#library(ginko)
#install.packages("R.utils")
library(R.utils)
sourceDirectory("./R")

metaMatrix <- createTextMatrixFromPDF(saveToWd = TRUE) # Don't give up! Keep running it even if you get errors.
# The following is the "Let your computer work for 20 min" way. It does exactly the same as the lines below.
# You can also add all arguments I introduced below.
#metaMatrix <- readRDS(file= 'metaMatrix2019_________FILL_IN_HERE__________')
GinkoAnalysis <- ordinationCluster(metaMatrix, #myAPIKey = myAPIKey,
                                   stemWords = TRUE,
                                   longMessages = FALSE, saveToWd = TRUE, method = "hclust")
# you can also access the network using modeledData$LocalMeasures
# other possible methods: "hclust" or "network"

# Insights into Ginko
GinkoSpecs <- inspectGinko(modeledData = GinkoAnalysis)


################## Step by step ##################
# This is the "I want to do it step by step" way. Does exactly the same as the one function above :D
#metaMatrix <- readRDS(file= 'metaMatrix2019________FILL_IN_HERE_________')
processedMetaMatrix <- processMetaDataMatrix(metaMatrix, control= list(language = "SMART", stemWords = FALSE),
                                             #, keepWordsFile = "Food_SLR.csv"
                                             , ignoreWords = c("Abstract", "Bulletin", "Editor"))


processedMetaMatrix$MetaMatrix <- getScopusMetaData(processedMetaMatrix$MetaMatrix, myAPIKey)

############### Mopdelling the data #####################
# Here, you can specify certain variables like the number of clusters you want. If you don't
# specify them, it uses the following:
# language = "SMART",
# numberOfClusters = NA, # the significant amount unless you specify it
# minWordsPerCluster = 5,
# maxWordsPerCluster = 10,
# stemWords = TRUE,
# ignoreWords = c(),
# p = 0.05,
# dendrogram = TRUE, # fairly new
# dendroLabels = "truncated" # broken lines is possible as well with "break"

#processedMetaMatrix <- readRDS(file= 'processedData________FILL_IN_HERE_________')
modeledData <- calculateModels(processedMetaMatrix, longMessages = TRUE)

################# The network approach ##################
# $LocalMeasures will return the local measurements for both papers and words
# $ReducedLocalMeasures will return 1/3 of the words (!) with their centrality measures & clustering according
# to three different clustering methods, arranged by eigenvector centrality (can be changed)
# $ReducedIncidenceMatrix will return 1/3 of the words arranged by eigenvector centrality,
# to be further processed eg in Gephi or with other clustering functions
# $GlobalMeasures will return the global measurements
modeledNetwork <- calculateNetwork(processedMetaMatrix, sortby = "Eigenvector", keep = 0.3)
#modeledNetwork <- readRDS(file= 'modeledNetwork_______FILL_IN_HERE_______')

######################### The graphics #########################
#modeledData <- readRDS(file= 'modeledData_______FILL_IN_HERE________')
modeledData <- readRDS(file= 'modeledData2019_02_14_14_46_46')
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


