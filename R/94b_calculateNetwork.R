# This function is an alternative to the fourth function
#     \code{\link{calculateModels}} of the scicloud analysis. It uses
#     a network clustering approach.

calculateNetwork <- function(processedMetaDataMatrix,
                             sortby = c(
                               "Eigenvector", "Degree",
                               "Closeness", "Betweenness"
                             ),
                             keep = 0.33) {
  sortby <- match.arg(sortby) # pick the argument input by user

  # Argument Checks
  Check <- ArgumentCheck::newArgCheck()
  if (any(c(
    is.null(processedMetaDataMatrix[[1]]),
    is.null(processedMetaDataMatrix[[2]]),
    is.null(processedMetaDataMatrix[[3]])
  ))) {
    ArgumentCheck::addError(
      msg = "Invalid object! Use value from runAnalysis()",
      argcheck = Check
    )
  }

  # keep has to be numeric between 0 and 1
  if (any(c(!is.numeric(keep), keep > 1, keep < 0))) {
    ArgumentCheck::addError(
      msg = "'keep' argument value must be in between 0 and 1",
      argcheck = Check
    )
  }

  cat(
    "NETWORK ANALYSIS\n"
  )
  ###### Generating a network
  sna <- as.matrix(processedMetaDataMatrix$Tf_Idf)
  papers <- nrow(sna)
  sna.transposed <- t(sna)
  snaIncidence <-
    igraph::graph.incidence(sna.transposed, directed = FALSE)
  snaEdgelist <- igraph::as_edgelist(snaIncidence)


  # snaTnet <- as.tnet(snaEdgelist, type = "binary two-mode tnet")
  snaTnet <-
    bipartite::web2edges(sna.transposed,
      weight.column = FALSE,
      return = TRUE
    )


  ###### Calculating local measures
  Sys.sleep(1)
  cat("Calculating local measures...\n")
  Sys.sleep(1)


  pb <- utils::txtProgressBar(
    min = 1,
    max = 5,
    style = 3
  )

  # Degree
  nodeDegree <- igraph::degree(snaIncidence)
  utils::setTxtProgressBar(pb, 1)


  # Betweenness
  nodeBetweenness <- igraph::betweenness(snaIncidence)
  utils::setTxtProgressBar(pb, 2)

  # Closeness
  nodeCloseness <- igraph::closeness(snaIncidence)
  utils::setTxtProgressBar(pb, 3)


  # Eigenvector
  nodeEigenvector <- igraph::eigen_centrality(snaIncidence)$vector
  utils::setTxtProgressBar(pb, 4)


  networkMatrix <- list()
  networkMatrix[[1]] <-
    cbind(nodeDegree, nodeBetweenness, nodeCloseness, nodeEigenvector) # [[1]] will be the papers
  utils::setTxtProgressBar(pb, 5)

  close(pb)

  ######## Clustering
  Sys.sleep(1)
  cat("Clustering based on the network...\n")
  Sys.sleep(1)

  pb <- utils::txtProgressBar(
    min = 1,
    max = 6,
    style = 3
  )

  # mind the order
  utils::setTxtProgressBar(pb, 1)

  clusterWalktrap <-
    igraph::membership(igraph::cluster_walktrap(snaIncidence, steps = 10))


  clusterGreedy <-
    igraph::membership(igraph::fastgreedy.community(snaIncidence))
  utils::setTxtProgressBar(pb, 2)


  clusterInfomap <-
    igraph::membership(igraph::cluster_infomap(snaIncidence))
  utils::setTxtProgressBar(pb, 3)


  networkMatrix[[1]] <-
    cbind(
      networkMatrix[[1]],
      clusterWalktrap,
      clusterGreedy,
      clusterInfomap
    )

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
  utils::setTxtProgressBar(pb, 4)

  nrowMatrix <- round((nrow(networkMatrix[[2]]) * keep))

  # Use networkMatrix[[2]] to filter sna, only keeping the words that are the most central

  testvector <- colnames(sna) %in% rownames(networkMatrix[[2]])
  networkMatrix[[3]] <-
    sna[, testvector == TRUE] ### this is the reduced incidence matrix based on keep
  utils::setTxtProgressBar(pb, 5)


  utils::write.csv(file = "reduced_incidence_matrix.csv", x = networkMatrix[[3]])
  utils::setTxtProgressBar(pb, 6)
  close(pb)


  # then let it run in gephi, do all the algorithms that didn't work

  ##### Global Measures
  Sys.sleep(1)
  cat("Calculating global measures...\n")
  Sys.sleep(1)

  pb <- utils::txtProgressBar(
    min = 1,
    max = 2,
    style = 3
  )

  meanDistance <-
    igraph::mean_distance(snaIncidence, directed = FALSE)
  utils::setTxtProgressBar(pb, 1)


  networkMatrix[[4]] <- cbind(meanDistance)
  utils::setTxtProgressBar(pb, 2)
  close(pb)

  Sys.sleep(1)
  cat("Done.\n")
  Sys.sleep(1)

  cat(
    "Find the .csv-file reduced based on centrality in your working directory for further use.\n"
  )

  names(networkMatrix) <-
    c(
      "LocalMeasures",
      "ReducedLocalMeasures",
      "ReducedIncidenceMatrix",
      "GlobalMeasures"
    )
  return(networkMatrix)
}
