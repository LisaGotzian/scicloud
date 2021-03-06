# The sixth function to the word analysis with scicloud. It takes
#     \code{scicloudAnalysis} and outputs a list of the most cited papers per
#     cluster into the console.

mostImportantPaperPerCluster <- function(scicloudAnalysis) {
  numberOfClusters <-
    nrow(as.data.frame(table(scicloudAnalysis$metaMatrix[, "Cluster"]))) # get the number of clusters from the provided data

  for (i in 1:numberOfClusters) {
    paperPerCluster <- 5

    newSubset <-
      subset(scicloudAnalysis$metaMatrix, scicloudAnalysis$metaMatrix[, "Cluster"] == i)

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
    } else {
      # Order Subset by two levels. Main filter is citesPerYear and secondary is overall citations ==> if the citesPerYear are the same, younger is better
      newSubset2 <-
        newSubset[order(as.numeric(newSubset[, "CitedBy"]), decreasing = T), ]
      orderedSubset <-
        newSubset2[order(as.numeric(newSubset2[, "CitationPerYear"]),
          decreasing =
            T
        )[c(1:paperPerCluster)], ]




      paperNames <- c()
      CitedBy <- as.vector(unlist(orderedSubset[, "CitedBy"]))
      CitationPerYear <-
        as.vector(unlist(orderedSubset[, "CitationPerYear"]))


      for (j in 1:paperPerCluster) {
        Title <- as.vector(unlist(orderedSubset[j, "Title"]))
        Year <- as.vector(unlist(orderedSubset[j, "Year"]))
        Volume <- as.vector(unlist(orderedSubset[j, "Volume"]))
        Issue <- as.vector(unlist(orderedSubset[j, "Issue"]))
        Journal <- as.vector(unlist(orderedSubset[j, "Journal"]))
        Pages <- as.vector(unlist(orderedSubset[j, "Pages"]))
        DOI <- as.vector(unlist(orderedSubset[j, "DOI"]))
        Authors <-
          as.vector(unlist(strsplit(orderedSubset[j, "Authors"], ", ")))
        Authors <- if (length(Authors) == 1) {
          Authors
        } else if (length(Authors) == 2) {
          paste(Authors[1], "and", Authors[2])
        } else if (length(Authors) > 2) {
          paste(Authors[1], "et al.")
        } else {
          NA
        }

        paperName <-
          paste0(
            Authors,
            " (",
            Year,
            '). "',
            Title,
            '" In: ',
            Journal,
            " ",
            if (!is.na(Volume)) {
              Volume
            } else {
              ""
            },
            if (!is.na(Issue)) {
              "."
            } else {
              ""
            },
            if (!is.na(Issue)) {
              Issue
            } else {
              ""
            },
            if (!is.na(Pages)) {
              ", pp."
            } else {
              ""
            },
            if (!is.na(Pages)) {
              Pages
            } else {
              ""
            },
            if (!is.na(DOI)) {
              ". DOI: "
            } else {
              ""
            },
            if (!is.na(DOI)) {
              DOI
            } else {
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
