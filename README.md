# scientific-wordcloud
This R package analyzes large amounts of (scientific) text, usually provided pdf files or automatically downloaded abstracts from scopus.com. It analyzes them using indicator values and clustering or network analysis.

* `PDFs`includes a sample of 20 papers for testing purposes.
* `the package once its done/ginko` includes all files for the package and will be the only files we work with in the long run.
* `Methods_Julius.pdf` is a draft from one of Julius Rathgen's papers in which he describes how he used this package for his scientific analysis. The advantage of this draft is his overview from a scientific point of view while also giving insights into the package is supposed to work.

While researching the topic, I stumbled upon the following resources resolving around word analysis:
* [Wolfram Alpha's word analysis tool](https://reference.wolfram.com/language/guide/TextAnalysis.html?fbclid=IwAR01lCl9xT627zSyVMBpYitkZ9qLqQtLp3dMVgccdTB6qHNWsaKZXrEJcPU) that did pretty much anything a basic statistician would do with it (filtering and stop words, word count, clustering and posistion analysis)
* [paperscape](https://paperscape.org) did an analysis of all papers on arxiv based on who cites whom (that's pretty close to our network approach except for that we take the words into account)

## Internal organisation
We agreed to work with the following tools:
* [GitKraken/GitHub Desktop as described in this tutorial](https://www.youtube.com/watch?v=FNgHFFfI4YE&list=PLe6EXFvnTV78WqGmGSq8JPnafR3lAa55n&index=2) or [RStudio as explained here](https://happygitwithr.com/rstudio-git-github.html)
* the R package `devtools` to develop our package [as explained here](https://www.hvitfeldt.me/blog/usethis-workflow-for-package-development/)
