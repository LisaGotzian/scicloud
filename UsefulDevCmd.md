# Snippet of R package development steps 

## Typical Development Workflow
*With repeating steps: in any order that you see fit*
1. Write some code 
+ To **create new** or to **open the existing .R file**, we can use the command: **use_r("function_name")**
+ When you write codes that depend on another package, we need to imports it in the *DESCRIPTION* file, we can use the following command: **use_package("package_name")** 
2. Restart R Session (Cmd+Shift+F10/ Ctrl+Shift+F10 for Windows)
3. Build and Reload (Cmd+Shift+B /Ctrl+Shift+B for Windows)
4. Check Package (Cmd+Shift+E/Ctrl+Shift+E for Windows)
5. Document Package (if any) - (Cmd+Shift+D/Ctrl+Shift+D for Windows). If you have written a new function, you need to use `#' @export` in the documentation of your function
+ We can **document the usage of our function/provide examples to the users** by using **Vignettes**. You can find an example document under ./scientific-wordcloud/vignettes folder. The documents are in R markdown format which can be rendered into html with "knitr". We can create the .Rmd file under vignettes folder directly with the use of **use_vignette("file_name")**

## Before every commit
We can check if we break anything by running the following commands before committing our updates to github:
1. Restart R Session (Cmd+Shift+F10 /Ctrl+Shift+F10 for Windows)
2. Document Package (if any) - (Cmd+Shift+D /Ctrl+Shift+D for Windows)
3. **Check Package (Cmd+Shift+E /Ctrl+Shift+E for Windows)** -> **IMPORTANT step!** to ensure there is no error found in building the package! 

## Version Control 
We can update the version number with the use of **use_version()**. There will be a menu prompt where we can select whether it is a major/minor/path/dev version update

## Other useful commands
+ When you add a file that is not part of the package in the project, you need to add the file in .Rbuildignore so that the file is excluded during the package build -> use_build_ignore(files, escape = TRUE)
+ When you want to document more details about the the commit/version update -> use_news_md()
+ In order to remove warnings/messages you can use suppressWarnings(expr) and suppressMessages(expr) respectively
+ In order to avoid a crash of the program due to errors and to rather store those you can use try(expr, silent = TRUE)) or the more sophisticated function tryCatch(expr, ..., finally)
+ More sophisticated error handling based on [ArgumentCheck](https://cran.r-project.org/web/packages/ArgumentCheck/vignettes/ArgumentChecking.html): `ArgumentCheck` allows to collect all errors made instead of stopping after the first encountered error. This gives a more complete picture of errors to the user. A sample process is documented in the beginning of `2_processMetaDataMatrix.R`.
+ Update Documentation .Rd files -> Run devtools::document() (or press Ctrl/Cmd + Shift + D in RStudio) to convert roxygen comments to .Rd files. (devtools::document() calls roxygen2::roxygenise() to do the hard work.)
+ when we want the function to be accessible by the user, add #’@export to the top of the function and run `devtools::document()` to invoke roxgen to update `Namespace` automatically  

## Writing test cases to check functionality/computation 

+ We use testthat unit testing package to writing our test cases
+ to create a test file under the test/testthat directory, just run `usethis::use_test("metaMatrix")` 
  + A test is created with `test_that()`, it groups together multiple (or only one) expectation(s) to test the output from a function
  + expectation describes the expected result of a function call/computation e.g. does it have the right class or value?
    + expectations are functions that start with `expect_` e.g. `expect_equal`, `expect_true` , `expect_match`, etc. 

+ to run all test files under the test directory: `test_dir("./tests/testthat", reporter = "summary”)` or just run one test file: `test_file("./tests/testthat/test-metaMatrix.R")`

# Testing the package once it's done
This protocol is supposed to give guidance when we later check the package against a number of pdfs.

## Step 1: Checking if pdfs got read in correctly
* we have 2 stacks of PDFs: both shouldn't ping back error
* test set: feel free to come up with your own set of PDFs to test the functionality
* do all PDFs have DOIs? Why not?
* do we have any duplicates?

## Step 2: Checking if the words got read in correctly
The following cases might cause problems when constructing the tf-idf matrix:
* dual-lingual papers
* papers with two columns of text (are the paragraphs mixed up, leading to paragraphs being thrown out when throwing out the references)
* how are equations read in?
* for unit testing: The current functionality yields RDS object xy, do future changes yield the same object?

-> see above the section about unit testing :-)

## Step 3: Check with different computers
* make sure Windows & Mac at different versions give the same result

## Step 4: Check documentation
* have the following components in each help file:
  * @title
  * @description
  * @author: Henrik is author on the top level, we are creators -> make sure everybody is mentioned where they worked and make sure we are using email addresses that will work in 2 years from now on
  * @param: params that are present in more than one function need to be consistent
  * @family: ! I want it present everywhere
  * @return
  * @seealso: the preceeding and proceeding workflow step
  * @export: sometimes this is not present
  * @example: a full ginko_explained script specifically made for this function, so eg calculateNetwork has a slightly different one than calculateModels
* general stuff to check
  * are the names consistent? Example: "metaMatrix" vs "processMetaDataMatrix" is not consistent, that's why I'd decide for one version
  * links to functions and to webpages within each help file work
  * nowhere "ginko"

[Reference for package development](https://www.hvitfeldt.me/blog/usethis-workflow-for-package-development/) and by [Hadley Wickham](http://r-pkgs.had.co.nz/)
