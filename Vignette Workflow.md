## Vignette Workflow

**What is a Vignette?**
+ documentation that provides instructive tutorials to demonstrate informative examples & practical uses of the package 

**Writing vignettes** 
+ To create vignette, run `use_vignette("filename")`
  + this will create a **filename.Rmd** in the folder **vigenttes**
  + this draft vignette serves as a useful reference & remind us the important parts of an R Markdown file
    + top part is the YAML header, do not remove it! add *author* and *date: "\`r Sys.Date()`"*
    + remove `collapse = TRUE` so that the comment are indented as needed 

+ Modify the vignette accordingly 
  + change your Knit Directory (drop down menu -*with the triangle sign* from the interface `Knit`) to Project Directory!!! -> to ensure the users can follow the tutorial exactly! 
    + default directory is the document directory which is `./vigenettes`
  + must include `library(ginko)`
+ Press Ctrl/Cmd + Shift + K or click `Knit` to knit the vignette & preview the output 

+ if we want to run the code and display the output, we can write the code within a code chunk
  + insert chunk shortcut: `opt` + `cmd` + `i` for Mac - the code within this chunk will be executed when knitting the html

+ We can just show the code without runnig the code during knitting using the R markdown syntax to create a code block by enclosing the code with 6 single quotes - \``` Write code here```

+ For other details syntax: refer [this cheatsheet](https://rstudio.com/wp-content/uploads/2016/03/rmarkdown-cheatsheet-2.0.pdf)
