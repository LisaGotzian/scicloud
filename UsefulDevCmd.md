# Snippet of R package development steps 

## Typical Development Workflow
*With repeating steps: in any order that you see fit*
1. Write some code 
+ To **create new** or to **open the existing .R file**, we can use the command: **use_r("function_name")**
+ When you write codes that depend on another package, we need to imports it in the *DESCRIPTION* file, we can use the following command: **use_package("package_name")** 
2. Restart R Session (Cmd+Shift+F10/ Ctrl+Shift+F10 for Windows)
3. Build and Reload (Cmd+Shift+B /Ctrl+Shift+B for Windows)
4. Check Package (Cmd+Shift+E/Ctrl+Shift+E for Windows)
5. Document Package (if any) - (Cmd+Shift+D/Ctrl+Shift+D for Windows) 
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

[Reference](https://www.hvitfeldt.me/blog/usethis-workflow-for-package-development/)
