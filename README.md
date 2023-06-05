# Sneaky Researchers
An interactive tool to highlight the impact of hidden or arbitrary researcher decisions on study results

## Launching the App

Launch R Console and run the following two lines
```
library(shiny)
shiny::runGitHub("sneaky_researchers","kunzhi-yu")
```

## Dataset

*  Data on the adoption and use of digital technologies and the online behaviours of individuals 15 years of age and older living in the ten provinces of Canada conducted by Statistics Canada in 2020.
*  The [public use microdata file (PUMF)](https://www150.statcan.gc.ca/n1/pub/56m0003x/56m0003x2020001-eng.htm) from the Canadian Internet Use Survey (CIUS).
*  You can download the data from the link above
   *  Move the data into the data directory.
   * The data is in SAS format after you download it. To convert prepare the data, run [data_cleaning.R](data_cleaning.R). 
   * You do not need to download and clean the data for the app to run, only if you wish to edit the app  or data.
*  The data dictionary is [data/CIUS 2020_PUMF_CdBk.pdf](https://github.com/kunzhi-yu/sneaky_researchers/blob/main/data/CIUS%202020_PUMF_CdBk.pdf). You can find more details on the Statistics Canada link above.


## Editing the Project

*  Clone the repo
*  Open app.R
   *  You will be prompted to install packages
*  Click `Run App`
*  R Shiny project with renv
   *  `renv` provides project-local R dependency and environment management. All packages are isolated from existing R packages.
   *  https://cran.r-project.org/web/packages/renv/vignettes/renv.html
   *  Improves reproducibility because it tells others exactly what environment was used.
   *  You know it's good cause Rohan recommends it!
