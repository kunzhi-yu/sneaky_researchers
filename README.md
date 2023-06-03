# Sneaky Researchers
An interactive tool to highlight the impact of hidden or arbitrary researcher decisions on study results

## Launching the App
*  Clone the repo
*  Open app.R
   *  You will be prompted to install packages
*  Click `Run App`

## Dataset

*  Data on the adoption and use of digital technologies and the online behaviours of individuals 15 years of age and older living in the ten provinces of Canada conducted by Statistics Canada in 2020.
*  The public use microdata file (PUMF) from the Canadian Internet Use Survey (CIUS)
*  https://www150.statcan.gc.ca/n1/pub/56m0003x/56m0003x2020001-eng.htm
*  The data is in SAS format after you download it. To convert prepare the data, run [this file](data_cleaning.R). 
   * You do not need to clean the data for the app to run, only if you wish to edit the app  or data.
*  The data dictionary is [here](data/CIUS 2020_PUMF_CdBk.pdf)


## Editing the Project

*  R Shiny project with renv
   *  `renv` provides project-local R dependency and environment management. All packages are isolated from existing R packages.
   *  https://cran.r-project.org/web/packages/renv/vignettes/renv.html
   *  Improves reproducibility because it tells others exactly what environment was used.
   *  You know it's good cause Rohan recommends it!

