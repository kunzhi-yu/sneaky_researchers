# Sneaky Researchers
An interactive tool to highlight the impact of hidden or arbitrary researcher decisions on study results

## Editing the Project

*  R Shiny project with renv
   *  `renv` provides project-local R dependency and enviroment management. All packages are isolated from existing R packages.
   *  https://cran.r-project.org/web/packages/renv/vignettes/renv.html
   *  Improves reproduciblity because it tells others exactly what enviroment was used.
   *  You know it's good cause Rohan recommends it!

## Dataset

*  Data on the adoption and use of digital technologies and the online behaviors of individuals 15 years of age and older living in the ten provinces of Canada conducted by Statistics Canada in 2020.
*  The public use microdata file (PUMF) from the Canadian Internet Use Survey (CIUS)
*  https://www150.statcan.gc.ca/n1/pub/56m0003x/56m0003x2020001-eng.htm
*  The data is in SAS format after you download it. To convert prepare the data, run data_cleaning.R. 
   * You do not need to clean the data for the app to run, only if you wish to edit the app.
