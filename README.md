# UK-census-project

This is a project developed for the EDX course HarvardX: PH125.9x Data Science: Capstone, machine learning predictions using the UK census data from Nomis, using R and RStudio.

The project is inspired by the UCI machine learning repository, to predict ages based on census data, here https://archive.ics.uci.edu/ml/datasets/Adult, and also the recent Black Lives Matter movements and recognition of the under-representation of certain groups in senior positions within society.

I will be using the UK 2011 census data.  This data does not have individual information, instead numbers within a geographical area, and in addition, it does not include pay, but does include occupation.

The most recent census in the UK is in 2011.  There are various sources and I used the information on the Office for National Statistics (ONS) as a starting point, here https://www.ons.gov.uk/census/2011census/2011censusdata/2011censususerguide/variablesandclassifications.

This in turn provided a link to the Nomis web site, provided buy Durham University on behalf of the ONS, as the official labour market statistics.  The following link provides a list of the tables from the 2011 UK census  https://www.nomisweb.co.uk/census/2011/data_finder 

The structure for the project is as follows:

* main file with R code 
++ ~/ph125.9x_CPY_UKcensus_vx.x.R 
* report file in R Markdown format, .RMD
+ ~/ph125.9x_CPY_UKcensus_vx.x.Rmd
* directory with raw data downloded from nomis (the code can do this directly)
+ ~/data/*.csv
* directory with the R object generated during the work (the code can generate these)
+ ~/rda/*.rda
