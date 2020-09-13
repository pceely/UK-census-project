# UK-census-project

This is a project developed for the EDX course HarvardX: PH125.9x Data Science: Capstone, machine learning predictions using the UK census data from Nomis, using R and RStudio.

The project is inspired by the UCI machine learning repository, to predict ages based on census data, here https://archive.ics.uci.edu/ml/datasets/Adult, and also the recent Black Lives Matter movements and recognition of the under-representation of certain groups in senior positions within society.

The project uses data from the most recent census in the UK in 2011.  This data does not have individual information, instead numbers within a geographical area, and in addition, it does not include pay, but does include occupation.

Information about the census is available from the Office for National Statistics (ONS) a, here https://www.ons.gov.uk/census/2011census/2011censusdata/2011censususerguide/variablesandclassifications.  The data was downloaded from the Nomis web site, provided buy Durham University on behalf of the ONS, as the official labour market statistics.  The following link provides a list of the tables from the 2011 UK census  https://www.nomisweb.co.uk/census/2011/data_finder 

The structure for the project is as follows:

* main file with R code 
  * ~/ph125.9x_CPY_UKcensus_vx.x.R 
* report file in R Markdown format, .RMD
  * ~/ph125.9x_CPY_UKcensus_vx.x.Rmd
* directory with raw data downloded from nomis (the code can do this directly)
  * ~/data/*.csv
* directory with the R object generated during the work (the code can generate these)
  * ~/rda/*.rda


## Instructions

The R code when run will create subdirectories of the location directory of the script, storing raw downloaded data in ~/data and r objects in ~/rda.  It will also install the packages necessary for the code to run.

It will take around 17 hours to run in total, downloading files from the internet, storing locally, and storing the r data files locally, using around 1.9GB of space for around 150 files.

Once the R script is run, and the files stored locally, the Rmd can be run from the same directory, using the r objects to generate the plots and data.

The code was tested and run on the following set up:

* RStudio Version 1.3.1073
* platform       x86_64-apple-darwin17.0     
* svn rev        78730                       
* language       R                           
* version.string R version 4.0.2 (2020-06-22)
* with the latest packages installed

