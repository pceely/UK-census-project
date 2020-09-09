#### ph125.9x Capstone choose your own project - UK census ####
# Paul Ceely
# 07/07/2020 to 30/08/2020
################## set up #####################
# this was developed and ran with RStudio:
# RStudio Version 1.3.1073
# platform       x86_64-apple-darwin17.0     
# svn rev        78730                       
# language       R                           
# version.string R version 4.0.2 (2020-06-22)
# with the latest packages installed
#################### initial set up ##########################
# setwd("~/Documents/study/Data-Science-R/ph125.9.Capstone/ChooseYourOwn")
# set the working directory as the current directory of the file
current_directory <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_directory)
# checking
getwd()
# create directories: raw data in ./data, R objects in ./rda
dir.create("data")
dir.create("rda")
# set up libraries
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('stringr')) install.packages('stringr'); library('stringr')
if (!require('rvest')) install.packages('rvest'); library('rvest')
if (!require('caret')) install.packages('caret'); library('caret')
if (!require('RColorBrewer')) install.packages('RColorBrewer'); library('RColorBrewer')
options(digits = 6)
# tidy
rm(current_directory)

############ Downloading the initial data #####################
# <select aria-labelledby="area-type-label" name="geography" id="geography" class="">
#  <option value="TYPE297">super output areas - middle layer 2011</option>
#  <option value="TYPE499">countries</option>
  # <option value="2013265926TYPE299">output areas 2011 in East</option>
  # <option value="2013265924TYPE299">output areas 2011 in East Midlands</option>
  # <option value="2013265927TYPE299">output areas 2011 in London</option>
  # <option value="2013265921TYPE299">output areas 2011 in North East</option>
  # <option value="2013265922TYPE299">output areas 2011 in North West</option>
  # <option value="2013265932TYPE299">output areas 2011 in Northern Ireland</option>
  # <option value="2013265931TYPE299">output areas 2011 in Scotland</option>
  # <option value="2013265928TYPE299">output areas 2011 in South East</option>
  # <option value="2013265929TYPE299">output areas 2011 in South West</option>
  # <option value="2013265930TYPE299">output areas 2011 in Wales</option>
  # <option value="2013265925TYPE299">output areas 2011 in West Midlands</option>
  # <option value="2013265923TYPE299">output areas 2011 in Yorkshire and The Humber</option>
  # <option value="TYPE480">regions</option>
  # <option value="TYPE297">super output areas - middle layer 2011</option>
  #   </select>
# <input type="submit" value="Download" class="btn" id="dwnBtn">

# function to create download file from nomis 
nomis_census_csv <- function(censusdata, geographytype){
  # generate filename
  filename <- paste(censusdata, geographytype, sep = "_") %>%
    paste0(., ".csv")
  destfile <- paste0(getwd(),"/data/", filename)
  # generate URL
  nomis_url_root <- "http://www.nomisweb.co.uk/census/2011/"
  nomis_url <- paste0(nomis_url_root, censusdata)
  # open http session
  nomis_session <- html_session(nomis_url)
  # get a copy of the form for the csv downloads
  nomis_form <- html_form(nomis_session)[[3]]
  # fill in the form, using the supplied details
  filled_form <- set_values(form = nomis_form, geography = geographytype)
  # request the data
  nomis_session2 <- submit_form(session = nomis_session,
                                form = filled_form)
  #download the file
  download.file(nomis_session2$url, destfile)
}

# Initially, TYPE480 regions to develop the code
geographytype <- "TYPE480" #regions
# later using TYPE297 'super output areas - middle layer 2011
# geographytype <- "TYPE297" #super output areas - middle layer
#creating list of census tables to take
censusdata_list <- c("ks101ew", "ks608ew","ks102ew", "ks201ew","ks501ew",
                     "ks103ew", "ks209ew", "ks105ew", "ks605ew", "qs203ew")
# apply the list to download all of the data
tmp <- lapply(censusdata_list, function(censusdata){
  # pause for a few random seconds to avoid annoying nomis
  time <- sample(4:17,1)
  print(paste("pausing", time, "seconds"))
  Sys.sleep(time)
  #run the function to generate and download the file
  nomis_census_csv(censusdata, geographytype)
})
### tidy:
rm(tmp, censusdata_list)
# rm(tmp, filled_form, nomis_form, nomis_session, nomis_session2, filename, nomis_url, nomis_url_root, censusdata_list, censusdata, time)

############ Import and clean the prediction #####################
# options(digits = 3)
# create function to read and load in csv data files
ingest <- function(censustable, geography){
  print(paste("ingesting", censustable))
  # generate file name for loading
  filename <- paste(censustable, geography, sep = "_") %>%
    paste0(., ".csv") %>%
    paste0("data/", .)
  print(filename)
  #read the file to data
  # data <- read_csv(filename)
  return(read_csv(filename))
}

# save data set with the geography type
data_set_save <- function(data_set_name){
  print(paste("saving", deparse(substitute(data_set_name))))
  # generate file name for saving
  filename <- paste(deparse(substitute(data_set_name)), 
                    geographytype, 
                    sep = "_") %>%
    paste0(., ".rda") %>%
    paste0("rda/", .)
  print(paste("saving as file", filename))
  # save the file to data
  save(data_set_name, file=filename)
}
# this is a work around for a weird occasional bug
# where in the previous function it saves with the name "data_set_name"
data_set_save_rds <- function(data_set_name){
  print(paste("saving", deparse(substitute(data_set_name))))
  # generate file name for saving
  filename <- paste(deparse(substitute(data_set_name)), 
                    geographytype, 
                    sep = "_") %>%
    paste0(., ".rds") %>%
    paste0("rda/", .)
  print(paste("saving as file", filename))
  # save the file to data
  saveRDS(data_set_name, file=filename)
}

#load in occupation data
# https://www.nomisweb.co.uk/census/2011/ks608ew
# import the new table with occupation data
data <- ingest("ks608ew", geographytype)
# names(data)
## initial version replaced by function
# create mgr-prf ratios
# occupation <- data %>%
#   rename("geo_code" = "geography code", 
#          "geo_name" = "geography", 
#          "geo_type" = "Rural Urban", 
#          "occupation_all" = "Sex: All persons; Occupation: All categories: Occupation; measures: Value") %>%
#   rowwise() %>%
#   # calculate the ratiof of managers and professionals
#   mutate(y = 
#            (sum(across(contains("Sex: All persons; Occupation: 1. Managers"))) +
#               sum(across(contains("Sex: All persons; Occupation: 2. Professional "))) )
#          /occupation_all) %>% 
#   select(geo_name, geo_code, geo_type, y, occupation_all)
# occupation

#now creating a function for repeatability
occupation_target <- function(data){
  occupation <- data %>%
    dplyr::rename("geo_code" = "geography code", 
           "geo_name" = "geography", 
           "geo_type" = "Rural Urban", 
           "occupation_all" = "Sex: All persons; Occupation: All categories: Occupation; measures: Value")  %>%
    rowwise() %>%
    # calculate the ratio of managers and professionals
    dplyr::mutate(y = 
             (sum(across(contains("Sex: All persons; Occupation: 1. Managers"))) +
                sum(across(contains("Sex: All persons; Occupation: 2. Professional "))) )
           /occupation_all) %>% 
    dplyr::select(geo_name, geo_code, geo_type, y, occupation_all)
  #save file
  save(occupation, file='rda/occupation.rda')
}
#running the function on the data set
occupation_target(data)
#adding to main data set
load('rda/occupation.rda')
data_set <- occupation
#tidy up
# save(data_set, file='rda/data_set.rda')
data_set_save(data_set)
rm(occupation, data)

############ Import and clean the data #####################
#step wise adding more tables from the census to put together potential predictors
#load in residents and sex/gender data
data <- ingest("ks101ew", geographytype)
names(data)
sex <- data %>% 
  # rename geo code column
  rename("geo_code" = "geography code") %>%
  rename("all_residents" = "Variable: All usual residents; measures: Value") %>%
  rename("females" = "Variable: Females; measures: Value") %>%
  mutate(female_ratio =  females/all_residents) %>%
  select(geo_code, all_residents, female_ratio)
names(sex)
#adding to main data set
data_set <- data_set %>%
  left_join(sex)
names(data_set)
# tidy
data_set_save(data_set)
save(sex, file='rda/sex_orig.rda')
rm(sex, data)

# load in age data
# https://www.nomisweb.co.uk/census/2011/ks102ew
# import the new table with age data
data <- ingest("ks102ew", geographytype)
names(data)
#look at all of the ages, to find the right ones to work on
age <- data %>%
  rename_at(vars(contains("Age")), ~str_replace_all(., "; measures: Value", "")) %>%
  rename_at(vars(contains("Age")), ~str_replace_all(., "Age: Age ", "age_")) %>%
  rename_at(vars(contains("Age")), ~str_replace_all(., " to ", "_")) %>% 
  rename_at(vars(contains("over")), ~str_replace_all(., " and ", "_")) %>% 
  rename("geo_code" = "geography code", 
         "all_residents" = "Age: All usual residents", 
         "age_median" = "Age: Median Age") %>% 
  mutate("under_16" = age_0_4 + age_5_7 + age_8_9 + age_10_14 + age_15) %>%
  mutate("over_74" = age_75_84 + age_85_89 + age_90_over) %>%
  mutate("age_16_to_74" = all_residents - over_74 - under_16) %>%
  mutate("age_25_to_64" = (age_16_to_74 - age_16_17 - age_18_19 - age_20_24 - age_65_74)) %>%
  select(geo_code, all_residents, age_16_to_74, age_25_to_64, age_median)
names(age)
data_set %>%
#comparing to the all occupation number in the data set
  left_join(age) %>%
  select(geo_name, occupation_all, all_residents, age_16_to_74, age_25_to_64) %>%
  knitr::kable(caption="Comparison of resident numbers")
age <- age %>%
  select(-age_16_to_74, -age_25_to_64)

# rewriting as a function for the median age only
age_predictors <- function(data){
  age <- data %>%
    dplyr::rename("geo_code" = "geography code", 
           "all_residents" = "Age: All usual residents; measures: Value",
           "age_median" = "Age: Median Age; measures: Value") %>% 
    dplyr::select(geo_code, all_residents, age_median)
  save(age, file='rda/age.rda')
}

#running the function on the data set
age_predictors(data)
#adding to main data set
load('rda/age.rda')
#adding to main data set
data_set <- data_set %>%
  left_join(age)
# show the ages, and the numbers
names(data_set)
# tidy
data_set_save(data_set)
rm(age, data)


# trying to find the source of the Occupation total of people
# checking economic activity
# loading the file with economic data
nomis_census_csv("ks601ew", geographytype)
data <- ingest("ks601ew", geographytype)
names(data)
# extract the resident numbers
economic <- data %>%
  rename_at(vars(contains("Sex")), ~str_replace_all(., "; measures: Value", "")) %>%
  rename_at(vars(contains("Sex")), ~str_replace_all(., "Sex: All persons; Economic Activity: ", "")) %>%
  select(2:22) %>% select(-"Rural Urban") %>%
  rename("geo_code" = "geography code", "geo_name" = "geography")
names(economic)
#table comparing the numbers
economic %>%
  left_join(data_set) %>%
  rename("all_res_16_74" = "All usual residents aged 16 to 74") %>%
  rename("econ_active" = "Economically active") %>%
  rename("employed" = "Economically active: In employment") %>%
  select(geo_name, all_res_16_74, econ_active, employed,
  occupation_all, all_residents) %>%
  knitr::kable(caption="Comparison of resident numbers and employment")
# tidy
rm(economic, data)


# next to load in the ethnicity data
# https://www.nomisweb.co.uk/census/2011/lc2101ew
# downloading a new table, this time with age in addition to ethnicity
nomis_census_csv("lc2101ew", geographytype)
data <- ingest("lc2101ew", geographytype)
names(data)
# keep only data for all sexes, between 24 and 64, and tidy the names
ethnicity_raw <- data %>%
  #keeping only the data relating to age 25 to 64
  select(3, 52:99) %>%
  #tidying up names as they are long
  rename_at(vars(contains("measures")), ~str_replace_all(., "; measures: Value", "")) %>%
  rename_at(vars(contains("persons")), ~str_replace_all(., "Sex: All persons; Age: ", "")) %>%
  rename_at(vars(contains("Ethnic")), ~str_replace_all(., "Ethnic Group: ", "")) %>%
  # adding up the year groups
  rowwise() %>%
  dplyr::mutate("age25_64 All categories: Ethnic group" = sum(across(contains("All categories: Ethnic group"))), .before = "Age 25 to 49; All categories: Ethnic group") %>%
  dplyr::mutate("age25_64 White: English/Welsh/Scottish/Northern Irish/British" = sum(across(contains("White: English/Welsh/Scottish/Northern Irish/British"))), .before = "Age 25 to 49; All categories: Ethnic group") %>%
  dplyr::mutate("age25_64 White: Irish" = sum(across(contains("White: Irish"))), .before = "Age 25 to 49; All categories: Ethnic group") %>%
  mutate("age25_64 White: Gypsy or Irish Traveller" = sum(across(contains("White: Gypsy or Irish Traveller"))), .before = "Age 25 to 49; All categories: Ethnic group") %>%
  mutate("age25_64 White: Other White" = sum(across(contains("White: Other White"))), .before = "Age 25 to 49; All categories: Ethnic group") %>% 
  mutate("age25_64 Mixed/multiple ethnic group: White and Black Caribbean" = sum(across(contains("Mixed/multiple ethnic group: White and Black Caribbean"))), .before = "Age 25 to 49; All categories: Ethnic group") %>%
  mutate("age25_64 Mixed/multiple ethnic group: White and Black African" = sum(across(contains("Mixed/multiple ethnic group: White and Black African"))), .before = "Age 25 to 49; All categories: Ethnic group") %>%
  mutate("age25_64 Mixed/multiple ethnic group: White and Asian" = sum(across(contains("Mixed/multiple ethnic group: White and Asian"))), .before = "Age 25 to 49; All categories: Ethnic group") %>%
  mutate("age25_64 Mixed/multiple ethnic group: Other Mixed" = sum(across(contains("Mixed/multiple ethnic group: Other Mixed"))), .before = "Age 25 to 49; All categories: Ethnic group") %>%
  mutate("age25_64 Asian/Asian British: Indian" = sum(across(contains("Asian/Asian British: Indian"))), .before = "Age 25 to 49; All categories: Ethnic group") %>%
  mutate("age25_64 Asian/Asian British: Pakistani" = sum(across(contains("Asian/Asian British: Pakistani"))), .before = "Age 25 to 49; All categories: Ethnic group") %>%
  mutate("age25_64 Asian/Asian British: Bangladeshi" = sum(across(contains("Asian/Asian British: Bangladeshi"))), .before = "Age 25 to 49; All categories: Ethnic group") %>%
  mutate("age25_64 Asian/Asian British: Chinese" = sum(across(contains("Asian/Asian British: Chinese"))), .before = "Age 25 to 49; All categories: Ethnic group") %>%
  mutate("age25_64 Asian/Asian British: Other Asian" = sum(across(contains("Asian/Asian British: Other Asian"))), .before = "Age 25 to 49; All categories: Ethnic group") %>%
  mutate("age25_64 Black/African/Caribbean/Black British: African" = sum(across(contains("Black/African/Caribbean/Black British: African"))), .before = "Age 25 to 49; All categories: Ethnic group") %>%
  mutate("age25_64 Black/African/Caribbean/Black British: Caribbean" = sum(across(contains("Black/African/Caribbean/Black British: Caribbean"))), .before = "Age 25 to 49; All categories: Ethnic group") %>%
  mutate("age25_64 Black/African/Caribbean/Black British: Other Black" = sum(across(contains("Black/African/Caribbean/Black British: Other Black"))), .before = "Age 25 to 49; All categories: Ethnic group") %>%
  mutate("age25_64 Other ethnic group: Arab" = sum(across(contains("Other ethnic group: Arab"))), .before = "Age 25 to 49; All categories: Ethnic group") %>%
  mutate("age25_64 Other ethnic group: Any other ethnic group" = sum(across(contains("Other ethnic group: Any other ethnic group"))), .before = "Age 25 to 49; All categories: Ethnic group") %>%
  #keep only the age 25 to 64 sections.
  select( contains("geography code") | contains("25_64")) %>%
  #checksum
  mutate(checksum_all = sum(across(3:20)), .after = "geography code") %>%
  #tidy some names
  rename_at(vars(contains(":")), ~str_replace_all(., ": ", "_")) %>%
  rename_at(vars(contains(" ")), ~str_replace_all(., " ", "_")) %>%
  rename_at(vars(contains("/")), ~str_replace_all(., "/", "_"))

# tidy
save(ethnicity_raw, file='rda/ethicity_raw.rda')
names(ethnicity_raw)

# reordering to identify which are the largest groups to retain
# load('rda/ethicity_raw.rda')
# make the geo code a rowname - all data is now a value
ethnicity_ordered <- column_to_rownames(ethnicity_raw, var="geography_code")
# str(ethnicity_ordered)
# add a new row with column sums, so totals for each ethnicity
ethnicity_ordered <- ethnicity_ordered %>%  
  rbind("total" = colSums(ethnicity_ordered))
# reorder columns based on the values in the total row
ethnicity_ordered <- ethnicity_ordered[, order(-ethnicity_ordered[which(rownames(ethnicity_ordered) == 'total'), ]) ]
# choose the first 11 columns, the first 11 ethnicities
data.frame("ethnicity categories" = names(ethnicity_ordered)[2:12]) %>%  
  knitr::kable(caption="11 most prevalent ethnicities")

# original code, replaced by the function.  kept as a check of the function
# # aggregating the other categories
# ethnicity_aggregated <- ethnicity_raw %>%
#   rename("all_25_64" = "age25_64_All_categories_Ethnic_group",
#   white_uk_25_64 = "age25_64_White_English_Welsh_Scottish_Northern_Irish_British") %>%
#   mutate("white_other_25_64" = age25_64_White_Other_White + age25_64_White_Gypsy_or_Irish_Traveller, .after = white_uk_25_64) %>%
#   select(-age25_64_White_Other_White, -age25_64_White_Gypsy_or_Irish_Traveller) %>% 
#   rename(indian_25_64 = "age25_64_Asian_Asian_British_Indian",
#   pakistani_25_64 = "age25_64_Asian_Asian_British_Pakistani",
#   black_african_25_64 = "age25_64_Black_African_Caribbean_Black_British_African",
#   asian_other_25_64 = "age25_64_Asian_Asian_British_Other_Asian") %>%
#   mutate("black_other_25_64" = age25_64_Black_African_Caribbean_Black_British_Caribbean + age25_64_Black_African_Caribbean_Black_British_Other_Black, .after = asian_other_25_64) %>%
#   select(-age25_64_Black_African_Caribbean_Black_British_Caribbean, -age25_64_Black_African_Caribbean_Black_British_Other_Black) %>%
#   rename(white_irish_25_64 = "age25_64_White_Irish",
#   chinese_25_64 = "age25_64_Asian_Asian_British_Chinese",
#   bangladeshi_25_64 = "age25_64_Asian_Asian_British_Bangladeshi") %>%
#   mutate(other_ethnicity_25_64 = age25_64_Other_ethnic_group_Any_other_ethnic_group +
#            age25_64_Mixed_multiple_ethnic_group_White_and_Black_Caribbean +
#            age25_64_Other_ethnic_group_Arab +
#            age25_64_Mixed_multiple_ethnic_group_Other_Mixed +
#            age25_64_Mixed_multiple_ethnic_group_White_and_Asian +
#            age25_64_Mixed_multiple_ethnic_group_White_and_Black_African, .after = bangladeshi_25_64) %>%
#   select(-age25_64_Other_ethnic_group_Any_other_ethnic_group, -age25_64_Mixed_multiple_ethnic_group_White_and_Black_Caribbean, -age25_64_Other_ethnic_group_Arab, -age25_64_Mixed_multiple_ethnic_group_Other_Mixed, -age25_64_Mixed_multiple_ethnic_group_White_and_Asian, -age25_64_Mixed_multiple_ethnic_group_White_and_Black_African) %>%
#   rename("geo_code" = "geography_code") %>%
#   rowwise() %>%
#   mutate(checksum_all2 = sum(across(4:14)), .after = checksum_all)
# # sum(ethnicity_aggregated$checksum_all2)
# # After this aggregation, I now need to divide by the sums of the rows, to have a normalised value for each of the ethnicities.
# ethnicity <- sweep(ethnicity_aggregated[,5:15], 1, rowSums(ethnicity_aggregated[,5:15]), FUN = "/")  %>%
#   cbind(ethnicity_aggregated[,1])
# #tmp, as a check
# ethn_orig <- ethnicity

# creating a function for repeatability
ethnicity_predictors <- function(data){
  # print("manipulating raw data")
  ethnicity_raw <- data %>%
    #keeping only the data relating to age 25 to 64
    dplyr::select(contains("geography code") | 
             contains("Age 25 to 49") | 
             contains("Age 50 to 64") ) %>%
    # adding up the year groups
    rowwise() %>%
    dplyr::mutate("all_25_64" = 
             sum(across(contains("All categories: Ethnic group"))) ) %>%
    dplyr::mutate("white_uk_25_64" = 
             sum(across(contains("White: English/Welsh/Scottish/Northern Irish/British"))) ) %>%
    dplyr::mutate("white_other_25_64" = 
             sum(across(contains("White: Other White"))) + 
             sum(across(contains("White: Gypsy or Irish Traveller"))) ) %>%
    dplyr::mutate("white_irish_25_64" = 
             sum(across(contains("White: Irish"))) ) %>%
    dplyr::mutate("indian_25_64" = 
             sum(across(contains("Asian/Asian British: Indian"))) ) %>%
    dplyr::mutate("pakistani_25_64" = 
             sum(across(contains("Asian/Asian British: Pakistani"))) ) %>%
    dplyr::mutate("bangladeshi_25_64" = 
             sum(across(contains("Asian/Asian British: Bangladeshi"))) ) %>%
    dplyr::mutate("other_ethnicity_25_64" = 
             sum(across(contains("Other ethnic group: Any other ethnic group"))) +
             sum(across(contains("Mixed/multiple ethnic group: White and Black Caribbean"))) +
             sum(across(contains("Other ethnic group: Arab"))) +
             sum(across(contains("Mixed/multiple ethnic group: Other Mixed"))) +
             sum(across(contains("Mixed/multiple ethnic group: White and Asian"))) +
             sum(across(contains("Mixed/multiple ethnic group: White and Black African")))) %>%
    dplyr::mutate("chinese_25_64" = 
             sum(across(contains("Asian/Asian British: Chinese"))) ) %>%
    dplyr::mutate("asian_other_25_64" =
             sum(across(contains("Asian/Asian British: Other Asian"))) ) %>%
    dplyr::mutate("black_other_25_64" = 
             sum(across(contains("Black/African/Caribbean/Black British: Caribbean"))) + 
             sum(across(contains("Black/African/Caribbean/Black British: Other Black"))) ) %>%
    dplyr::mutate("black_african_25_64" = 
             sum(across(contains("Black/African/Caribbean/Black British: African"))) ) %>%
    dplyr::rename("geo_code" = "geography code") %>%
    # print(names(ethnicity)) %>%
    dplyr::select(contains("geo_code") | contains("25_64"))
    # print(names(ethnicity))
  # After this aggregation, I now need to divide by the sums of the rows, to have a normalised value for each of the ethnicities.
  # print("normalising the data")
  ethnicity <- sweep(ethnicity_raw[,3:13], 1, rowSums(ethnicity_raw[,3:13]), FUN = "/")  %>%
    cbind(ethnicity_raw[,1])
  # print("saving the file")
  save(ethnicity, file='rda/ethnicity.rda')
}
#running the function on the data set
ethnicity_predictors(data)
#adding to main data set
load('rda/ethnicity.rda')
#checking it works ok
# identical(ethn_orig, ethnicity)
#adding to main data set
data_set <- data_set %>%
  left_join(ethnicity)
names(data_set)
#save file, tidy up
data_set_save(data_set)
rm(ethnicity_aggregated, ethnicity_ordered)
rm(ethnicity_raw, ethnicity, ethn_orig, data)


# next to load in the sex/gender data, for ages 25 to 64 
# https://www.nomisweb.co.uk/census/2011/lc2101ew
# using the ethnicity data, which includes gender
data <- ingest("lc2101ew", geographytype)
# # original code commented out and retained for checking the function
# #creating the raw file
# sex_25_64 <- data %>%
#   #filtering all ethnicities
#   select(contains("Ethnic Group: All categories:") | 
#            contains("geography code") ) %>%
#   #filtering to the age groups in question
#   select(contains("Age 25 to 49") | 
#            contains("Age 50 to 64") | 
#            contains("geography code") ) %>%
#   #mutate to create the sums for comparison
#   rowwise() %>%
#   mutate("all_25_64"= sum(across(contains("Sex: All Persons")))) %>%
#   mutate("female_25_64"= sum(across(contains("Sex: Female")))) %>%
#   mutate("male_25_64"= sum(across(contains("Sex: Males")))) %>%
#   mutate("checksum" = female_25_64 + male_25_64) %>%
#   mutate(female_ratio_25_64 = female_25_64/all_25_64)%>% 
#   rename("geo_code" = "geography code") 
# #limit to the two columns necessary
# sex_25_64 <- sex_25_64 %>%
#   select(contains("female_ratio_25_64") | contains("geo_code") )

#recreating as a function for reuse
sex_predictors <- function(data){
  sex_25_64 <- data %>%
    dplyr::select(contains("Ethnic Group: All categories:") | 
             contains("geography code") ) %>%
    dplyr::select(contains("Age 25 to 49") | 
             contains("Age 50 to 64") | 
             contains("geography code") ) %>%
    rowwise() %>%
    #create the ratio necessary
    dplyr::mutate(female_ratio_25_64 = sum(across(contains("Sex: Female"))) / 
             sum(across(contains("Sex: All Persons")))) %>%
    dplyr::rename("geo_code" = "geography code")  %>%
    dplyr::select(contains("female_ratio_25_64") | contains("geo_code") )
  save(sex_25_64, file='rda/sex_25_64.rda')
}
#running the function on the data set
sex_predictors(data)
#adding to main data set
load('rda/sex_25_64.rda')
#adding to main data set
data_set <- data_set %>%
  left_join(sex_25_64)
# show the data set values
names(data_set)
# tidy up
# tidy earlier female ratio for all ages
data_set <- data_set %>%
  select(-female_ratio)
data_set_save(data_set)
rm(sex, sex_25_64, data)


# qualifications
# https://www.nomisweb.co.uk/census/2011/lc5102ew
# import the new table with qualifications
nomis_census_csv("lc5102ew", geographytype)
data <- ingest("lc5102ew", geographytype)
names(data)
# # retained original code, commented out
# # manipulate to be the 25 to 64 only
# qualifications_raw <- data %>%
#   #save only the 25 to 64 years
#   select(contains("Age 25 to 34") | contains("Age 35 to 49") | contains("Age 50 to 64") | contains("geography code")) %>%
#   # tidy names to help understand them
#   rename_at(vars(contains("measures")), ~str_replace_all(., "; measures: Value", "")) %>%
#   rename("geo_code" = "geography code") %>%
#   rename_at(vars(contains("; Highest Level of Qualification: ")), ~str_replace_all(., "; Highest Level of Qualification: ", "_")) %>%
#   #calculate the sums for the retained predictors
#   rowwise() %>%
#   mutate("all_qual_25_64"= sum(across(contains("All categories: ")))) %>%
#   mutate("no_qual_25_64"= sum(across(contains("No qualifications")))) %>%
#   mutate("level1_25_64"= sum(across(contains("Level 1 qualification")))) %>%
#   mutate("level2_25_64"= sum(across(contains("Level 2 qualification")))) %>%
#   mutate("level3_25_64"= sum(across(contains("Level 3 qualification")))) %>%
#   mutate("level4_25_64"= sum(across(contains("Level 4 qualification")))) %>%
#   mutate("other_qual_25_64"= sum(across(contains("Other qualifications")))) %>%
#   mutate("apprentice_25_64"= sum(across(contains("Apprenticeship")))) %>%
#   select( contains("geo_code") | contains("25_64")) %>%
#   #checksum to check for errors
#   mutate(checksum_all = sum(across(contains("25_64"))) - all_qual_25_64, .after = geo_code)
# 
# # normalise per area
# qualifications <- sweep(qualifications_raw[,4:10], 1, rowSums(qualifications_raw[,4:10]), FUN = "/")  %>%
#   cbind(qualifications_raw[,1])
# # checksum to ensure no errors/typos...
# # %>%
# #   rowwise() %>%
# #   mutate(checksum_all = sum(across(1:7)))
# #to check the function works
# # qual_orig <- qualifications

#creating a function for repeatability
qualifications_predictors <- function(data){
  qualifications_raw <- data %>%
    #save only the 25 to 64 years
    dplyr::select(contains("Age 25 to 34") | contains("Age 35 to 49") | contains("Age 50 to 64") | contains("geography code")) %>%
    dplyr::rename("geo_code" = "geography code") %>%
    #calculate the sums for the retained predictors
    rowwise() %>%
    dplyr::mutate("all_qual_25_64"= sum(across(contains("All categories: ")))) %>%
    dplyr::mutate("no_qual_25_64"= sum(across(contains("No qualifications")))) %>%
    dplyr::mutate("level1_25_64"= sum(across(contains("Level 1 qualification")))) %>%
    dplyr::mutate("level2_25_64"= sum(across(contains("Level 2 qualification")))) %>%
    dplyr::mutate("level3_25_64"= sum(across(contains("Level 3 qualification")))) %>%
    dplyr::mutate("level4_25_64"= sum(across(contains("Level 4 qualification")))) %>%
    dplyr::mutate("other_qual_25_64"= sum(across(contains("Other qualifications")))) %>%
    dplyr::mutate("apprentice_25_64"= sum(across(contains("Apprenticeship")))) %>%
    dplyr::select( contains("geo_code") | contains("25_64"))
  # normalise per area
  qualifications <- sweep(qualifications_raw[,3:9], 1, rowSums(qualifications_raw[,3:9]), FUN = "/")  %>%
    cbind(qualifications_raw[,1])
  #save file
  save(qualifications, file='rda/qualifications.rda')
}
#running the function on the data set
qualifications_predictors(data)
#adding to main data set
load('rda/qualifications.rda')
# check the function works
# identical(qual_orig, qualification)
#adding to main data set
data_set <- data_set %>%
  left_join(qualifications)
names(data_set)
#tidy up
data_set_save(data_set)
rm(qualifications_raw, qualifications, qual_orig, data)


# next marital status
# https://www.nomisweb.co.uk/census/2011/lc1101ew
# import the new table with marital status
nomis_census_csv("lc1101ew", geographytype)
data <- ingest("lc1101ew", geographytype)
names(data)
# original code commented out, retained to check functions
# # manipulate to be the 25 to 64 only
# marital_raw <- data %>%
#   #only all genders
#   select(contains("Sex: All persons;") | contains("geography code")) %>%
#   #save only the 25 to 64 years
#   select(contains("Age 25 to 34") | contains("Age 35 to 49") | contains("Age 50 to 64") | contains("geography code")) %>%
#   rename("geo_code" = "geography code") %>%
#   #calculate for retained predictors
#   rowwise() %>%
#   mutate("all_marital_25_64"= sum(across(contains("All categories: ")))) %>%
#   mutate("single_25_64"= sum(across(contains("Marital Status: Single")))) %>%
#   mutate("married_25_64"= sum(across(contains("Marital Status: Married;")))) %>%
#   mutate("civil_25_64"= sum(across(contains("Marital Status: In a registered same-sex civil")))) %>%
#   mutate("separated_25_64"= sum(across(contains("Marital Status: Separated")))) %>%
#   mutate("divorced_25_64"= sum(across(contains("Marital Status: Divorced")))) %>%
#   mutate("widowed_25_64"= sum(across(contains("Marital Status: Widowed")))) %>%
#   select(contains("geo_code") | contains("25_64")) %>%
#   #checksum to confirm works ok
#   mutate(checksum_all = sum(across(contains("25_64"))) - all_marital_25_64, .after = geo_code)
# # normalise per area
# marital <- sweep(marital_raw[,4:9], 1, rowSums(marital_raw[,4:9]), FUN = "/")  %>%
#   cbind(marital_raw[,1])
# # checksum to ensure no errors/typos...
# # %>%
# #   rowwise() %>%
# #   mutate(checksum_all = sum(across(1:6)))
# #to check the function works
# # marital_orig <- marital

#creating a function for repeatability
marital_predictors <- function(data){
  marital_raw <- data %>%
    #only all genders
    dplyr::select(contains("Sex: All persons;") | contains("geography code")) %>%
    #save only the 25 to 64 years
    dplyr::select(contains("Age 25 to 34") | contains("Age 35 to 49") | contains("Age 50 to 64") | contains("geography code")) %>%
    dplyr::rename("geo_code" = "geography code") %>%
    #calculate for retained predictors
    rowwise() %>%
    dplyr::mutate("all_marital_25_64"= sum(across(contains("All categories: ")))) %>%
    dplyr::mutate("single_25_64"= sum(across(contains("Marital Status: Single")))) %>%
    dplyr::mutate("married_25_64"= sum(across(contains("Marital Status: Married;")))) %>%
    dplyr::mutate("civil_25_64"= sum(across(contains("Marital Status: In a registered same-sex civil")))) %>%
    dplyr::mutate("separated_25_64"= sum(across(contains("Marital Status: Separated")))) %>%
    dplyr::mutate("divorced_25_64"= sum(across(contains("Marital Status: Divorced")))) %>%
    dplyr::mutate("widowed_25_64"= sum(across(contains("Marital Status: Widowed")))) %>%
    dplyr::select(contains("geo_code") | contains("25_64"))
  # normalise per area
  marital <- sweep(marital_raw[,3:8], 1, rowSums(marital_raw[,3:8]), FUN = "/")  %>%
    cbind(marital_raw[,1])
  save(marital, file='rda/marital.rda')
}
#running the function on the data set
marital_predictors(data)
#adding to main data set
load('rda/marital.rda')
# check the function works
# identical(marital_orig, marital)
#adding to main data set
data_set <- data_set %>%
  left_join(marital)
names(data_set)
#tidy up
data_set_save(data_set)
rm(marital, marital_raw, marital_orig, data)


# next religion
# https://www.nomisweb.co.uk/census/2011/lc2107ew
# import the new table with religion
nomis_census_csv("lc2107ew", geographytype)
data <- ingest("lc2107ew", geographytype)
names(data)
# retaining original code for checking
# # manipulate to be the 25 to 64 only
# religion_raw <- data %>%
#   # all genders
#   select(contains("Sex: All persons;") | contains("geography code")) %>%
#   #select the working age only
#   select(contains("Age 25 to 34") | contains("Age 35 to 49") | contains("Age 50 to 64") | contains("geography code")) %>%
#   rename("geo_code" = "geography code") %>%
#   #calculate the sums across ages
#   rowwise() %>%
#   mutate("all_religion_25_64"= sum(across(contains("All categories:")))) %>%
#   mutate("christian_25_64"= sum(across(contains("Christian")))) %>%
#   mutate("buddhist_25_64"= sum(across(contains("Buddhist")))) %>%
#   mutate("hindu_25_64"= sum(across(contains("Hindu")))) %>%
#   mutate("jewish_25_64"= sum(across(contains("Jewish")))) %>%
#   mutate("muslim_25_64"= sum(across(contains("Muslim")))) %>%
#   mutate("sikh_25_64"= sum(across(contains("Sikh")))) %>%
#   mutate("other_25_64"= sum(across(contains("Other religion;")))) %>%
#   mutate("no_religion_25_64"= sum(across(contains("No religion;")))) %>%
#   mutate("religion_not_stated_25_64"= sum(across(contains("Religion not stated;")))) %>%
#   select(contains("geo_code") | contains("25_64")) %>%
#   #checksum to ensure everything covered
#   mutate(checksum_all = sum(across(contains("25_64"))) - all_religion_25_64, .after = geo_code)
# # checking for how many as a proportion
# religion_ordered <- column_to_rownames(religion_raw, var="geo_code")
# religion_ordered <- religion_ordered %>%  
#   rbind("total" = colSums(religion_ordered)) %>%
#   rbind("proportion" = 100*colSums(religion_ordered)/29615071) 
# religion_ordered <- religion_ordered[, order(-religion_ordered[which(rownames(religion_ordered) == 'total'), ]) ]
# names(religion_ordered)
# # normalise per area
# religion <- sweep(religion_raw[,4:12], 1, rowSums(religion_raw[,4:12]), FUN = "/")  %>%
#   cbind(religion_raw[,1])
# # checksum to ensure no errors/typos...
# # %>%
# #   rowwise() %>%
# #   mutate(checksum_all = sum(across(1:9)))
# #check function works
# # religion_orig <- religion

#creating a function for repeatability
religion_predictors <- function(data){
  religion_raw <- data %>%
    # all genders
    dplyr::select(contains("Sex: All persons;") | contains("geography code")) %>%
    #select the working age only
    dplyr::select(contains("Age 25 to 34") | contains("Age 35 to 49") | contains("Age 50 to 64") | contains("geography code")) %>%
    dplyr::rename("geo_code" = "geography code") %>%
    #calculate the sums across ages
    rowwise() %>%
    dplyr::mutate("all_religion_25_64"= sum(across(contains("All categories:")))) %>%
    dplyr::mutate("christian_25_64"= sum(across(contains("Christian")))) %>%
    dplyr::mutate("buddhist_25_64"= sum(across(contains("Buddhist")))) %>%
    dplyr::mutate("hindu_25_64"= sum(across(contains("Hindu")))) %>%
    dplyr::mutate("jewish_25_64"= sum(across(contains("Jewish")))) %>%
    dplyr::mutate("muslim_25_64"= sum(across(contains("Muslim")))) %>%
    dplyr::mutate("sikh_25_64"= sum(across(contains("Sikh")))) %>%
    dplyr::mutate("other_25_64"= sum(across(contains("Other religion;")))) %>%
    dplyr::mutate("no_religion_25_64"= sum(across(contains("No religion;")))) %>%
    dplyr::mutate("religion_not_stated_25_64"= sum(across(contains("Religion not stated;")))) %>%
    dplyr::select(contains("geo_code") | contains("25_64"))
  # normalise per area
  religion <- sweep(religion_raw[,3:11], 1, rowSums(religion_raw[,3:11]), FUN = "/")  %>%
    cbind(religion_raw[,1])
  save(religion, file='rda/religion.rda')
}

#running the function on the data set
religion_predictors(data)
#adding to main data set
load('rda/religion.rda')
# check the function works
# identical(religion_orig, religion)
#adding to main data set
data_set <- data_set %>%
  left_join(religion)
names(data_set)
#tidy up
data_set_save(data_set)
rm(religion, religion_raw, religion_ordered, religion_orig, data)


#next household composition
# https://www.nomisweb.co.uk/census/2011/lc1109ew
# import the new table with household
nomis_census_csv("lc1109ew", geographytype)
data <- ingest("lc1109ew", geographytype)
names(data)
# retaining original code to check the function.
# # manipulate to be the 25 and over only
# household_raw <- data %>%
#   # retain only the all sex (not female or male)
#   select(contains("Sex: All persons;") | contains("geography code")) %>%
#   select(contains("Age 25 to 34") | contains("Age 35 to 49") | contains("Age 50 and over") | contains("geography code")) %>%
#   rename("geo_code" = "geography code") %>%
#   # rename so that it is easier to understand the breakdowns
#   rename_at(vars(contains("; Household Composition: ")), ~str_replace_all(., "; Household Composition: ", "_")) %>%
#   rename_at(vars(contains("Sex: All persons; Age: Age")), ~str_replace_all(., "Sex: All persons; Age: Age ", "Age ")) %>%
#   rename_at(vars(contains("; measures: Value")), ~str_replace_all(., "; measures: Value", "")) %>%
#   # retain the totals, the total amount, and the over 65 columns
#   select(contains("geo_code") | contains("All categories: Household composition") | contains(": Total") | contains("ged 65 and over")) %>%
#   # #checksum, make sure the totals add up for 25 to 34
#   # select(contains("geo_code") | contains("Age 25 to 34")) %>%
#   # rowwise() %>%
#   # mutate(checksum_25_34 = sum(across(contains("Age 25 to 34"))) - 
#   #          sum(across(contains("All categories: Household composition"))) - 
#   #          sum(across(contains("One family only: Total"))) -
#   #          sum(across(contains("Other household types: Other"))), .after = geo_code) %>%
#   # mutate(checksum_25_34_2 = 
#   #          sum(across(contains("One person household: Total"))) + 
#   #          sum(across(contains("Married or same-sex"))) + 
#   #          sum(across(contains("Cohabiting couple:"))) +
#   #          sum(across(contains("Lone parent:"))) +
#   #          sum(across(contains("Other household types: Total"))), .after = geo_code)
#   rowwise() %>%
#   # combine the age categories to create the predictors
#   mutate("all_household_25_64"= sum(across(contains("All categories:"))) -
#            sum(across(contains("One person household: Aged 65 and over"))) -
#            sum(across(contains("One family only: All aged 65 and over")))) %>%
#   mutate("oneperson_25_64"= sum(across(contains("One person household: Total"))) -
#            sum(across(contains("One person household: Aged 65 and over")))) %>%
#   mutate("marriedfamily_25_64"= sum(across(contains("Married")))) %>%
#   mutate("cohoabiting_25_64"= sum(across(contains("Cohabiting")))) %>%
#   mutate("loneparent_25_64"= sum(across(contains("Lone parent")))) %>%
#   mutate("otherhousehold_25_64"= sum(across(contains("Other household types: Total")))) %>%
#   select(contains("geo_code") | contains("25_64")) %>%
#   mutate(checksum_all = sum(across(contains("25_64"))) - all_household_25_64, .after = geo_code)
# # normalise per area
# household <- sweep(household_raw[,4:8], 1, rowSums(household_raw[,4:8]), FUN = "/")  %>%
#   cbind(household_raw[,1])
# # checksum to ensure no errors/typos...
# # %>%
# #   rowwise() %>%
# #   mutate(checksum_all = sum(across(1:5)))

# rerunning as a function for reuse
household_predictors <- function(data){
  household_raw <- data %>%
    # retain only the all sex (not female or male)
    dplyr::select(contains("Sex: All persons;") | contains("geography code")) %>%
    # retain only the specific age groups
    dplyr::select(contains("Age 25 to 34") | contains("Age 35 to 49") | contains("Age 50 and over") | contains("geography code")) %>%
    dplyr::rename("geo_code" = "geography code") %>%
    # retain the totals, the total amount, and the over 65 columns
    dplyr::select(contains("geo_code") | contains("All categories: Household composition") | contains(": Total") | contains("ged 65 and over")) %>%
    rowwise() %>%
    # combine the age categories to create the predictors
    dplyr::mutate("all_household_25_64"= sum(across(contains("All categories:"))) -
             sum(across(contains("One person household: Aged 65 and over"))) -
             sum(across(contains("One family only: All aged 65 and over")))) %>%
    dplyr::mutate("oneperson_25_64"= sum(across(contains("One person household: Total"))) -
             sum(across(contains("One person household: Aged 65 and over")))) %>%
    dplyr::mutate("marriedfamily_25_64"= sum(across(contains("Married")))) %>%
    dplyr::mutate("cohoabiting_25_64"= sum(across(contains("Cohabiting")))) %>%
    dplyr::mutate("loneparent_25_64"= sum(across(contains("Lone parent")))) %>%
    dplyr::mutate("otherhousehold_25_64"= sum(across(contains("Other household types: Total")))) %>%
    dplyr::select(contains("geo_code") | contains("25_64")) %>%
    dplyr::mutate(checksum_all = sum(across(contains("25_64"))) - all_household_25_64, .after = geo_code)
  # normalise per area
  household <- sweep(household_raw[,4:8], 1, rowSums(household_raw[,4:8]), FUN = "/")  %>%
    cbind(household_raw[,1])
  save(household, file='rda/household.rda')
}
#running the function on the data set
household_predictors(data)
#adding to main data set
load('rda/household.rda')
data_set <- data_set %>%
  left_join(household)
names(data_set)
#tidy up
data_set_save(data_set)
rm(household, data)
# rm(household_ordered, household_raw, household.org, houseshold_predictors)


#next Industry
# https://www.nomisweb.co.uk/census/2011/lc6110ew
# import the new table with Industry
nomis_census_csv("lc6110ew", geographytype)
data <- ingest("lc6110ew", geographytype)
names(data)
# original code retained for checking
# industry_raw <- data %>%
#   # retain only the specific age groups
#   select(contains("Age 25 to 34") | contains("Age 35 to 49") | contains("Age 50 to 64") | contains("geography code")) %>%
#   rename("geo_code" = "geography code") %>%  
# # combine the age categories to create the predictors
#   rowwise() %>%
#   mutate("all_industry_25_64"= sum(across(contains("All categories:")))) %>%
#   mutate("industry_abde_25_64"= sum(across(contains("Industry: A, B, D, E ")))) %>%
#   mutate("manufacturing_25_64"= sum(across(contains("Manufacturing")))) %>%
#   mutate("construction_25_64"= sum(across(contains("Construction")))) %>%
#   mutate("industry_gi_25_64"= sum(across(contains("Industry: G, I")))) %>%
#   mutate("industry_hj_25_64"= sum(across(contains("Industry: H, J ")))) %>%
#   mutate("industry_klmn_25_64"= sum(across(contains("Industry: K, L, M, N")))) %>%
#   mutate("industry_opq_25_64"= sum(across(contains("Industry: O, P, Q ")))) %>%
#   mutate("Industry_rstu_25_64"= sum(across(contains("Industry: R, S, T, U ")))) %>%
#   select(contains("geo_code") | contains("25_64")) %>%
#   mutate(checksum_all = sum(across(contains("25_64"))) - all_industry_25_64, .after = geo_code)
# # normalise per area
# industry <- sweep(industry_raw[,4:11], 1, rowSums(industry_raw[,4:11]), FUN = "/")  %>%
#   cbind(industry_raw[,1]) 
# # checksum to ensure no errors/typos...
#  # %>%
#   # rowwise() %>%
#   # mutate(checksum_all = sum(across(1:8)))

# rerunning as a function for reuse
industry_predictors <- function(data){
  industry_raw <- data %>%
    # retain only the specific age groups
    dplyr::select(contains("Age 25 to 34") | contains("Age 35 to 49") | 
             contains("Age 50 to 64") | contains("geography code")) %>%
    dplyr::rename("geo_code" = "geography code") %>%  
    # combine the age categories to create the predictors
    rowwise() %>%
    dplyr::mutate("all_industry_25_64"= sum(across(contains("All categories:")))) %>%
    dplyr::mutate("industry_abde_25_64"= sum(across(contains("Industry: A, B, D, E ")))) %>%
    dplyr::mutate("manufacturing_25_64"= sum(across(contains("Manufacturing")))) %>%
    dplyr::mutate("construction_25_64"= sum(across(contains("Construction")))) %>%
    dplyr::mutate("industry_gi_25_64"= sum(across(contains("Industry: G, I")))) %>%
    dplyr::mutate("industry_hj_25_64"= sum(across(contains("Industry: H, J ")))) %>%
    dplyr::mutate("industry_klmn_25_64"= sum(across(contains("Industry: K, L, M, N")))) %>%
    dplyr::mutate("industry_opq_25_64"= sum(across(contains("Industry: O, P, Q ")))) %>%
    dplyr::mutate("Industry_rstu_25_64"= sum(across(contains("Industry: R, S, T, U ")))) %>%
    dplyr::select(contains("geo_code") | contains("25_64")) %>%
    dplyr::mutate(checksum_all = sum(across(contains("25_64"))) - all_industry_25_64, .after = geo_code)
  # normalise per area
  industry <- sweep(industry_raw[,4:11], 1, rowSums(industry_raw[,4:11]), FUN = "/")  %>%
    cbind(industry_raw[,1]) 
  save(industry, file='rda/industry.rda')
}
#running the function on the data set
industry_predictors(data)
#adding to main data set
load('rda/industry.rda')
data_set <- data_set %>%
  left_join(industry)
names(data_set)
#tidy up
data_set_save(data_set)
rm(industry, industry_raw, indust_orig, data)


# next country of birth
# https://www.nomisweb.co.uk/census/2011/lc2103ew
# import the new table with country of birth
nomis_census_csv("lc2103ew", geographytype)
data <- ingest("lc2103ew", geographytype)
names(data)
# retain original for checking the functions
# # manipulate to be the 25 to 64 only
# country_raw <- data %>%
#   # retain only the all sex (not female or male)
#   select(contains("Sex: All persons;") | contains("geography code")) %>%
#   select(contains("Age 25 to 34") | contains("Age 35 to 49") | contains("Age 50 to 64") | contains("geography code")) %>%
#   rename("geo_code" = "geography code") %>%
#   # rename so that it is easier to understand the breakdowns
#   rename_at(vars(contains("; Country of Birth: ")), ~str_replace_all(., "; Country of Birth: ", "_")) %>%
#   rename_at(vars(contains("Sex: All persons; Age: Age")), ~str_replace_all(., "Sex: All persons; Age: Age ", "Age ")) %>%
#   rename_at(vars(contains("; measures: Value")), ~str_replace_all(., "; measures: Value", "")) %>%
#   # #checksum, make sure the totals add up for 25 to 34
#   # select(contains("geo_code") | contains("Age 25 to 34")) %>%
#   # rowwise() %>%
#   # mutate(checksum_25_34 = 
#   #          sum(across(contains("United Kingdom: Total"))) +
#   #          sum(across(contains("Europe: Ireland"))) +
#   #          sum(across(contains("Other Europe: EU countries: Total"))) +
#   #          sum(across(contains("Other Europe: Rest of Europe"))) +
#   #          sum(across(contains("Africa"))) +
#   #          sum(across(contains("Middle East and Asia"))) +
#   #          sum(across(contains("The Americas and the Caribbean"))) +
#   #          sum(across(contains("Antarctica, Oceania"))), .after = geo_code)
#   #          , .after = geo_code)
# # combine the age categories to create the predictors
#   rowwise() %>%
#   mutate("all_country_25_64"= sum(across(contains("All categories:")))) %>%
#   mutate("uk_25_64"= sum(across(contains("United Kingdom: Total")))) %>%
#   mutate("eu_2001_25_64"= sum(across(contains("Other Europe: EU countries: Member countries in March 2001"))) + sum(across(contains("Europe: Ireland")))) %>%
#   mutate("eu_rest_25_64"= sum(across(contains("Other Europe: EU countries: Accession countries April 2001 to March 2011"))) ) %>%
#   mutate("europe_rest_25_64"= sum(across(contains("Other Europe: Rest of Europe")))) %>%
#   mutate("africa_25_64"= sum(across(contains("Africa")))) %>%
#   mutate("me_asia_25_64"= sum(across(contains("Middle East and Asia")))) %>%
#   mutate("americas_25_64"= sum(across(contains("The Americas and the Caribbean")))) %>%
#   mutate("other_country_25_64"= sum(across(contains("Antarctica, Oceania (including Australasia) and other")))) %>%
#   select(contains("geo_code") | contains("25_64")) %>%
#   mutate(checksum_all = sum(across(contains("25_64"))) - all_country_25_64, .after = geo_code)
# # normalise per area
# country <- sweep(country_raw[,4:11], 1, rowSums(country_raw[,4:11]), FUN = "/")  %>%
#   cbind(country_raw[,1])
# # checksum to ensure no errors/typos...
# # %>%
# #   rowwise() %>%
# #   mutate(checksum_all = sum(across(1:9)))
# # to check the function
# # country_orig <- country
# # country_raw_orig <- country_raw

# rerunning as a function for reuse
country_predictors <- function(data){
  country_raw <- data %>%
    # retain only the all sex (not female or male)
    dplyr::select(contains("Sex: All persons;") | contains("geography code")) %>%
    dplyr::select(contains("Age 25 to 34") | contains("Age 35 to 49") | contains("Age 50 to 64") | contains("geography code")) %>%
    dplyr::rename("geo_code" = "geography code") %>%
    # combine the age categories to create the predictors
    rowwise() %>%
    dplyr::mutate("all_country_25_64"= sum(across(contains("All categories:")))) %>%
    dplyr::mutate("uk_25_64"= sum(across(contains("United Kingdom: Total")))) %>%
    dplyr::mutate("eu_2001_25_64"= sum(across(contains("Other Europe: EU countries: Member countries in March 2001"))) + sum(across(contains("Europe: Ireland")))) %>%
    dplyr::mutate("eu_rest_25_64"= sum(across(contains("Other Europe: EU countries: Accession countries April 2001 to March 2011"))) ) %>%
    dplyr::mutate("europe_rest_25_64"= sum(across(contains("Other Europe: Rest of Europe")))) %>%
    dplyr::mutate("africa_25_64"= sum(across(contains("Africa")))) %>%
    dplyr::mutate("me_asia_25_64"= sum(across(contains("Middle East and Asia")))) %>%
    dplyr::mutate("americas_25_64"= sum(across(contains("The Americas and the Caribbean")))) %>%
    dplyr::mutate("other_country_25_64"= sum(across(contains("Antarctica, Oceania (including Australasia) and other")))) %>%
    dplyr::select(contains("geo_code") | contains("25_64")) %>%
    dplyr::mutate(checksum_all = sum(across(contains("25_64"))) - all_country_25_64, .after = geo_code)
  # normalise per area
  country <- sweep(country_raw[,4:11], 1, rowSums(country_raw[,4:11]), FUN = "/")  %>%
    cbind(country_raw[,1])
  save(country, file='rda/country.rda')
}
#running the function on the data set
country_predictors(data)
#adding to main data set
load('rda/country.rda')
#checking the function
# identical(country_orig, country)
data_set <- data_set %>%
  left_join(country)
names(data_set)
#tidy up
data_set_save(data_set)
rm(country, country_raw, country_raw_orig, country_orig, data)
rm(data_set_save, data_set_save_rds)

# check the functions in the next section
data_set_maincode <- data_set


####### repeatable code ##########

#creating list of census tables to take
censusdata_list <- c("ks608ew", "ks102ew", "lc2101ew", "lc2103ew","lc6110ew", "lc1109ew", "lc2107ew", "lc1101ew", "lc5102ew")

#create a function to download all the tables in the list
download_censusdata <- function(geotype, datalist){
  # apply the list to download all of the data
  tmp <- lapply(datalist, function(censusdata){
    # pause for a few random seconds to avoid annoying nomis
    time <- sample(4:17,1)
    print(paste("pausing", time, "seconds"))
    Sys.sleep(time)
    print(paste("downloading geography area", geotype, "for table", censusdata))
    #run the function to generate and download the file
    nomis_census_csv(censusdata, geotype)
  })
  rm(tmp)
}

# function to load in the data for a given geotype
import_data <- function(geotype){
  print(paste("importing", geotype))
  #load in occupation data
  print(paste("loading occupation data"))
  # https://www.nomisweb.co.uk/census/2011/ks608ew
  # import the new table with occupation data
  data <- ingest("ks608ew", geotype)
  #running the function on the data set
  occupation_target(data)
  #adding to main data set
  load('rda/occupation.rda')
  data_set <- occupation
  rm(occupation)
  
  # load in age data
  print(paste("loading age data"))
  # https://www.nomisweb.co.uk/census/2011/ks102ew
  # import the new table with age data
  data <- ingest("ks102ew", geotype)
  #running the function on the data set
  age_predictors(data)
  #adding to main data set
  load('rda/age.rda')
  data_set <- data_set %>%
    left_join(age)
  rm(age)
  
  # next to load in the ethnicity data
  print(paste("loading ethnicity data"))
  # https://www.nomisweb.co.uk/census/2011/lc2101ew
  # updating with age and ethnicity
  data <- ingest("lc2101ew", geotype)
  #running the function on the data set
  ethnicity_predictors(data)
  #adding to main data set
  load('rda/ethnicity.rda')
  data_set <- data_set %>%
    left_join(ethnicity)
  rm(ethnicity)
  
  # next to load in the sex/gender data, for ages 25 to 64 
  print(paste("loading sex data"))
  # https://www.nomisweb.co.uk/census/2011/lc2101ew
  # using the ethnicity data, which includes gender
  data <- ingest("lc2101ew", geotype)
  #running the function on the data set
  sex_predictors(data)
  #adding to main data set
  load('rda/sex_25_64.rda')
  data_set <- data_set %>%
    left_join(sex_25_64)
  rm(sex_25_64)
  
  # qualifications
  print(paste("loading qualifications data"))
  # https://www.nomisweb.co.uk/census/2011/lc5102ew
  # import the new table with qualifications
  data <- ingest("lc5102ew", geotype)
  #running the function on the data set
  qualifications_predictors(data)
  #adding to main data set
  load('rda/qualifications.rda')
  data_set <- data_set %>%
    left_join(qualifications)
  rm(qualifications)
  
  # next marital status
  print(paste("loading marital data"))
  # https://www.nomisweb.co.uk/census/2011/lc1101ew
  # import the new table with marital
  data <- ingest("lc1101ew", geotype)
  #running the function on the data set
  marital_predictors(data)
  #adding to main data set
  load('rda/marital.rda')
  data_set <- data_set %>%
    left_join(marital)
  rm(marital)
  
  # next religion
  print(paste("loading religion data"))
  # https://www.nomisweb.co.uk/census/2011/lc2107ew
  # import the new table with religion
  data <- ingest("lc2107ew", geotype)
  #running the function on the data set
  religion_predictors(data)
  #adding to main data set
  load('rda/religion.rda')
  data_set <- data_set %>%
    left_join(religion)
  rm(religion)
  
  #next household composition
  print(paste("loading household data"))
  # https://www.nomisweb.co.uk/census/2011/lc1109ew
  # import the new table with household
  data <- ingest("lc1109ew", geotype)
  #running the function on the data set
  household_predictors(data)
  #adding to main data set
  load('rda/household.rda')
  data_set <- data_set %>%
    left_join(household)
  rm(household)
  
  #next Industry
  print(paste("loading industry data"))
  # https://www.nomisweb.co.uk/census/2011/lc6110ew
  # import the new table with Industry
  data <- ingest("lc6110ew", geotype)
  #running the function on the data set
  industry_predictors(data)  
  #adding to main data set
  load('rda/industry.rda')
  data_set <- data_set %>%
    left_join(industry)
  rm(industry)
  # names(data_set)
  
  # next country of birth
  print(paste("loading country data"))
  # https://www.nomisweb.co.uk/census/2011/lc2103ew
  # import the new table with country of birth
  data <- ingest("lc2103ew", geotype)
  #running the function on the country of birth data set
  country_predictors(data)
  #adding to main data set
  load('rda/country.rda')
  data_set <- data_set %>%
    left_join(country)
  rm(country, data)
  
  #save the data set
  filename <- paste("data_set", geotype, sep = "_") %>%
    paste0(., ".rda")
  destfile <- paste0("rda/", filename)
  print(paste("saving", destfile))
  save(data_set, file=destfile)
  #clean up
  rm(filename, destfile)
  return(data_set)
}

# test with the regional data vs the data created earlier
geographytype <- "TYPE480" #regions
# run the function to download the csv files for regions
download_censusdata("TYPE480", censusdata_list)
# run the function on the region imported data
data_set <- import_data("TYPE480")
#check function vs the main code
names(data_set)
names(data_set_maincode)
identical(data_set, data_set_maincode)
# true means that the functions are working

# run the function to download the csv files
geographytype="TYPE297"
download_censusdata("TYPE297", censusdata_list)
# run the function on the MSOA imported data
data_set <- import_data("TYPE297")
# data_set_TYPE297 <- data_set

#clean up
rm(data_set_maincode)
# rm(ingest, marital_predictors, country_predictors, occupation_target)
# rm(ethnicity_predictors, household_predictors, religion_predictors)
# rm(industry_predictors, sex_predictors)
# rm(qualifications_predictors, age_predictors)
rm(censusdata_list)

############ Create main set, validation set #####################

load("rda/data_set_TYPE297.rda")
names(data_set)
# Validation set will be 10% of the data
set.seed(2011, sample.kind="Rounding")
test_index <- createDataPartition(y = data_set$y, times = 1, p = 0.1, list = FALSE)
main <- data_set[-test_index,]
validation <- data_set[test_index,]
### tidy, take backup:
save(main, file='rda/main.rda')
save(validation, file='rda/validation.rda')
rm(validation, data_set, test_index)
# check later work
# main_check <- main
# validation_check <- validation
# load("rda/main.rda")
# identical(main, main_check)

############ exploration and visualisation ##########

head(main)
names(main)
#check what is in the geo_type feature
table(main$geo_type)
#make the occupation in an area a ratio
main <- main %>%
  mutate(occ_ratio = occupation_all/all_residents, .after = all_residents)

#graph the outcome y
main %>%  ggplot(aes(y))  + 
  geom_histogram(bins = 30, color = "black") +
  ggtitle("Distribution of the outcome, y") +
  xlab("y, proportions of people in managerial or professional occupations") +
  ylab("count of numbers of MSOA areas")
#table of key data points of the outcome y
main %>% group_by(geo_type) %>%
  summarise(mean = mean(y), sd = sd(y), max = max(y), min = min(y)) %>%
  select(-geo_type)  %>%
  knitr::kable()
# an alternate method, although less flexible for the markdown doc
summary(main$y)

#graph the geographic areas
main %>%  ggplot(aes(all_residents))  + 
  geom_histogram(bins = 30, color = "black") +
  ggtitle("Size of the geographic areas") +
  xlab("number of residents in the MSOA area") +
  ylab("count of numbers of MSOA areas")
#table of key data points of the sizes of the areas
main %>% group_by(geo_type) %>%
  summarise(mean = mean(all_residents), sd = sd(all_residents), max = max(all_residents), min = min(all_residents)) %>%
  select(-geo_type)  %>%
  knitr::kable()

#looking at the features
#box plot 
#create a "tidy" long form version for the tidyverse functions 
main_tidy <- main %>%  
  select(-occupation_all, -all_residents, -geo_type, -geo_name, -age_median) %>%
  #tidy the names, removing the _25_64
  rename_at(vars(contains("_25_64")), ~str_replace_all(., "_25_64", "")) %>%
  #pivot to a long version with a row per feature/value
  pivot_longer(cols = c(-geo_code, -y),
               names_to = "feature", 
               values_to = "proportion")
#plot the box plot
main_tidy %>%
  ggplot(aes(x=reorder(feature, proportion, FUN=mean), y=proportion))  + 
  geom_boxplot() +
  ggtitle("Boxplot of feature values") +
  ylab("Feature value, proportion of 25 to 64 in the area") + 
  xlab("Features, reordered by mean") + 
  theme(axis.text.x=element_text(angle = 90, hjust=1, vjust=0.5))

#choosing the 10 features with the lowest mean
main_tidy %>% group_by(feature) %>%
  summarise(mean=mean(proportion), sd=sd(proportion), max=max(proportion), min=min(proportion)) %>%
  arrange(mean) %>% head(10) %>%
  knitr::kable()

### calculating the correlation between features
correlationmatrix <- cor(main[,7:63])
# image of correlation matrix
heatmap(x = correlationmatrix, 
        col = RColorBrewer::brewer.pal(11, "Spectral"), 
        main="Heatmap of feature correlation with main data set")

# find attributes that are highly corrected
highlycorrelated <- findCorrelation(correlationmatrix, cutoff=0.8, exact = TRUE, names=TRUE)
highlycorrelated %>% knitr::kable()

#looking at the correlation for uk_25_64 specifically
data.frame(correlationmatrix) %>%
  select(uk_25_64) %>%
  filter(abs(uk_25_64) > 0.8) %>%
  knitr::kable()

#graph of features against uk_25_64
main %>%  pivot_longer(
  cols = (!"uk_25_64" & contains("25_64") | contains("ratio") ), 
  names_to = "feature", 
  values_to = "proportion")  %>%
  select(-occupation_all, -all_residents, -geo_type, -geo_name) %>%
  filter(feature %in% c("white_uk_25_64", "me_asia_25_64", "other_ethnicity_25_64", "other_qual_25_64")) %>%
  ggplot(aes(uk_25_64, proportion))  + 
  geom_point() + 
  facet_grid(. ~feature) +
  ggtitle("Correlation of features") +
  xlab("proportion for 'uk_25_64' feature") +
  ylab("proportion for other features")

# tidy
rm(correlationmatrix, highlycorrelated)
save(main_tidy, file='rda/main_tidy.rda')
rm(main_tidy)

#### geography ####
# getting the mapping for the locations
# https://borders.ukdataservice.ac.uk/lut_download_data.html?data=oa11_lsoa11_msoa11_lad11_ew_lu
#download file
geo_url <- "https://borders.ukdataservice.ac.uk/ukborders/lut_download/prebuilt/luts/engwal/OA11_LSOA11_MSOA11_LAD11_EW_LU.zip"
download.file(geo_url, 'data/OA11_LSOA11_MSOA11_LAD11_EW_LU.zip')
#unzip to data directory
unzip('data/OA11_LSOA11_MSOA11_LAD11_EW_LU.zip', exdir = "data/")
geo_mapping <- read.csv('data/OA11_LSOA11_MSOA11_LAD11_EW_LUv2.csv', fileEncoding="latin1")
save(geo_mapping, file='rda/geo_mapping.rda')
head(main$geo_code, 20)
head(geo_mapping)
# for the columns of interest are MSOA11CD (MSOA), OA11CD (SOA) and LAD11CD
load("rda/geo_mapping.rda")
geo_lookup <- geo_mapping %>%
  #for the MSOA and SOA
  select(LAD11CD, OA11CD, MSOA11CD, ) %>%
  rename(local_area = LAD11CD, geo_code_soa=OA11CD, geo_code_msoa = MSOA11CD) %>%
  unique()
head(geo_lookup)
# however, for many models to work, the feature needs to be numeric
# I need to convert the character strings to numbers, so remove the "E"
# calculating a list of strings within the local area to be rewritten as numbers
# keeping the strings replaced as long as possible for smaller area_code
# retaining a 3 digit long area code
area_update <- levels(as.factor(str_sub(geo_lookup$local_area, 1, -3)))
#create a new column to be updated
geo_lookup <- geo_lookup %>%
  mutate(area_code = local_area)
head(geo_lookup)
#create a for loop to go through this list and update the local areas
for(i in 1:length(area_update)){
  #the old string to replace
  oldprefix <- area_update[i]
  # the new string, a number, but as a character to work with the existing character
  newprefix <- as.character(i)
  geo_lookup$area_code <- str_replace_all(geo_lookup$area_code, oldprefix, newprefix)
}
head(geo_lookup)
# change the character string to be numerical
geo_lookup$area_code <- as.integer(geo_lookup$area_code)
#checking
head(geo_lookup$area_code)
length(levels(as.factor(geo_lookup$area_code)))
head(levels(as.factor(geo_lookup$area_code)))
# tmp save fpr checking
# save(geo_lookup, file='rda/geo_lookup_tmp.rda')

#but this may not work for some algorithms
# in addition creating a column per region
# https://opendata.arcgis.com/datasets/0aac9db88cbb4f8d9db42ad20b03cb71_0.csv
#download file
geo_url <- "https://opendata.arcgis.com/datasets/0aac9db88cbb4f8d9db42ad20b03cb71_0.csv"
download.file(geo_url, 'data/0aac9db88cbb4f8d9db42ad20b03cb71_0.csv')
# load in file
region_mapping<- read.csv('data/0aac9db88cbb4f8d9db42ad20b03cb71_0.csv')
save(region_mapping, file='rda/region_mapping.rda')
#checking the data
# load('rda/region_mapping.rda')
head(region_mapping)
levels(as.factor(region_mapping$RGN11NM))
# some have empty region name
# visual inspection shows that the unnamed regions are all Wales
# and the local area ids start with "W"
#there is a blank, for row 1, to be investigated
region_mapping %>%
  select(LAD11NM, LAD11CD, RGN11NM) %>%
  filter(RGN11NM == "") %>% unique()
# the blank row is the Isles of Scilly, which may or may not be in the data set
region_mapping %>%
  filter(LAD11CD == "")
#save just the local area and region
region_lookup <- region_mapping %>%
  rename(local_area = LAD11CD, region = RGN11NM) %>%
  select(local_area, region) %>%
  unique()
# check the regions, showing the blank
levels(as.factor(region_lookup$region))
table(region_lookup$region)
head(region_lookup)
# update empty fields to be "wales", where the local area starts with "W0"
# create an index of areas starting with W
index <- str_which(region_lookup$local_area, "W0")
# confirm it works
region_lookup[index,]
# add wales to the region column for those areas
region_lookup$region[index] <- "wales"
# check the regions, now including wales
levels(as.factor(region_lookup$region))
table(region_lookup$region)
#removing the blank line (Isles of Scilly)
region_lookup <- region_lookup %>%
  filter(region != "")
#confirm that the regions are now correct
levels(as.factor(region_lookup$region))
table(region_lookup$region)
# updating the names to be lower case, shorter, without spaces
region_lookup$region <- str_to_lower(region_lookup$region)
region_lookup$region <- str_replace_all(region_lookup$region, " and the ", "_")
region_lookup$region <- str_replace_all(region_lookup$region, " of ", "_")
region_lookup$region <- str_replace_all(region_lookup$region, " ", "_")
levels(as.factor(region_lookup$region))
#create a list of regions
region_list <- unique(region_lookup$region)
# create a table, with a column per region
# looping through, for each name in the region list
# if it matches the region assign a 1 
region_table <- sapply(region_list, function(region){
  ifelse(str_detect(region_lookup$region, region), 1, 0)
})
# checking it works as intended
head(region_table, 20)
head(region_lookup, 20)
tail(region_table, 20)
tail(region_lookup, 20)
# adding the columns to the region lookup
region_lookup <- region_lookup %>% cbind(as.data.frame(region_table))
# save completed lookup for later
save(region_lookup, file='rda/region_lookup.rda')
# checking
head(region_lookup, 50)
tail(region_lookup, 20)
head(geo_lookup)
sum(is.na(region_lookup))
sum(is.na(region_lookup))
#add to the geo_lookup file
geo_lookup <- geo_lookup %>%
  left_join(region_lookup) %>%
  select(-region)
#this has created some NAs, so need to fix
geo_lookup[is.na(geo_lookup)] <- 0
sum(is.na(geo_lookup))
head(geo_lookup)
#number of local areas
length(levels(as.factor(geo_lookup$area_code)))
#tidy up
save(geo_lookup, file='rda/geo_lookup.rda')

#tidy up
rm(geo_mapping, geo_url, area_update, index, i, newprefix, oldprefix, geo_lookup)
rm(region_lookup, region_mapping, region_table, region_list)

#a quick visual check to see whether regions are relevant
#graph including regions, need the regions from earlier
load("rda/region_lookup.rda")
load("rda/geo_lookup.rda")
# use the geo lookup and region from the region_lookup
geo_lookup %>% 
  mutate(geo_code=geo_code_msoa) %>%
  select(geo_code, local_area) %>%
  unique() %>%
  left_join(region_lookup, by = "local_area") %>%
  select(geo_code, region) %>%
  #add the main data with y
  right_join(main) %>%  
  #plot...
  ggplot(aes(y))  + 
  geom_histogram(aes(fill=region), bins = 30) +
  # geom_histogram(bins = 30) +
  ggtitle("Distribution of the outcome, y") +
  xlab("y, proportions of people in managerial or professional occupations") +
  ylab("count of numbers of MSOA areas")
#tidy
rm(geo_lookup, region_lookup)

#### create test and train set ####
load("rda/main.rda")
# load("rda/main_tidy.rda")
# test set will be 10% of the data
set.seed(2001, sample.kind="Rounding")
test_index <- createDataPartition(y = main$y, times = 1, p = 0.1, list = FALSE)
train_set <- main[-test_index,]
test_set <- main[test_index,]
#check
names(train_set)
sum(is.na(train_set))
### tidy, take backup:
save(train_set, file='rda/train_set.rda')
save(test_set, file='rda/test_set.rda')
rm(main, test_index)
# load("rda/test_set.rda")
# load("rda/train_set.rda")


#### data cleanse ####
# carry out the following
# * create ratio of occupied/all
# * create local area features
# * remove occupation & all_residents population numbers, geo name  & geo type
load("rda/geo_lookup.rda")
#currently working for MSOA and SOA
data_cleanse <- function(data_set_name, geosize){
  #generate geo mapping info
  load("rda/geo_lookup.rda")
  print(paste("working on", geosize))
  #choose the msoa geo_code
  if(geosize=="msoa"){
    print("msoa geolookup")
    geo_lookup <- geo_lookup %>%
      mutate(geo_code=geo_code_msoa)  %>%
      dplyr::select(-geo_code_soa, -geo_code_msoa) %>%
      unique()
  }
  #choose the soa geo_code
  if(geosize=="soa"){
    print("soa geolookup")
    geo_lookup <- geo_lookup %>%
      mutate(geo_code=geo_code_soa)  %>%
      dplyr::select(-geo_code_soa, -geo_code_msoa) %>%
      unique()
  }
  # modify the data
  tmp <- data_set_name %>%
    # add the occupation ratio
    dplyr::mutate(occ_ratio = 
                    occupation_all/all_residents, 
                  .after = all_residents) %>%
    # remove the unnecessary columns
    dplyr::select(-occupation_all, -all_residents, -geo_type, -geo_name) %>%
    # add the geographic region columns
    left_join(geo_lookup) %>%
    # remove the unnecessary geography columns
    dplyr::select(-local_area, -geo_code)
  #return the new object
  return(tmp)
}

#run on the test set and train set
test_set_final <- data_cleanse(test_set, "msoa")
train_set_final <- data_cleanse(train_set, "msoa")
# train_set_final3 <- data_cleanse(train_set, "msoa")
#check
names(test_set_final)
names(train_set_final)
head(test_set)
head(test_set_final)
sum(is.na(train_set_final))
test_set_final$wales
# identical(train_set_final, train_set_final2)
# train_set_final2 <- train_set_final
# sum(is.na(train_set_final2))
#check the number of local areas is correct
length(levels(as.factor(train_set_final$area_code)))
# tidy up
save(test_set_final, file="rda/test_set_final.rda")
save(train_set_final, file="rda/train_set_final.rda")
rm(test_set, train_set, geo_lookup, main)
rm(train_set_final, train_set_final2)

#### baseline rmse and rmse function ####
#load the test and train data
load("rda/test_set_final.rda")
load("rda/train_set_final.rda")
options(digits = 6)
#create RMSE function
rmse <- function(true_proportions, predicted_proportions){
  sqrt(mean((true_proportions - predicted_proportions)^2))
}
# baseline RMSE with an average of all ratings
mu_hat <- mean(train_set_final$y)
rmse_ave <- rmse(test_set_final$y, mu_hat)
rmse_results <- tibble(method = "Mean of all locations", rmse = rmse_ave)
rmse_results %>% knitr::kable(caption="Initial baseline RMSE on an SOA with a mean estimate")
# tidy
rm(mu_hat, rmse_ave)


#### modelling ####
# Before starting, reduce the size and remove the potentially difficult to handle categorical data on location.

##### feature selection & training sets ####
# <!-- https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/ -->

# low variation
# choosing the features as candidates to remove
low_variability <- train_set_final %>%  
  #pivot to a long version with a row per feature/value
  pivot_longer(cols = (contains("25_64") | contains("ratio")), 
               names_to = "feature", 
               values_to = "proportion") %>%
  group_by(feature) %>%
  summarise(mean=mean(proportion), sd=sd(proportion), max=max(proportion), min=min(proportion)) %>%
  filter(mean <= "0.05" & sd <= "0.015" & max <= "0.15")
low_variability %>%
  arrange(mean) %>%
  knitr::kable(caption="Summary of features with low variability")
# creating a list for later...
low_variability_list <- low_variability %>% .$feature

# histogram of potential features to lose
train_set_final %>%  
  #pivot to a long version with a row per feature/value
  pivot_longer(cols = (contains("25_64") | contains("ratio")), 
               names_to = "feature", 
               values_to = "proportion") %>%
  filter(feature %in% low_variability_list) %>% 
  ggplot(aes(proportion, fill=feature))  + 
  geom_histogram(bins = 30) +
  ggtitle("Distribution of proportions for least varying features") +
  ylab("count of numbers of MSOA areas")

# categorical data
#this is the location data
load("rda/geo_lookup.rda")
head(geo_lookup)
names(geo_lookup)
categorical_features <- names(geo_lookup[5:14])
categorical_features

#highly correlated
names(train_set_final)
correlationmatrix <- cor(train_set_final[,3:69])
# image of correlation matrix
heatmap(x = correlationmatrix, 
        col = RColorBrewer::brewer.pal(11, "Spectral"), 
        main="Heatmap of feature correlaton with the train_set_final")

# find attributes that are highly corrected
highlycorrelated <- findCorrelation(correlationmatrix, cutoff=0.8, exact = TRUE, names=TRUE)
#listing the correlations for a specific one of the selected features, say, 2, and 3
#create index for the feature, and show features highly correlated with feature 2 and 3 
highlycorrelated[3]
index <- str_which(names(data.frame(correlationmatrix)), highlycorrelated[4])
correlationmatrix[,index][abs(correlationmatrix[,index]) > 0.7]
highlycorrelated[2]
index <- str_which(names(data.frame(correlationmatrix)), highlycorrelated[2])
correlationmatrix[,index][abs(correlationmatrix[,index]) > 0.7]
data.frame(correlationmatrix[,index][abs(correlationmatrix[,index]) > 0.7]) %>%
  rename("correlation" = starts_with("correlationmatri")) %>%
  rownames_to_column(var="feature name") %>%
  knitr::kable(caption="Highly correlated features with the 'white_uk' feature")
#tidy up
save(highlycorrelated, file="rda/highlycorrelated.rda")
save(categorical_features, file="rda/categorical_features.rda")
save(low_variability_list, file="rda/low_variability_list.rda")
rm(low_variability, geo_lookup)
rm(index, correlationmatrix)

## create smaller data set, and remove the geo_code column
#remove low variability, highly correlated and categorical
low_variability_list  %>% knitr::kable()
highlycorrelated %>%  knitr::kable()
categorical_features  %>%  knitr::kable()
#remove low variability, highly correlated and categorical columns
train_small <- train_set_final %>%
  select(-all_of(low_variability_list)) %>%
  select(-all_of(highlycorrelated)) %>%
  select(-all_of(categorical_features))
names(train_small)
names(train_set_final)

#make a smaller set, 15 columns and 1000 rows
#sampling the 1000 observations
set.seed(2001, sample.kind="Rounding")
index <- sample(1:nrow(train_small), 1000, replace = FALSE)
train_smaller <- train_small[index,]
# there are 35 features numbers 2:36 inclusive.
# creating the index, 15 from 35 plus 1
set.seed(1984, sample.kind="Rounding")
#create the sample of 15 features
index <- sample(1:(ncol(train_small)-1), 15, replace = FALSE)
# as the first column is y, I need to keep that column
# add 1 to all the index values, to retain the y values
index2 <- append(1, (index+1))
train_smaller <- train_smaller[,index2]

#tidy up
rm(index, index2)
save(train_small, file="rda/train_small.rda")
save(train_smaller, file="rda/train_smaller.rda")
# load("rda/train_smaller.rda")
names(train_small)
names(train_smaller)
# then the same but with categorical variables
#remove low variability, highly correlated columns
train_small_cat <- train_set_final %>%
  select(-all_of(low_variability_list)) %>%
  select(-all_of(highlycorrelated))
names(train_small_cat)

#make a smaller set, 15 columns and 1000 rows
#sampling the 1000 observations
set.seed(2001, sample.kind="Rounding")
index <- sample(1:nrow(train_small_cat), 1000, replace = FALSE)
train_smaller_cat <- train_small_cat[index,]
# there are 35 features numbers 2:36 inclusive.
# creating the index, 15 from 35 plus 1
set.seed(1984, sample.kind="Rounding")
#create the sample of 15 features (random, but a manual check shows two categorical variables were chosen)
index <- sample(1:(ncol(train_small_cat)-1), 15, replace = FALSE)
# as the first column is y, I need to keep that column
# add 1 to all the index values, to retain the y values
index2 <- append(1, (index+1))
train_smaller_cat <- train_smaller_cat[,index2]
#tidy up
rm(index, index2)
save(train_small_cat, file="rda/train_small_cat.rda")
save(train_smaller_cat, file="rda/train_smaller_cat.rda")
names(train_small_cat)
names(train_smaller_cat)

# create a full training set without the categorical features
#remove categorical columns
train_final_nocat <- train_set_final %>%
  select(-all_of(categorical_features))
names(train_final_nocat)
# tidy up
save(train_final_nocat, file="rda/train_final_nocat.rda")
rm(categorical_features, low_variability_list, highlycorrelated)


#### function for running the models ####
# creating a function to run the models, with different algorithms and data sets, with default settings.
results_train_method <- function(traindata, testdata, algorithm){
  trainname <- paste("train", deparse(substitute(algorithm)), deparse(substitute(traindata)), sep = "_")
  print(paste0("running ", trainname))
  #setting the random seed
  set.seed(2011, sample.kind="Rounding")
  #run the model training on the training data
  train_obj <- train(y ~ ., method = algorithm, data = traindata)
  # making a prediction on the test set
  yhat_obj <- predict(train_obj, testdata)
  # calculating the rmse
  rmse_obj <- rmse(testdata$y, yhat_obj)
  print(paste("rmse result is", rmse_obj))
  #adding to the RMSE list
  resultname <- paste0(deparse(substitute(algorithm)), " - ", deparse(substitute(traindata)))
  print(paste("creating result list for", resultname))
  rmse_results <- tibble(method = resultname, rmse = rmse_obj)
  #saving the results
  #returning the training model, y_hats, and rmse results
  my_list <- list("train" = train_obj, "y_hat" = yhat_obj, "results" = rmse_results)
  return(my_list) 
  # return(rmse_results)
}
load("rda/train_small.rda")
load("rda/train_smaller.rda")
load("rda/train_small_cat.rda")
load("rda/train_smaller_cat.rda")
load("rda/train_final_nocat.rda")
load("rda/test_set_final.rda")
load("rda/train_set_final.rda")


#### Generalized linear model - glm ####
# initial the straightforward glm model
# with the smaller training set
result_glm_train_smaller <- 
  results_train_method(train_smaller, test_set_final,"glm")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_glm_train_smaller$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance
importance <- varImp(result_glm_train_smaller$train, scale=FALSE)
plot(importance, 20)

# with the small train set
result_glm_train_small <- 
  results_train_method(train_small, test_set_final, "glm")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_glm_train_small$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance
importance <- varImp(result_glm_train_small$train, scale=FALSE)
plot(importance, 20)

# with the full train set, less categorical
result_glm_train_final_nocat <- 
  results_train_method(train_final_nocat, test_set_final, "glm")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_glm_train_final_nocat$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance
importance <- varImp(result_glm_train_final_nocat$train, scale=FALSE)
plot(importance, 20)

# test if it will work with categorical feature the the smaller_cat
train_glm <- train(y ~ ., method = "glm", data = train_smaller_cat)
# glm does work...

# with the full train set
result_glm_train_set_final <- 
  results_train_method(train_set_final, test_set_final, "glm")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_glm_train_set_final$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance
importance <- varImp(result_glm_train_set_final$train, scale=FALSE)
plot(importance, 20)
importance$importance

#plotting important features vs y
# important_features <- c("level4_25_64", "age_median", "oneperson_25_64", "loneparent_25_6", "cohoabiting_25_64", "construction_25_64",  "eu_rest_25_64")
# important_features <- c("level4", "age_median", "loneparent", "cohoabiting", "construction",  "eu_rest", "no_qual",  "bangladeshi", "black_african")
important_features <- data.frame(importance$importance) %>%
  rownames_to_column(var="feature") %>%
  mutate(feature = str_replace_all(.$feature,"_25_64", "")) %>%
  arrange(desc(Overall)) %>%
  head(9) %>%
  pull(feature)
#taking the training data and making long form for a graph
train_set_final %>%
  # tidy the names
  rename_at(vars(ends_with("25_64")), ~str_replace_all(., "_25_64", "")) %>%
  #pivot to a long version with a row per feature/value
  pivot_longer(cols = !"y", 
               names_to = "feature", 
               values_to = "value") %>%
  filter(feature %in% important_features) %>% 
  ggplot(aes(y, value, col=feature))  + 
  geom_point(size=0.5) +
  facet_wrap(. ~feature,  scales = "free_y") +
  ggtitle("Outcome y for important features for GLM") +
  xlab("y, proportion of managerial & professional") +
  ylab("feature values")

# tidy up
importance_glm <- importance 
save(result_glm_train_set_final, file="rda/result_glm_train_set_final.rda")
save(importance_glm, file="rda/importance_glm.rda")
save(rmse_results, file="rda/rmse_results.rda")
rm(train_glm, important_features, importance, importance_glm)
rm(result_glm_train_final_nocat, result_glm_train_small)
rm(result_glm_train_smaller, result_glm_train_set_final)


#### K nearest neighbours - knn ####
# with the smaller training set
result_knn_train_smaller <- 
  results_train_method(train_smaller, test_set_final, "knn")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_knn_train_smaller$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance, not working
importance <- varImp(result_knn_train_smaller$train, scale=FALSE)
# check the model
result_knn_train_smaller$train$finalModel

# with the small train set, less categorical
result_knn_train_small <- 
  results_train_method(train_small, test_set_final, "knn")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_knn_train_small$results, 1))
rmse_results %>% knitr::kable()

# with the full train set, less categorical
result_knn_train_final_nocat <- 
  results_train_method(train_final_nocat, test_set_final, "knn")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_knn_train_final_nocat$results, 1))
rmse_results %>% knitr::kable()

# test if it will work with categorical feature the the smaller_cat
result_knn_train_smaller_cat <- 
  results_train_method(train_smaller_cat, test_set_final, "knn")
# knn does work, but very poor performance, not much better than the mean...

#making a new small training set, without the age and area
train_set_knn_small <- train_small %>%
  select(-age_median, -area_code)
#checking the max, should be less than 1
max(train_set_knn_small[1,])

#check with the new knn small training set
result_knn_train_set_knn_small <- 
  results_train_method(train_set_knn_small, test_set_final, "knn")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_knn_train_set_knn_small$results, 1))
rmse_results %>% knitr::kable()

#making a new final training set, no categorical, without the age and area
train_set_knn_final_nocat <- train_final_nocat %>%
  select(-age_median, -area_code)
#checking the max, should be 1
max(train_set_knn_final_nocat[1,])

#check with the new training set
result_knn_train_set_knn_final_nocat <- 
  results_train_method(train_set_knn_final_nocat, test_set_final, "knn")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_knn_train_set_knn_final_nocat$results, 1))
rmse_results %>% knitr::kable()

#normalising the data, to be between 0 and 1
#look at some of the data, show the max and min
summary(train_final_nocat[,4:7])
#create a function to normalise
normalise <- function(feature) {
  return ((feature - min(feature)) / (max(feature) - min(feature))) }
#create a new training set, removing the y value and adding back in
train_final_norm <- train_final_nocat[1] %>%
  cbind(as.data.frame(lapply(train_final_nocat[2:59], normalise)))
#check the data again
identical(names(train_final_norm), names(train_final_nocat))
print("after normalising")
summary(train_final_norm[,4:7])

#check with the new normalised training set
result_knn_train_final_norm <- 
  results_train_method(train_final_norm, test_set_final, "knn")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_knn_train_final_norm$results, 1))
rmse_results %>% knitr::kable()

#checking the tuning for the best model
plot(result_knn_train_set_knn_final_nocat$train)
# this shows 9 neighbours is not the bottom of the curve
# trying more neighbours
# load("rda/train_knn.rda")
train_knn <- train(y ~ ., method = "knn", data = train_set_knn_final_nocat, tuneLength=7)
# train_knnsmaller <- train(y ~ ., method = "knn", data = train_smaller, preProcess = c("center","scale"), tuneLength=7)
# 13 neighbours is best
plot(train_knn)
#making the prediction
yhat_knn <- predict(train_knn, test_set_final)
# yhat_knn <- predict(train_knnsmaller, test_set_final)
# calculating the rmse
rmse_knn <- rmse(test_set_final$y, yhat_knn)
#adding to the RMSE list
rmse_results <- bind_rows(rmse_results,
                          tibble(method='"knn" - train_set_knn_final_nocat, k=13',  
                                 rmse = rmse_knn))
rmse_results %>% tail(5) %>% knitr::kable()


# tidy up
save(rmse_results, file="rda/rmse_results.rda")
save(train_knn, file="rda/train_knn.rda")
save(train_set_knn_small, file="rda/train_set_knn_small.rda")
save(train_set_knn_final_nocat, file="rda/train_set_knn_final_nocat.rda")
save(result_knn_train_set_knn_final_nocat, 
     file="rda/result_knn_train_set_knn_final_nocat.rda")
rm(result_knn_train_final_norm, result_knn_train_set_knn_final_nocat)
rm(result_knn_train_smaller, result_knn_train_small)
rm(result_knn_train_set_knn_small)
rm(result_knn_train_final_nocat, result_knn_train_smaller_cat)
rm(train_knn, train_knnsmaller, train_set_knn_final_nocat)
rm(rmse_knn, yhat_knn)
rm(train_set_knn_small, train_final_norm, normalise)


#### naive_bayes ####
# *** an error "Error: wrong model type for regression"
# this is for classification, not continuous problems
# with the smaller training set
set.seed(2011, sample.kind="Rounding")
train_nb <- train(y ~ ., method = "naive_bayes", data = train_smaller)


#### CART - rpart ####
# uses the rpart package
if (!require('rpart')) install.packages('rpart'); library('rpart')

# with the smaller training set
result_rpart_train_smaller <- 
  results_train_method(train_smaller, test_set_final, "rpart")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_rpart_train_smaller$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance
importance <- varImp(result_rpart_train_smaller$train, scale=FALSE)
plot(importance, 20)
# looking at the model, it is simply based on the no_qualification feature
result_rpart_train_smaller$train$finalModel

# with the small training set
result_rpart_train_small <- 
  results_train_method(train_small, test_set_final, "rpart")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_rpart_train_small$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance
importance <- varImp(result_rpart_train_small$train, scale=FALSE)
plot(importance, 20)
result_rpart_train_small$train$finalModel

# with the final set
result_rpart_train_set_final <- 
  results_train_method(train_set_final, test_set_final, "rpart")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_rpart_train_set_final$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance
importance <- varImp(result_rpart_train_set_final$train, scale=FALSE)
plot(importance, 20)
result_rpart_train_set_final$train$finalModel

#tidy
save(rmse_results, file="rda/rmse_results.rda")
rm(result_rpart_train_smaller, importance, result_rpart_train_small)
rm(result_rpart_train_set_final)


#### Support Vector Machines - svmLinear ####
# using the kernlab package
if (!require('kernlab')) install.packages('kernlab'); library('kernlab')

# with the smaller training set
result_svm_train_smaller <- 
  results_train_method(train_smaller, test_set_final, "svmLinear")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_svm_train_smaller$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance, doesn't work
importance <- varImp(result_svm_train_smaller$train, scale=FALSE)

# with the small training set
result_svm_train_small <- 
  results_train_method(train_small, test_set_final, "svmLinear")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_svm_train_small$results, 1))
rmse_results %>% knitr::kable()

# with the full train set, less categorical
result_svm_train_final_nocat <- 
  results_train_method(train_final_nocat, test_set_final, "svmLinear")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_svm_train_final_nocat$results, 1))
rmse_results %>% knitr::kable()

# test if it will work with categorical feature the the smaller_cat
train_svm <- train(y ~ ., method = "svmLinear", data = train_smaller_cat)
# svm does work...

# with the full training set
result_svm_train_set_final <- 
  results_train_method(train_set_final, test_set_final, "svmLinear")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_svm_train_set_final$results, 1))
rmse_results %>% knitr::kable()

#tidy
save(result_svm_train_set_final, file="rda/result_svm_train_set_final.rda")
save(rmse_results, file="rda/rmse_results.rda")
rm(train_svm, result_svm_train_smaller, result_svm_train_small)
rm(result_svm_train_final_nocat, result_svm_train_set_final)


#### Stochastic Gradient Boosting	- gbm ####
# using the gbm plyr, package
if (!require('gbm')) install.packages('gbm'); library('gbm')
if (!require('plyr')) install.packages('plyr'); library('plyr')

# with the smaller training set
result_gbm_train_smaller <- 
  results_train_method(train_smaller, test_set_final, "gbm")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_gbm_train_smaller$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance
importance <- varImp(result_gbm_train_smaller$train, scale=FALSE)
plot(importance, 20)
#checking the model
result_gbm_train_smaller$train$finalModel
plot(result_gbm_train_smaller$train)

# with the small train set
result_gbm_train_small <- 
  results_train_method(train_small, test_set_final, "gbm")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_gbm_train_small$results, 1))
rmse_results %>% knitr::kable()

# with the full train set, less categorical
result_gbm_train_final_nocat <- 
  results_train_method(train_final_nocat, test_set_final, "gbm")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_gbm_train_final_nocat$results, 1))
rmse_results %>% knitr::kable()

# with the full train set
result_gbm_train_set_final <- 
  results_train_method(train_set_final, test_set_final, "gbm")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_gbm_train_set_final$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance
importance <- varImp(result_gbm_train_set_final$train, scale=FALSE)
plot(importance, 20)
#checking the tuning, seems ok
plot(result_gbm_train_set_final$train)

#tidy
importance_gbm <- importance
save(importance_gbm, file="rda/importance_gbm.rda")
save(result_gbm_train_set_final, file="rda/result_gbm_train_set_final.rda")
# load("rda/result_gbm_train_set_final.rda")
save(rmse_results, file="rda/rmse_results.rda")
rm(result_gbm_train_smaller, result_gbm_train_small)
rm(result_gbm_train_final_nocat, result_gbm_train_set_final)
rm(importance_gbm, importance)

#### Multilayer Perceptrons - mlp ####
#
#  will not run this, as the RSNNS package overwrites caret and causes problems
#
# if (!require('RSNNS')) install.packages('RSNNS'); library('RSNNS')
# require(RSNNS)
# library(RSNNS)
# install.packages('caret')
# library(caret)
#tricky, RSNNS overwrites caret's train, so need to reinstall caret and re load it from the library after installing RSNNS
# with the smaller training set
# set.seed(2011, sample.kind="Rounding")
# train_mlp <- train(y ~ ., method = "mlp", data = train_smaller)
# # importance <- varImp(train_mlp, scale=FALSE)
# # plot(importance, 20)
# # making a prediction on the test set:
# y_hat_mlp <- predict(train_mlp, test_set_final)
# rmse_mlp <- rmse(test_set_final$y, y_hat_mlp)
# rmse_results <- bind_rows(rmse_results,
#                           tibble(method="MLP - smaller train set",  
#                                  rmse = rmse_mlp))
# rmse_results %>% knitr::kable()
# 
# # with the small train set
# set.seed(2011, sample.kind="Rounding")
# train_mlp <- train(y ~ ., method = "mlp", data = train_small)
# # making a prediction on the test set:
# y_hat_mlp <- predict(train_mlp, test_set_final)
# rmse_mlp <- rmse(test_set_final$y, y_hat_mlp)
# rmse_results <- bind_rows(rmse_results,
#                           tibble(method="MLP - small train set",  
#                                  rmse = rmse_mlp))
# rmse_results %>% knitr::kable()
# #tidy
# save(rmse_results, file="rda/rmse_results.rda")
# rm(train_mlp, rmse_mlp, y_hat_mlp)


#### Generalized Additive Model using Splines - gam ####
# this uses the mgcv and nlme packages
if (!require('nlme')) install.packages('nlme'); library('nlme')
if (!require('mgcv')) install.packages('mgcv'); library('mgcv')

# with the smaller training set
result_gam_train_smaller <- 
  results_train_method(train_smaller, test_set_final, "gam")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_gam_train_smaller$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance
importance <- varImp(result_gam_train_smaller$train, scale=FALSE)
plot(importance, 20)
result_gam_train_smaller$train$finalModel

# with the small training set
# result_gam_train_small <- results_train_method(train_small, test_set_final, "gam")
# did not complete, took too long

#tidy
save(rmse_results, file="rda/rmse_results.rda")
rm(result_gam_train_smaller, importance)


#### Least Absolute Shrinkage and Selection Operator - lasso ####
# this uses the lasso package
if (!require('elasticnet')) install.packages('elasticnet'); library('elasticnet')

# with the smaller training set
result_lasso_train_smaller <- 
  results_train_method(train_smaller, test_set_final, "lasso")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_lasso_train_smaller$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance, doesn't work
importance <- varImp(result_lasso_train_smaller$train, scale=FALSE)
result_lasso_train_smaller$train$finalModel

# with the small training set
result_lasso_train_small <- 
  results_train_method(train_small, test_set_final, "lasso")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_lasso_train_small$results, 1))
rmse_results %>% knitr::kable()

# with the full train set, less categorical
# with the small training set
result_lasso_train_final_nocat <- 
  results_train_method(train_final_nocat, test_set_final, "lasso")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results,
                          tail(result_lasso_train_final_nocat$results, 1))
rmse_results %>% knitr::kable()

# does it work with categorical?
train_lasso <- train(y ~ ., method = "lasso", data = train_smaller_cat)
#yes it does

#compare glm and lasso predictions
result_lasso_train_set_final <- 
  results_train_method(train_set_final, test_set_final, "lasso")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results,
                          tail(result_lasso_train_set_final$results, 1))
result_lasso_train_set_final$results$rmse
# load("rda/result_lasso_train_set_final.rda")
load("rda/result_glm_train_set_final.rda")
result_glm_train_set_final$results$rmse
# check whether the RMSE are identical, or almost
identical(result_lasso_train_set_final$results$rmse,
          result_glm_train_set_final$results$rmse)
all.equal(result_lasso_train_set_final$results$rmse,
          result_glm_train_set_final$results$rmse)
# check whether the y_hat are identical, or almost
identical(result_lasso_train_set_final$y_hat,
          result_glm_train_set_final$y_hat)
all.equal(result_lasso_train_set_final$y_hat,
          result_glm_train_set_final$y_hat)
# plotting graphs
data.frame(result_lasso_train_set_final$y_hat,
           result_glm_train_set_final$y_hat) %>%
  ggplot(aes(x=result_lasso_train_set_final$y_hat, 
             y=result_glm_train_set_final$y_hat)) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col="blue")  +
  ggtitle("Comparing the GLM and LASSO algorithms") +
  xlab("LASSO predictions") +
  ylab("GLM predictions")

#tidy
save(result_lasso_train_set_final, file="rda/result_lasso_train_set_final.rda")
rm(train_lasso, result_lasso_train_smaller, result_lasso_train_small) 
rm(result_lasso_train_set_final, result_lasso_train_final_nocat)
rm(result_glm_train_set_final)
save(rmse_results, file="rda/rmse_results.rda")


#### Principal Component Analysis	- pcr ####
# uses package "pls"
if (!require('pls')) install.packages('pls'); library('pls')

# with the smaller training set
result_pcr_train_smaller <- 
  results_train_method(train_smaller, test_set_final, "pcr")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_pcr_train_smaller$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance, doesn't work
importance <- varImp(result_pcr_train_smaller$train, scale=FALSE)
result_pcr_train_smaller$train$modelInfo
plot(result_pcr_train_smaller$train)

# with the small training set
result_pcr_train_small <- 
  results_train_method(train_small, test_set_final, "pcr")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_pcr_train_small$results, 1))
rmse_results %>% knitr::kable()

#test the categorical
result_pcr_train_smaller_cat <- 
  results_train_method(train_smaller_cat, test_set_final, "pcr")
# it works

# with the full train set
result_pcr_train_set_final <- 
  results_train_method(train_set_final, test_set_final, "pcr")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_pcr_train_set_final$results, 1))
rmse_results %>% knitr::kable()
#performs badly though

#tidy
rm(result_pcr_train_smaller, result_pcr_train_small)
rm(result_pcr_train_smaller_cat, result_pcr_train_set_final)
save(rmse_results, file="rda/rmse_results.rda")


#### Bayesian Generalized Linear Model - bayesglm ####
# uses the arm package
if (!require('arm')) install.packages('arm'); library('arm')

# with the smaller training set
result_bayesglm_train_smaller <- 
  results_train_method(train_smaller, test_set_final, "bayesglm")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_bayesglm_train_smaller$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance, doesn't work
importance <- varImp(result_bayesglm_train_smaller$train, scale=FALSE)
result_bayesglm_train_smaller$train$finalModel

# with the small training set
result_bayesglm_train_small <- 
  results_train_method(train_small, test_set_final, "bayesglm")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_bayesglm_train_small$results, 1))
rmse_results %>% knitr::kable()
#checking the final model
result_bayesglm_train_small$train$finalModel

# seems similar to the GLM
# checking...
# compare glm and bayesglm predictions
result_bayesglm_train_set_final <- 
  results_train_method(train_set_final, test_set_final, "bayesglm")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_bayesglm_train_set_final$results, 1))
rmse_results %>% knitr::kable()
# load("rda/result_bayesglm_train_set_final.rda")
load("rda/result_glm_train_set_final.rda")
result_glm_train_set_final$results$rmse
# check whether the RMSE are identical, or almost
identical(result_bayesglm_train_set_final$results$rmse,
          result_glm_train_set_final$results$rmse)
all.equal(result_bayesglm_train_set_final$results$rmse,
          result_glm_train_set_final$results$rmse)
# check whether the y_hat are identical, or almost
identical(result_bayesglm_train_set_final$y_hat,
          result_glm_train_set_final$y_hat)
all.equal(result_bayesglm_train_set_final$y_hat,
          result_glm_train_set_final$y_hat)
# plotting graphs
data.frame(result_bayesglm_train_set_final$y_hat,
           result_glm_train_set_final$y_hat) %>%
  ggplot(aes(x=result_bayesglm_train_set_final$y_hat, 
             y=result_glm_train_set_final$y_hat)) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col="blue")  +
  ggtitle("Comparing the BayesGLM and GLM algorithms") +
  xlab("BayesGLM predictions") +
  ylab("GLM predictions")

#tidy
rm(result_bayesglm_train_smaller)
rm(result_bayesglm_train_set_final, result_bayesglm_train_small)
rm(result_glm_train_set_final)
save(rmse_results, file="rda/rmse_results.rda")


#### Generalized Additive Model using LOESS - gamLoess ####
# uses the gam package
if (!require('gam')) install.packages('gam'); library('gam')

# with the smaller training set
result_gamloess_train_smaller <- 
  results_train_method(train_smaller, test_set_final, "gamLoess")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results,
                          tail(result_gamloess_train_smaller$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance
importance <- varImp(result_gamloess_train_smaller$train, scale=FALSE)
plot(importance, 20)
result_gamloess_train_smaller$train

# with the small training set
# result_gamloess_train_small <- 
  # results_train_method(train_small, test_set_final, "gamLoess")
# did not complete

#tidy
save(rmse_results, file="rda/rmse_results.rda")
rm(result_gamloess_train_smaller, importance)


#### Random Forest - ranger ####
# uses the e1071, ranger, dplyr packages
if (!require('ranger')) install.packages('ranger'); library('ranger')
if (!require('e1071')) install.packages('e1071'); library('e1071')?
  
# with the smaller training set
result_ranger_train_smaller <- 
  results_train_method(train_smaller, test_set_final, "ranger")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results,
                          tail(result_ranger_train_smaller$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance, not present
importance <- varImp(result_ranger_train_smaller$train, scale=FALSE)
result_ranger_train_smaller$train$finalModel

# with the small training set
result_ranger_train_small <- 
  results_train_method(train_small, test_set_final, "ranger")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results,
                          tail(result_ranger_train_small$results, 1))
rmse_results %>% knitr::kable()

# with the full training set
result_ranger_train_set_final <- 
  results_train_method(train_set_final, test_set_final, "ranger")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results,
                          tail(result_ranger_train_set_final$results, 1))
rmse_results %>% knitr::kable()

#checking the tuning, seems ok
plot(result_ranger_train_set_final$train)

#tidy
save(result_ranger_train_set_final, file="rda/result_ranger_train_set_final.rda")
rm(result_ranger_train_set_final, result_ranger_train_small) 
rm(result_ranger_train_smaller)
save(rmse_results, file="rda/rmse_results.rda")


#### Boosted Generalized Linear Model - glmboost ####
# uses the plyr, mboost packages
if (!require('mboost')) install.packages('mboost'); library('mboost')
if (!require('plyr')) install.packages('plyr'); library('plyr')

# with the smaller training set
result_glmboost_train_smaller <- 
  results_train_method(train_smaller, test_set_final, "glmboost")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results,
                          tail(result_glmboost_train_smaller$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance
importance <- varImp(result_glmboost_train_smaller$train, scale=FALSE)
plot(importance, 20)
result_glmboost_train_smaller$train$finalModel

# with the small training set
result_glmboost_train_small <- 
  results_train_method(train_small, test_set_final, "glmboost")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results,
                          tail(result_glmboost_train_small$results, 1))
rmse_results %>% knitr::kable()

#test the categorical
result_glmboost_train_smaller_cat <- 
  results_train_method(train_smaller_cat, test_set_final, "glmboost")
# it works

# with the full training set
result_glmboost_train_set_final <- 
  results_train_method(train_set_final, test_set_final, "glmboost")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results,
                          tail(result_glmboost_train_set_final$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance
importance <- varImp(result_glmboost_train_set_final$train, scale=FALSE)
plot(importance, 10)

#checking tuning
plot(result_glmboost_train_set_final$train)

#tidy
importance_glmboost <- importance 
save(result_glmboost_train_set_final, file="rda/result_glmboost_train_set_final.rda")
save(importance_glmboost, file="rda/importance_glmboost.rda")
# load("rda/importance_glmboost.rda")
save(rmse_results, file="rda/rmse_results.rda")
# load("rda/result_glmboost_train_set_final.rda")
rm(importance, importance_glmboost)
rm(result_glmboost_train_smaller, result_glmboost_train_small) 
rm(result_glmboost_train_smaller_cat, result_glmboost_train_set_final) 


#### Model Averaged Neural Network - avNNet ####
# uses the nnet package
if (!require('nnet')) install.packages('nnet'); library('nnet')

# with the smaller training set
result_avnnet_train_smaller <- 
  results_train_method(train_smaller, test_set_final, "avNNet")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results,
                          tail(result_avnnet_train_smaller$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance, not there
importance <- varImp(result_avnnet_train_smaller$train, scale=FALSE)
#check the model
result_avnnet_train_smaller$train$finalModel

# with the small training set
result_avnnet_train_small <- 
  results_train_method(train_small, test_set_final, "avNNet")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results,
                          tail(result_avnnet_train_small$results, 1))
rmse_results %>% knitr::kable()

#test the categorical
result_avnnet_train_smaller_cat <- 
  results_train_method(train_smaller_cat, test_set_final, "avNNet")
# it works

# with the full train set
result_avnnet_train_set_final <- 
  results_train_method(train_set_final, test_set_final, "avNNet")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results,
                          tail(result_avnnet_train_set_final$results, 1))
rmse_results %>% knitr::kable()

# checking model tuning
plot(result_avnnet_train_set_final$train)
result_avnnet_train_set_final$train
result_avnnet_train_set_final$train$finalModel

#tidy 
save(rmse_results, file="rda/rmse_results.rda")
save(result_avnnet_train_set_final, file="rda/result_avnnet_train_set_final.rda")
# load("rda/result_avnnet_train_set_final.rda")
rm(result_avnnet_train_small, result_avnnet_train_smaller) 
rm(result_avnnet_train_smaller_cat, result_avnnet_train_set_final)


#### Support Vector Machines with Linear Kernel	- svmLinear2 ####
# uses the e1071 package
if (!require('e1071')) install.packages('e1071'); library('e1071')

# with the smaller training set
result_svm2_train_smaller <- 
  results_train_method(train_smaller, test_set_final, "svmLinear2")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results,
                          tail(result_svm2_train_smaller$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance, not there
importance <- varImp(result_svm2_train_smaller$train, scale=FALSE)

# with the small training set
result_svm2_train_small <- 
  results_train_method(train_small, test_set_final, "svmLinear2")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results,
                          tail(result_svm2_train_small$results, 1))
rmse_results %>% knitr::kable()

#test the categorical
result_svm2_train_smaller_cat <- 
  results_train_method(train_smaller_cat, test_set_final, "svmLinear2")
# it works

# seems similar to the svmlinear
# checking...
# compare svmlinear and svmlinear2 predictions
result_svm2_train_set_final <- 
  results_train_method(train_set_final, test_set_final, "svmLinear2")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_svm2_train_set_final$results, 1))
rmse_results %>% knitr::kable()

#comparing...
# load("rda/result_svm2_train_set_final.rda")
load("rda/result_svm_train_set_final.rda")
result_svm2_train_set_final$results$rmse
result_svm_train_set_final$results$rmse
# check whether the RMSE are identical, or almost
identical(result_svm2_train_set_final$results$rmse,
          result_svm_train_set_final$results$rmse)
all.equal(result_svm2_train_set_final$results$rmse,
          result_svm_train_set_final$results$rmse)
# check whether the y_hat are identical, or almost
identical(result_svm2_train_set_final$y_hat,
          result_svm_train_set_final$y_hat)
all.equal(result_svm2_train_set_final$y_hat,
          result_svm_train_set_final$y_hat)

#tidy 
save(result_svm2_train_set_final, file="rda/result_svm2_train_set_final.rda")
save(rmse_results, file="rda/rmse_results.rda")
rm(result_svm2_train_smaller, result_svm2_train_small)
rm(result_svm2_train_smaller_cat, result_svm2_train_set_final)
rm(result_svm_train_set_final)


##### Support Vector Machines with Linear Kernel - svmRadial #####
# uses package kernlab
if (!require('kernlab')) install.packages('kernlab'); library('kernlab')

# with the smaller training set
result_svmradial_train_smaller <- 
  results_train_method(train_smaller, test_set_final, "svmRadial")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results,
                          tail(result_svmradial_train_smaller$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance, not here
importance <- varImp(result_svmradial_train_smaller$train, scale=FALSE)

# with the small training set
result_svmradial_train_small <- 
  results_train_method(train_small, test_set_final, "svmRadial")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results,
                          tail(result_svmradial_train_small$results, 1))
rmse_results %>% knitr::kable()

#test the categorical
result_svmradial_train_smaller_cat <- 
  results_train_method(train_smaller_cat, test_set_final, "svmRadial")
# it works

# with the full train set
result_svmradial_train_set_final <- 
  results_train_method(train_set_final, test_set_final, "svmRadial")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results,
                          tail(result_svmradial_train_set_final$results, 1))
rmse_results %>% knitr::kable()

#checking the tuning
plot(result_svmradial_train_set_final$train)
result_svmradial_train_set_final$train

#tidy 
save(result_svmradial_train_set_final, file="rda/result_svmradial_train_set_final.rda")
save(rmse_results, file="rda/rmse_results.rda")
rm(result_svmradial_train_smaller, result_svmradial_train_small)
rm(result_svmradial_train_smaller_cat)
rm(result_svmradial_train_set_final)


##### ensemble #####
load("rda/rmse_results.rda")
# Identify the leading models, the top 5
rmse_results %>%
  arrange(rmse) %>% 
  filter(str_detect(method, "train_set_final") ) %>%
  # head(10) %>% 
  knitr::kable()
# create an ensemble y hats
load("rda/result_glm_train_set_final.rda")
load("rda/result_svm_train_set_final.rda")
load("rda/result_svmradial_train_set_final.rda")
load("rda/result_gbm_train_set_final.rda")
load("rda/result_ranger_train_set_final.rda")

# create table of y_hat
y_hat_ens_table <- 
  data.frame(result_glm_train_set_final$y_hat) %>%
  cbind(data.frame(result_gbm_train_set_final$y_hat)) %>%
  cbind(data.frame(result_svm_train_set_final$y_hat)) %>%
  cbind(data.frame(result_svmradial_train_set_final$y_hat)) %>%
  cbind(data.frame(result_ranger_train_set_final$y_hat)) %>%
  #calculate average of the y_hats
  mutate(y_hat_ave = rowMeans(.))
#test against the actual values
rmse_ens <- rmse(test_set_final$y, y_hat_ens_table$y_hat_ave)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Ensemble glm, gbm, svm, svmRadial, ranger",  
                                 rmse = rmse_ens))
rmse_results %>% tail(5) %>% knitr::kable()
# rmse_results %>%
#   arrange(rmse) %>% 
#   head(10) %>% knitr::kable()
#tidy up
save(y_hat_ens_table, file="rda/y_hat_ens_table.rda")
save(rmse_results, file="rda/rmse_results.rda")
rm(y_hat_ens_table)
rm(result_gbm_train_set_final, result_ranger_train_set_final)

#### ensemble 2 ####
#a smaller ensemble of the three best models
y_hat_ens_table2 <- 
  data.frame(result_glm_train_set_final$y_hat) %>%
  cbind(data.frame(result_svm_train_set_final$y_hat)) %>%
  cbind(data.frame(result_svmradial_train_set_final$y_hat)) %>%
  #calculate average of the y_hats
  mutate(y_hat_ave = rowMeans(.))
#test against the actual values
rmse_ens <- rmse(test_set_final$y, y_hat_ens_table2$y_hat_ave)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Ensemble2 glm, svm, svmRadial",  
                                 rmse = rmse_ens))
rmse_results %>% tail(5) %>% knitr::kable()

# tidy up
save(y_hat_ens_table2, file="rda/y_hat_ens_table2.rda")
save(rmse_results, file="rda/rmse_results.rda")
rm(y_hat_ens_table2, rmse_ens, y_hat_ens_table)


#### feature prioritisation ####
# other promising models, looks like GAM, GAMloess and KNN could be interesting
rmse_results %>%
  filter(str_detect(method, "smaller") ) %>%
  arrange(rmse) %>% head(10) %>% knitr::kable()

# using GLM, GBM, GLM boost to rank features
load("rda/importance_gbm.rda")
load("rda/importance_glm.rda")
load("rda/importance_glmboost.rda")
#create ranking for gbm algorithm, with normalised score
gbm_rank <- importance_gbm$importance %>%
  rownames_to_column(var = "feature") %>%
  arrange(desc(Overall)) %>% 
  cbind(data.frame(gbm_rank = 1:length(importance_gbm$importance$Overall))) %>%
  mutate("gbm_overall" = Overall/max(Overall)) %>% 
  dplyr::select(-Overall)
head(gbm_rank)
#create ranking for glm algorithm, with normalised score
glm_rank <- importance_glm$importance %>%
  rownames_to_column(var = "feature") %>%
  arrange(desc(Overall)) %>% 
  cbind(data.frame(glm_rank = 1:length(importance_glm$importance$Overall))) %>%
  mutate("glm_overall" = Overall/max(Overall)) %>% 
  dplyr::select(-Overall)
head(glm_rank)
#create ranking for glmboost algorithm, with normalised score
glmboost_rank <- importance_glmboost$importance %>%
  rownames_to_column(var = "feature") %>%
  arrange(desc(Overall)) %>% 
  cbind(data.frame(glmboost_rank = 1:length(importance_glmboost$importance$Overall))) %>%
  mutate("glmboost_overall" = Overall/max(Overall)) %>% 
  dplyr::select(-Overall)
head(glmboost_rank)
# combined ranking table, with mean, and reordered
feature_rank <- glm_rank %>%
  left_join(gbm_rank) %>%
  left_join(glmboost_rank) %>%
  mutate(mean_overall = (glm_overall+gbm_overall+glmboost_overall)/3, .after = feature) %>%
  mutate(mean_rank = (glm_rank+gbm_rank+glmboost_rank)/3, .after = feature) 
# arrange by mean ranking and display top 9
feature_rank %>% dplyr::select(feature, mean_rank, mean_overall) %>%
  arrange(mean_rank) %>% head(9) %>% knitr::kable()
# arrange by mean score and display top 9
feature_rank %>% dplyr::select(feature, mean_rank, mean_overall) %>%
  arrange(desc(mean_overall)) %>% head(9) %>% knitr::kable()

# plotting the top 9
# load("rda/feature_rank.rda")
# load("rda/train_set_final.rda")
feature_top9 <- feature_rank %>% 
  dplyr::select(feature, mean_rank, mean_overall) %>%
  arrange(desc(mean_overall)) %>% head(9) %>%
  pull(feature)
#create the plot
train_set_final %>% 
  #select the top9 features
  dplyr::select(y, all_of(feature_top9)) %>%
  # tidy the names
  rename_at(vars(ends_with("25_64")), ~str_replace_all(., "_25_64", "")) %>%
  #pivot to a long version with a row per feature/value
  pivot_longer(cols = !"y", 
               names_to = "feature", 
               values_to = "value") %>%
  ggplot(aes(y, value, col=feature))  + 
  geom_point(size=0.5) +
  facet_wrap(. ~feature,  scales = "free_y") +
  ggtitle("Outcome y for important features") +
  xlab("y, proportion of managerial & professional") +
  ylab("feature values")

#looking at correlation with y
data.frame(cor(train_set_final)) %>%
  dplyr::select(y) %>%
  rownames_to_column(var="feature") %>%
  filter(feature %in% feature_top9) %>% 
  arrange(desc(abs(y))) %>%
  knitr::kable(caption="Correlation with the outcome y, for MSOA")

# looking at the rmse with numbers of features
#creating a function to choose the top n features, based on overall score
topfeature_overall <- function(topn, traindata){
  feature_n <- feature_rank %>% 
    dplyr::select(feature, mean_rank, mean_overall) %>%
    arrange(desc(mean_overall)) %>% head(topn) %>%
    pull(feature)
  train_topn <- traindata %>%
    dplyr::select(y, all_of(feature_n))
  return(train_topn)
}
#creating a function to choose the top n features, based on rank
topfeature_rank <- function(topn, traindata){
  feature_n <- feature_rank %>% 
    dplyr::select(feature, mean_rank, mean_overall) %>%
    arrange(mean_rank) %>% head(topn) %>%
    pull(feature)
  train_topn <- traindata %>%
    dplyr::select(y, all_of(feature_n))
  return(train_topn)
}

# load("rda/feature_rank.rda")
#create new training sets
train_top10 <- topfeature_overall(10, train_set_final)
# names(train_top10)
train_top10_rank <- topfeature_rank(10, train_set_final)
# names(train_top10_rank)
train_top15 <- topfeature_overall(15, train_set_final)
train_top20 <- topfeature_overall(20, train_set_final)

# tidy up
save(feature_rank, file="rda/feature_rank.rda")
# save(train_top15, file="rda/train_top15.rda")
# save(train_top10, file="rda/train_top10.rda")
rm(gbm_rank, glm_rank, glmboost_rank, glmboost_rank2)
rm(feature_top9)
rm(importance_gbm, importance_glm, importance_glmboost)
rm(topfeature_rank)

# running on the GLM model
# with the top15 training set
result_glm_train_top15 <- 
  results_train_method(train_top15, test_set_final, "glm")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_glm_train_top15$results, 1))
rmse_results %>% knitr::kable()
# with the top20 training set
result_glm_train_top20 <- 
  results_train_method(train_top20, test_set_final, "glm")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_glm_train_top20$results, 1))
rmse_results %>% knitr::kable()

# with the top10 training set
result_glm_train_top10 <- 
  results_train_method(train_top10, test_set_final, "glm")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_glm_train_top10$results, 1))
rmse_results %>% knitr::kable()

# with the top10 training set on rank
result_glm_train_top10_rank <- 
  results_train_method(train_top10_rank, test_set_final, "glm")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_glm_train_top10_rank$results, 1))
rmse_results %>% 
  filter(str_detect(method, '"glm"') ) %>% 
  arrange(rmse) %>%
  knitr::kable()
# tidy
rm(train_top10, train_top10_rank)
rm(train_top15, train_top20)
rm(result_glm_train_top10, result_glm_train_top10_rank)
rm(result_glm_train_top15, result_glm_train_top20)

# running with features from top 10 to top 50 say
#set up the RMSE table
load("rda/feature_rank.rda")
#apply to all of the top 10 to top 50 features
glm_top10_top50_tmp <- sapply(10:50, function(n){
  print(paste("training with", n, "features"))
  train_topn <- topfeature_overall(n, train_set_final)
  print("running results function")
  result_glm_train_topn <- 
    results_train_method(train_topn, test_set_final, "glm")
  result_glm_train_topn$results[,2]
})
#extract the rmse values and put in a table
glm_top10_top50 <- t(data.frame(glm_top10_top50_tmp)) %>%
  cbind(feature_no = 10:50)
colnames(glm_top10_top50) <- c("rmse", "feature_no")
# load the data
# load("rda/glm_top10_top50.rda")
#plot the values
data.frame(glm_top10_top50) %>%
  ggplot(aes(x=feature_no, y=rmse)) +
  geom_line() +
  ggtitle("Improvement in RMSE with number of features for GLM") +
  xlab("number of features, in priority order") +
  ylab("RMSE")

# similar with svm radial
#apply to the top 20 to top 40 features
svmradial_top20_top40_tmp <- sapply(seq(20, 40, 2), function(n){
  print(paste("training with", n, "features"))
  train_topn <- topfeature_overall(n, train_set_final)
  print("running results function")
  result_svmradial_train_topn <- 
    results_train_method(train_topn, test_set_final, "svmRadial")
  result_svmradial_train_topn$results[,2]
})
#extract the rmse values and put in a table
svmradial_top20_top40_tmp
svmradial_top20_top40 <- t(data.frame(svmradial_top20_top40_tmp)) %>%
  cbind(feature_no = seq(20, 40, 2))
colnames(svmradial_top20_top40) <- c("rmse", "feature_no")
svmradial_top20_top40
#load the calculation from file
# load("rda/svmradial_top20_top40.rda")
#plot the values
data.frame(svmradial_top20_top40) %>%
  ggplot(aes(x=feature_no, y=rmse)) +
  geom_line() +
  ggtitle("Improvement in RMSE with number of features for SVM Radial") +
  xlab("number of features, in priority order") +
  ylab("RMSE")

# similar with SVM
#apply to the top 20 to top 40 features
svm_top20_top40_tmp <- sapply(seq(20, 40, 2), function(n){
  print(paste("training with", n, "features"))
  train_topn <- topfeature_overall(n, train_set_final)
  print("running results function")
  result_svm_train_topn <- 
    results_train_method(train_topn, test_set_final, "svmLinear")
  result_svm_train_topn$results[,2]
})
#extract the rmse values and put in a table
svm_top20_top40_tmp
svm_top20_top40 <- t(data.frame(svm_top20_top40_tmp)) %>%
  cbind(feature_no = seq(20, 40, 2))
colnames(svm_top20_top40) <- c("rmse", "feature_no")
svm_top20_top40
#load the calculation from file
# load("rda/svm_top20_top40.rda")
#plot the values
data.frame(svm_top20_top40) %>%
  ggplot(aes(x=feature_no, y=rmse)) +
  geom_line() +
  ggtitle("Improvement in RMSE with number of features for SVM") +
  xlab("number of features, in priority order") +
  ylab("RMSE")

# tidy up
save(glm_top10_top50, file="rda/glm_top10_top50.rda")
save(svmradial_top20_top40, file="rda/svmradial_top20_top40.rda")
save(svm_top20_top40, file="rda/svm_top20_top40.rda")
rm(svm_top20_top40, svm_top20_top40_tmp)
rm(svmradial_top20_top40, svmradial_top20_top40_tmp)
rm(glm_top10_top50, glm_top10_top50_tmp)


#### gam and gam loess again ####
# running on GAM with 25 features
train_top25 <- topfeature_overall(25, train_set_final)
# with the train_top25 training set
result_gam_train_top25 <- 
  results_train_method(train_top25, test_set_final, "gam")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, tail(result_gam_train_top25$results, 1))
rmse_results %>% knitr::kable()
# checking the variable importance
importance <- varImp(result_gam_train_top25$train, scale=FALSE)
plot(importance, 20)
# tidy
save(result_gam_train_top25, file="rda/result_gam_train_top25.rda")
rm(result_gam_train_top25)

# running on GAM with 28 features
train_top28 <- topfeature_overall(28, train_set_final)
# with the train_top28 training set
result_gam_train_top28 <- 
  results_train_method(train_top28, test_set_final, "gam")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_gam_train_top28$results, 1))
rmse_results %>% knitr::kable()
rmse_results %>% arrange(rmse) %>% knitr::kable()
# checking the variable importance
importance <- varImp(result_gam_train_top28$train, scale=TRUE)
plot(importance, 20)
# tidy
save(result_gam_train_top28, file="rda/result_gam_train_top28.rda")
# load("rda/result_gam_train_top28.rda")
# rm(result_gam_train_top28)
save(rmse_results, file="rda/rmse_results.rda")

# running on GAMloess with 28 features
# with the train_top28 training set
result_gamloess_train_top28 <- 
  results_train_method(train_top28, test_set_final, "gamLoess")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, tail(result_gamloess_train_top28$results, 1))
rmse_results %>% knitr::kable()
# tidy
save(result_gamloess_train_top28, file="rda/result_gamloess_train_top28.rda")
rm(result_gamloess_train_top28)
save(rmse_results, file="rda/rmse_results.rda")
rm(importance)
rm(train_top25)

#checking the overall performance
rmse_results %>% arrange(rmse) %>% head(10) %>%   knitr::kable() 

# comparing gam and gamloess
load("rda/result_gamloess_train_top28.rda")
load("rda/result_gam_train_top28.rda")
y_hat_gam <- result_gam_train_top28$y_hat
y_hat_gamloess <- result_gamloess_train_top28$y_hat
# graph of y hat against each other
data.frame(y_hat_gam, y_hat_gamloess) %>%
  ggplot(aes(y_hat_gam, y_hat_gamloess)) + 
  geom_point() +
  ggtitle("Comparing GAM vs GAM Loess predictions") +
  xlab("GAM model predictions") +
  ylab("GAM Loess model predictions")
# histogram of deltas
data.frame(y_hat_gam, y_hat_gamloess) %>%
  mutate(delta = (y_hat_gam-y_hat_gamloess)) %>%
  ggplot(aes(delta)) + 
  geom_histogram(bins = 30) +
  ggtitle("Comparing GAM vs GAM Loess predictions") +
  xlab("Delta between GAM and GAM Loess predictions") +
  ylab("number of predictions in each interval")
# tidy
rm(y_hat_gam, y_hat_gamloess)
rm(result_gamloess_train_top28, result_gam_train_top28)

# running on knn with 28 features, less age and area
train_top28_knn <- train_top28 %>%
  dplyr::select(-age_median, -area_code)
# with the train_top28 training set
result_knn_train_top28_knn <- 
  results_train_method(train_top28_knn, test_set_final, "knn")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, tail(result_knn_train_top28_knn$results, 1))
rmse_results %>% knitr::kable()
# tidy
rm(train_top28_knn, result_knn_train_top28_knn, train_top28)


#### ensemble 3 ####
# updating the ensemble with the new models
# Identify the leading models, the new top 5
# load("rda/rmse_results.rda")
rmse_results %>%
  filter(str_detect(method, 'Ensemble', negate=TRUE) ) %>% 
  arrange(rmse) %>% head(15) %>% knitr::kable()

#loading the separate model information
load("rda/result_glm_train_set_final.rda")
load("rda/result_svm_train_set_final.rda")
load("rda/result_svmradial_train_set_final.rda")
load("rda/result_gam_train_top28.rda")
load("rda/result_gamloess_train_top28.rda")

# an ensemble of the best five models
y_hat_ens_table3 <- 
  data.frame(result_glm_train_set_final$y_hat) %>%
  cbind(data.frame(result_svm_train_set_final$y_hat)) %>%
  cbind(data.frame(result_svmradial_train_set_final$y_hat)) %>%
  cbind(data.frame(result_gam_train_top28$y_hat)) %>%
  cbind(data.frame(result_gamloess_train_top28$y_hat)) %>%
  #calculate average of the y_hats
  mutate(y_hat_ave = rowMeans(.))
head(y_hat_ens_table3)
#test against the actual values
rmse_ens <- rmse(test_set_final$y, y_hat_ens_table3$y_hat_ave)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Ensemble3 glm, svm, svmRadial, gam, gamloess", rmse = rmse_ens))
rmse_results %>% 
  arrange(rmse) %>% head(20) %>% 
  knitr::kable(caption="Leading models including the ensembles, with MSOA")

#### ensemble 4 ####
# an ensemble of the best three models
y_hat_ens_table4 <- 
  data.frame(result_glm_train_set_final$y_hat) %>%
  cbind(data.frame(result_svmradial_train_set_final$y_hat)) %>%
  cbind(data.frame(result_gam_train_top28$y_hat)) %>%
  #calculate average of the y_hats
  mutate(y_hat_ave = rowMeans(.))
head(y_hat_ens_table4)
#test against the actual values
rmse_ens <- rmse(test_set_final$y, y_hat_ens_table4$y_hat_ave)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Ensemble4 svmRadial, gam, glm",  
                                 rmse = rmse_ens))
rmse_results %>%
  arrange(rmse) %>% head(10) %>% knitr::kable()

#### ensemble 5 ####
#ensemble of 2
y_hat_ens_table5 <- 
  data.frame(svmradial_y_hat = result_svmradial_train_set_final$y_hat, 
             gam_y_hat = result_gam_train_top28$y_hat) %>%
  #calculate average of the y_hats
  mutate(y_hat_ave = rowMeans(.))
#test against the actual values
rmse_ens <- rmse(test_set_final$y, y_hat_ens_table5$y_hat_ave)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Ensemble5 svmRadial, gam",  
                                 rmse = rmse_ens))
rmse_results %>%
  # arrange(rmse) %>% head(10) %>% knitr::kable()
  arrange(rmse) %>% head(20) %>% knitr::kable()

# tidy up
save(y_hat_ens_table5, file="rda/y_hat_ens_table5.rda")
save(rmse_results, file="rda/rmse_results.rda")
rm(y_hat_ens_table3, rmse_ens, y_hat_ens_table4)
rm(y_hat_ens_table5)
rm(result_gamloess_train_top28, result_svm_train_set_final)
rm(result_gam_train_top28, result_glm_train_set_final)
rm(result_svmradial_train_set_final)
rm(feature_rank)

#### Investigation and visualisation of errors ####
# looking at the best ensemble model, and constituent ones svmRadial and gam
# plot of y against y_hat for all three
load("rda/y_hat_ens_table5.rda")
y_hat_ens_table5 %>%
  cbind(y = test_set_final$y) %>%
  dplyr::rename(ensemble_y_hat=y_hat_ave) %>%
  pivot_longer(cols=contains("y_hat"), names_to="model", values_to="y_hat") %>%
  ggplot(aes(x=y_hat, y=y, col=model)) +
  # geom_point() +  
  geom_point(size=0.7) +
  geom_abline(slope = 1, intercept = 0, col="black")  +
  facet_grid(. ~model) +
  ggtitle("Comparing ensemble5 predictions vs the outcome y") +
  xlab("model predictions") +
  ylab("actual values, y") + 
  theme(legend.position = "bottom")

# histogram of deltas for all three
y_hat_ens_table5 %>%
  cbind(y = test_set_final$y) %>%
  mutate(svmradial_delta = (y-svmradial_y_hat)) %>%
  mutate(gam_delta = (y-gam_y_hat)) %>%
  mutate(ensemble_delta = (y-y_hat_ave)) %>%
  dplyr::select(-svmradial_y_hat, -gam_y_hat, -y_hat_ave) %>%
  pivot_longer(cols=contains("delta"), names_to="model", values_to="delta") %>%
  ggplot(aes(delta, fill=model)) +
  geom_histogram(bins = 30) +
  facet_grid(. ~model) +
  ggtitle("Comparing ensemble5 predictions vs the outcome y") +
  xlab("delta between y and the model predictions") +
  ylab("number of predictions in each interval") +
  theme(legend.position = "bottom")

# comparing the svmradial and gam models
y_hat_ens_table5 %>%
  cbind(y = test_set_final$y) %>%
  dplyr::select(-y_hat_ave) %>%
  pivot_longer(cols=contains("y_hat"), names_to="model", values_to="y_hat") %>%
  ggplot(aes(x=y_hat, y=y, col=model)) +
  geom_point()  +
  geom_abline(slope = 1, intercept = 0, col="black") +
  ggtitle("Comparing gam and svm radial predictions vs the outcome y") +
  xlab("model predictions") +
  ylab("actual values, y") 

# looking at the largest errors
largest_errors <- y_hat_ens_table5 %>%
  cbind(y = test_set_final$y) %>%
  cbind(area_code = test_set_final$area_code) %>%
  mutate(delta = (y-y_hat_ave)) %>%
  arrange(desc(abs(delta))) %>%
  head(20) %>%
  left_join(test_set_final)
names(largest_errors)
head(largest_errors, 20)

# tidy
rm(largest_errors)
rm(y_hat_ens_table5)
rm(train_small, train_small_cat)
rm(train_smaller, train_smaller_cat)
rm(train_final_nocat, train_set_final)
rm(test_set_final)


#### running on the validation / main set ####

# load the files
load("rda/geo_lookup.rda")
load("rda/main.rda")
load("rda/validation.rda")
#cleanse the data, add the geo info
main_clean <- data_cleanse(main, "msoa")
validation_clean <- data_cleanse(validation, "msoa")
#check
names(main_clean)
names(validation_clean)
head(main_clean)
head(validation_clean)
sum(is.na(main_clean))

# tidy up
save(main_clean, file="rda/main_clean.rda")
save(validation_clean, file="rda/validation_clean.rda")
rm(geo_lookup)
rm(main, validation)

# create baseline
mu_hat <- mean(main_clean$y)
rmse_ave <- rmse(validation_clean$y, mu_hat)
rmse_results_validation <- tibble(method = "Mean on main & validation", rmse = rmse_ave)
rmse_results_validation %>% knitr::kable()

#tidy
load("rda/rmse_results.rda")
save(rmse_results_validation, file="rda/rmse_results_validation.rda")

# run on glm radial
result_glm_main <- 
  results_train_method(main_clean, validation_clean, "glm")
# extract the rmse from the results
rmse_results_validation <- bind_rows(rmse_results_validation, 
                                     tail(result_glm_main$results, 1))
rmse_results_validation %>% knitr::kable()
#tidy
save(rmse_results_validation, file="rda/rmse_results_validation.rda")
save(result_glm_main, file="rda/result_glm_main.rda")

# run on svm radial
# uses package kernlab
if (!require('kernlab')) install.packages('kernlab'); library('kernlab')
# with the full train set
result_svmradial_main <- 
  results_train_method(main_clean, validation_clean, "svmRadial")
# extract the rmse from the results
rmse_results_validation <- bind_rows(rmse_results_validation, 
                                     tail(result_svmradial_main$results, 1))
rmse_results_validation %>% knitr::kable()
#tidy
save(rmse_results_validation, file="rda/rmse_results_validation.rda")
save(result_svmradial_main, file="rda/result_svmradial_main.rda")
# load("rda/rmse_results_validation.rda")

# run on gam
# load top 28 features with the GLM, GBM and GLM boost models on the train/test data
load("rda/feature_rank.rda")
# put together the prioritised main data set subset
main_top28 <- topfeature_overall(28, main_clean)
# train the model with the train_top28 training set
result_gam_main_top28 <- 
  results_train_method(main_top28, validation_clean, "gam")
# extract the rmse from the results
rmse_results_validation <- bind_rows(rmse_results_validation,
                                     tail(result_gam_main_top28$results, 1))
rmse_results_validation %>% knitr::kable()
# tidy
save(result_gam_main_top28, file="rda/result_gam_main_top28.rda")
save(rmse_results_validation, file="rda/rmse_results_validation.rda")

# ensemble of 2
load("rda/result_gam_main_top28.rda")
load("rda/result_svmradial_main.rda")
y_hat_ens_main <- 
  data.frame(svmradial_y_hat = result_svmradial_main$y_hat, 
             gam_y_hat = result_gam_main_top28$y_hat) %>%
  #calculate average of the y_hats
  mutate(y_hat_ave = rowMeans(.))
#test against the actual values
rmse_ens <- rmse(validation_clean$y, y_hat_ens_main$y_hat_ave)
rmse_results_validation <- bind_rows(rmse_results_validation,
                          tibble(method="Ensemble svmRadial, gam28",  
                                 rmse = rmse_ens))
rmse_results_validation %>% knitr::kable()

# comparing the svmradial and gam models
y_hat_ens_main %>%
  cbind(y = validation$y) %>%
  dplyr::select(-y_hat_ave) %>%
  pivot_longer(cols=contains("y_hat"), names_to="model", values_to="y_hat") %>%
  ggplot(aes(x=y_hat, y=y, col=model)) +
  geom_point()  +
  geom_abline(slope = 1, intercept = 0, col="black") +
  ggtitle("Comparing gam & svm radial predictions vs the outcome y for main") +
  xlab("model predictions") +
  ylab("actual values, y") 

# tidy up, remove all data sets relating to MSOA
save(rmse_results_validation, file="rda/rmse_results_validation.rda")
save(y_hat_ens_main, file="rda/y_hat_ens_main.rda")
# load("rda/y_hat_ens_main.rda")
rm(rmse_ens, y_hat_ens_main)
rm(mu_hat, rmse_ave)
rm(result_gam_main_top28)
rm(result_svmradial_main)
rm(result_glm_main)
rm(validation_clean, main_clean, main_top28)
rm(rmse_results, rmse_results_validation)
rm(feature_rank)
rm(geographytype)


#### running on the SOA  #####
### downloading the SOA data
# choosing London and Yorkshire
# "2013265927TYPE299">output areas 2011 in London
# "2013265923TYPE299">output areas 2011 in Yorkshire and The Humber
#creating list of census tables to take
censusdata_list <- c("ks608ew", "ks102ew", "lc2101ew", "lc2103ew","lc6110ew", "lc1109ew", "lc2107ew", "lc1101ew", "lc5102ew")

# London
# <option value="2013265927TYPE299">output areas 2011 in London</option>
# apply the list to download all of the data
download_censusdata("2013265927TYPE299", censusdata_list)
#import the data for London
data_set <- import_data("2013265927TYPE299")
load("rda/data_set_2013265927TYPE299.rda")
# tidy up
data_set_london <- data_set
rm(data_set)
# rm(geographytype)

# Yorkshire and The Humber
# <option value="2013265923TYPE299">output areas 2011 in Yorkshire and The Humber
# geographytype <- "2013265923TYPE299" # output areas 2011 in Yorkshire and The Humber
# apply the list to download all of the data
download_censusdata("2013265923TYPE299", censusdata_list)
#import the data for Yorkshire
data_set <- import_data("2013265923TYPE299")
load("rda/data_set_2013265923TYPE299.rda")
# tidy up
data_set_yorkshire <- data_set
rm(marital_predictors, country_predictors, occupation_target)
rm(ethnicity_predictors, household_predictors, religion_predictors)
rm(industry_predictors, sex_predictors)
rm(qualifications_predictors, age_predictors)
rm(import_data)
rm(censusdata_list)


#combine in to one data set
data_set <- data_set_london %>%
  rbind(data_set_yorkshire)
# tidy
save(data_set, file="rda/data_set_lon_york.rda")
rm(data_set_london, data_set_yorkshire)


#### create main and validation set
# Validation set will be 10% of the data
set.seed(2011, sample.kind="Rounding")
test_index <- createDataPartition(y = data_set$y, times = 1, p = 0.1, list = FALSE)
main_soa <- data_set[-test_index,]
validation_soa <- data_set[test_index,]
### tidy, take backup:
save(main_soa, file='rda/main_soa.rda')
save(validation_soa, file='rda/validation_soa.rda')
rm(validation_soa, data_set, test_index)

#### create test and train set
load("rda/main_soa.rda")
# test set will be 10% of the data
set.seed(2001, sample.kind="Rounding")
test_index <- createDataPartition(y = main_soa$y, times = 1, p = 0.1, list = FALSE)
train_soa <- main_soa[-test_index,]
test_soa <- main_soa[test_index,]
#check
names(train_soa)
sum(is.na(train_soa))
### tidy, take backup:
save(train_soa, file='rda/train_soa.rda')
save(test_soa, file='rda/test_soa.rda')
rm(main_soa, test_index)
# load("rda/test_soa.rda")
# load("rda/train_soa.rda")

#data cleanse on the test set and train set
load("rda/geo_lookup.rda")
test_soa_final <- data_cleanse(test_soa, "soa")
train_soa_final <- data_cleanse(train_soa, "soa")

#check
names(test_soa_final)
names(test_soa)
head(test_soa_final$area_code)
head(test_soa$geo_code)
#check the number of local areas is correct
# means 54 sub regions in London and Yorkshire
length(levels(as.factor(test_soa_final$area_code)))
length(levels(as.factor(train_soa_final$area_code)))
names(train_soa_final)
# check, the sum of London and Yorkshire is all of the locations, 3811
sum(test_soa_final$london) + sum(test_soa_final$yorkshire_humber)
# no NA
sum(is.na(train_soa_final))
sum(is.na(test_soa_final))
# tidy up
save(test_soa_final, file="rda/test_soa_final.rda")
save(train_soa_final, file="rda/train_soa_final.rda")
rm(test_soa, train_soa, geo_lookup)

#### baseline rmse and rmse function
# load the test and train data
load("rda/test_soa_final.rda")
load("rda/train_soa_final.rda")
options(digits = 6)
# baseline RMSE with an average of all ratings
mu_hat <- mean(train_soa_final$y)
rmse_ave <- rmse(test_soa_final$y, mu_hat)
rmse_results_soa <- tibble(method = "Mean of all locations", rmse = rmse_ave)
rmse_results_soa %>% knitr::kable()
# tidy
save(rmse_results_soa, file="rda/rmse_results_soa.rda")
rm(mu_hat, rmse_ave)
# load("rda/rmse_results_soa.rda")
load("rda/rmse_results.rda")
rmse_results %>% knitr::kable()

#### visualisation
#graph the geographic areas
load("rda/main_soa.rda")
load("rda/main.rda")

main_soa %>%  ggplot(aes(all_residents))  + 
  geom_histogram(bins = 50, color = "black") +
  ggtitle("Size of the geographic areas") +
  xlab("number of residents in the SOA area") +
  ylab("count of numbers of SOA areas")
#table of key data points of the sizes of the SOA areas
main_soa_sizes <- c(mean = mean(main_soa$all_residents),
                      sd=sd(main_soa$all_residents),
                      max = max(main_soa$all_residents), 
                      min = min(main_soa$all_residents))
#table of key data points of the sizes of the MSOA areas
main_sizes <- c(mean = mean(main$all_residents),
                     sd=sd(main$all_residents),
                     max = max(main$all_residents), 
                     min = min(main$all_residents))
# displaying in a table for comparison
data.frame("Residents_in_SOA" = main_soa_sizes) %>% 
  cbind(data.frame("Residents_in_MSOA" = main_sizes)) %>%
  knitr::kable(caption="Comparison of number of residents in SOA and MSOA")
# tidy
rm(main, main_soa, main_soa_sizes, main_sizes)

#graph the outcome y
load("rda/train_set_final.rda")
tmp <- data.frame(SOA=train_soa_final$y) %>%
  pivot_longer(cols = SOA,
               names_to = "area_type", 
               values_to = "y")
data.frame(MSOA=train_set_final$y) %>%
  pivot_longer(cols = MSOA,
               names_to = "area_type", 
               values_to = "y") %>%
  rbind(tmp) %>%
  ggplot(aes(y))  + 
  geom_histogram(bins = 30, color = "black") + 
  facet_grid(area_type~., scales="free_y") +
  ggtitle("Distribution of the outcome, y for SOA and MSOA areas") +
  xlab("y, proportions of people in managerial or professional occupations") +
  ylab("count of numbers of areas")

#table of key data points of the outcome y
# summary info for the SOA data
train_soa_y <- c(mean = mean(train_soa_final$y),
                             sd=sd(train_soa_final$y),
                             max = max(train_soa_final$y), 
                             min = min(train_soa_final$y))
# summary info for the MSOA data
train_msoa_y <- c(mean = mean(train_set_final$y),
                             sd=sd(train_set_final$y),
                             max = max(train_set_final$y), 
                             min = min(train_set_final$y))
# displaying in a table for comparison
data.frame("y_SOA" = train_soa_y) %>% 
  cbind(data.frame("y_MSOA" = train_msoa_y)) %>%
  knitr::kable(caption="Comparison of y, proportion of senior managers and professional occupations in SOA and MSOA")
# tidy
rm(tmp, train_soa_y, train_soa_y, train_msoa_y)

# graph illustrating the impact of London or Yorkshire
test_soa_final %>%
  dplyr::select(y, london, yorkshire_humber) %>%
  pivot_longer(cols=c("london", "yorkshire_humber"), 
               names_to="region", values_to="true") %>%
  filter(true=="1") %>%
  dplyr::select(-true) %>%  
  ggplot(aes(y, fill=region))  + 
  geom_histogram(bins = 30) +
  ggtitle("Distribution of the outcome, y for regions") +
  xlab("y, proportions of people in managerial or professional occupations") +
  ylab("count of numbers of SOA areas")

#looking at the features
# box plot of features expressed as a ratio
# identify the geography features not necessary for this graph
load("rda/geo_lookup.rda")
# pull out only the places, not the geo and area columns
geography_features <-  intersect(
  str_subset(names(geo_lookup),  "area", negate = TRUE),
  str_subset(names(geo_lookup),  "geo", negate = TRUE))
# plot the graph
train_soa_final %>%  
  # remove columns not needed, features that are not ratios
  dplyr::select(-age_median, -area_code, -all_of(geography_features)) %>%
  #tidy the names, removing the _25_64
  rename_at(vars(contains("_25_64")), ~str_replace_all(., "_25_64", "")) %>%
  #pivot to a long version with a row per feature/value
  pivot_longer(cols = c(-y),
               names_to = "feature", 
               values_to = "proportion") %>%
  ggplot(aes(x=reorder(feature, proportion, FUN=mean), y=proportion))  + 
  geom_boxplot() +
  ggtitle("Boxplot of features for the SOA") +
  ylab("proportion") + 
  xlab("feature name, reordered by mean") + 
  theme(axis.text.x=element_text(angle = 90, hjust=1, vjust=0.5))
# tidy
rm(geo_lookup)
rm(geography_features)


#### modelling on the train/test data
# GLM
# with the full train set
result_glm_train_soa <- 
  results_train_method(train_soa_final, test_soa_final, "glm")
# extract the rmse from the results
rmse_results_soa <- bind_rows(rmse_results_soa, 
                          tail(result_glm_train_soa$results, 1))
rmse_results_soa %>% knitr::kable()
#checking the variable importance
importance <- varImp(result_glm_train_soa$train, scale=FALSE)
plot(importance, 20)
importance$importance
# tidy up
importance_soa_glm <- importance 
save(result_glm_train_soa, file="rda/result_glm_train_soa.rda")
save(importance_soa_glm, file="rda/importance_soa_glm.rda")
save(rmse_results_soa, file="rda/rmse_results_soa.rda")
rm(importance)
rm(result_glm_train_soa)

##### GBM
# using the gbmm plyr, package
if (!require('gbm')) install.packages('gbm'); library('gbm')
if (!require('plyr')) install.packages('plyr'); library('plyr')
# with the full train set
result_gbm_train_soa <- 
  results_train_method(train_soa_final, test_soa_final, "gbm")
# extract the rmse from the results
rmse_results_soa <- bind_rows(rmse_results_soa, 
                              tail(result_gbm_train_soa$results, 1))
rmse_results_soa %>% knitr::kable()
#checking the variable importance
importance <- varImp(result_gbm_train_soa$train, scale=FALSE)
plot(importance, 20)
#tidy
importance_soa_gbm <- importance 
save(importance_soa_gbm, file="rda/importance_soa_gbm.rda")
save(result_gbm_train_soa, file="rda/result_gbm_train_soa.rda")
save(rmse_results_soa, file="rda/rmse_results_soa.rda")
rm(importance)
rm(result_gbm_train_soa)

#### Boosted Generalized Linear Model - glmboost
# uses the plyr, mboost packages
if (!require('mboost')) install.packages('mboost'); library('mboost')
if (!require('plyr')) install.packages('plyr'); library('plyr')
# with the full training set
result_glmboost_train_soa <- 
  results_train_method(train_soa_final, test_soa_final, "glmboost")
# extract the rmse from the results
rmse_results_soa <- bind_rows(rmse_results_soa,
                          tail(result_glmboost_train_soa$results, 1))
rmse_results_soa %>% knitr::kable()
#checking the variable importance
importance <- varImp(result_glmboost_train_soa$train, scale=FALSE)
plot(importance, 20)

#tidy
importance_soa_glmboost <- importance 
save(result_glmboost_train_soa, file="rda/result_glmboost_train_soa.rda")
save(importance_soa_glmboost, file="rda/importance_soa_glmboost.rda")
save(rmse_results_soa, file="rda/rmse_results_soa.rda")
rm(importance)
rm(result_glmboost_train_soa)


# top 28 features
# using GLM, GBM, GLM boost to rank features
# load("rda/importance_soa_gbm.rda")
# load("rda/importance_soa_glm.rda")
# load("rda/importance_soa_glmboost.rda")
#create ranking for gbm algorithm, with normalised score
gbm_soa_rank <- importance_soa_gbm$importance %>%
  rownames_to_column(var = "feature") %>%
  arrange(desc(Overall)) %>% 
  cbind(data.frame(gbm_rank = 1:length(importance_soa_gbm$importance$Overall))) %>%
  mutate("gbm_overall" = Overall/max(Overall)) %>% 
  dplyr::select(-Overall)
head(gbm_soa_rank)
#create ranking for glm algorithm, with normalised score
glm_soa_rank <- importance_soa_glm$importance %>%
  rownames_to_column(var = "feature") %>%
  arrange(desc(Overall)) %>% 
  cbind(data.frame(glm_rank = 1:length(importance_soa_glm$importance$Overall))) %>%
  mutate("glm_overall" = Overall/max(Overall)) %>% 
  dplyr::select(-Overall)
head(glm_soa_rank)
#create ranking for glmboost algorithm, with normalised score
glmboost_soa_rank <- importance_soa_glmboost$importance %>%
  rownames_to_column(var = "feature") %>%
  arrange(desc(Overall)) %>% 
  cbind(data.frame(glmboost_rank = 1:length(importance_soa_glmboost$importance$Overall))) %>%
  mutate("glmboost_overall" = Overall/max(Overall)) %>% 
  dplyr::select(-Overall)
head(glmboost_soa_rank)
# combined ranking table, with mean, and reordered
feature_rank <- glm_soa_rank %>%
  left_join(gbm_soa_rank) %>%
  left_join(glmboost_soa_rank) %>%
  dplyr::mutate(mean_rank = (glm_rank+gbm_rank+glmboost_rank)/3, .after = "feature") %>%
  dplyr::mutate(mean_overall = (glm_overall+gbm_overall+glmboost_overall)/3, .after = "feature") %>%
  arrange(desc(mean_overall)) 
head(feature_rank)

# load("rda/feature_rank_soa.rda")
#create new training set, using the function from earlier
train_soa_top28 <- topfeature_overall(28, train_soa_final)

# tidy up
save(feature_rank, file="rda/feature_rank_soa.rda")
save(train_soa_top28, file="rda/train_soa_top28.rda")
# load("rda/train_soa_top28")
rm(importance_soa_gbm, importance_soa_glm, importance_soa_glmboost)
rm(glm_soa_rank, gbm_soa_rank, glmboost_soa_rank)


#### SVM radial
# uses package kernlab
if (!require('kernlab')) install.packages('kernlab'); library('kernlab')
# with the full train set
result_svmradial_train_soa <- 
  results_train_method(train_soa_final, test_soa_final, "svmRadial")
# extract the rmse from the results
rmse_results_soa <- bind_rows(rmse_results_soa,
                          tail(result_svmradial_train_soa$results, 1))
rmse_results_soa %>% knitr::kable()
# poor performance, lots of errors
# with the top28 train set
# result_svmradial_train_soa_top28 <- 
#   results_train_method(train_soa_top28, test_soa_final, "svmRadial")
# failed
# training with 15 features
# load("rda/feature_rank_soa.rda")

#create new small and smaller training sets

#sampling the 5000 observations
set.seed(2001, sample.kind="Rounding")
index <- sample(1:nrow(train_soa_final), 5000, replace = FALSE)
train_soa_5k <- train_soa_final[index,]
#create new training set, using the function from earlier
train_soa_5k_top5 <- topfeature_overall(5, train_soa_5k)
train_soa_5k_top10 <- topfeature_overall(10, train_soa_5k)
train_soa_5k_top15 <- topfeature_overall(15, train_soa_5k)
train_soa_5k_top28 <- topfeature_overall(28, train_soa_5k)

# with the 5k top5 train set
result_svmradial_train_soa_5k_top5 <- 
  results_train_method(train_soa_5k_top5, test_soa_final, "svmRadial")
# extract the rmse from the results
rmse_results_soa <- bind_rows(rmse_results_soa,
                              tail(result_svmradial_train_soa_5k_top5$results, 1))
rmse_results_soa %>% knitr::kable()
# with the 5k top10 train set
result_svmradial_train_soa_5k_top10 <- 
  results_train_method(train_soa_5k_top10, test_soa_final, "svmRadial")
# extract the rmse from the results
rmse_results_soa <- bind_rows(rmse_results_soa,
                              tail(result_svmradial_train_soa_5k_top10$results, 1))
rmse_results_soa %>% knitr::kable()
# with the 5k top15 train set
result_svmradial_train_soa_5k_top15 <- 
  results_train_method(train_soa_5k_top15, test_soa_final, "svmRadial")
# extract the rmse from the results
rmse_results_soa <- bind_rows(rmse_results_soa,
                              tail(result_svmradial_train_soa_5k_top15$results, 1))
rmse_results_soa %>% knitr::kable()
# with the 5k top28 train set
result_svmradial_train_soa_5k_top28 <- 
  results_train_method(train_soa_5k_top28, test_soa_final, "svmRadial")
# extract the rmse from the results
rmse_results_soa <- bind_rows(rmse_results_soa,
                              tail(result_svmradial_train_soa_5k_top28$results, 1))
rmse_results_soa %>% knitr::kable()

#tidy 
save(result_svmradial_train_soa, file="rda/result_svmradial_train_soa.rda")
save(result_svmradial_train_soa_5k_top28,
     file="rda/result_svmradial_train_soa_5k_top28.rda")
save(rmse_results_soa, file="rda/rmse_results_soa.rda")
rm(result_svmradial_train_soa_5k_top28, result_svmradial_train_soa_5k_top15)
rm(result_svmradial_train_soa_5k_top10, result_svmradial_train_soa_5k_top5)
rm(result_svmradial_train_soa)
rm(index)
rm(train_soa_5k)

#sampling the 10000 observations
set.seed(2001, sample.kind="Rounding")
index <- sample(1:nrow(train_soa_final), 10000, replace = FALSE)
train_soa_10k <- train_soa_final[index,]
# with the 10k top28 train set
train_soa_10k_top28 <- topfeature_overall(28, train_soa_10k)
# training the model
result_svmradial_train_soa_10k_top28 <- 
  results_train_method(train_soa_10k_top28, test_soa_final, "svmRadial")
# extract the rmse from the results
rmse_results_soa <- 
  bind_rows(rmse_results_soa,
            tail(result_svmradial_train_soa_10k_top28$results, 1))
rmse_results_soa %>% knitr::kable()

# tidy
save(rmse_results_soa, file="rda/rmse_results_soa.rda")
save(result_svmradial_train_soa_10k_top28, 
     file="rda/result_svmradial_train_soa_10k_top28.rda")
rm(result_svmradial_train_soa_10k_top28, index)
rm(train_soa_10k)

#sampling the 15000 observations
set.seed(2001, sample.kind="Rounding")
index <- sample(1:nrow(train_soa_final), 15000, replace = FALSE)
train_soa_15k <- train_soa_final[index,]
# with the 15k top28 train set
train_soa_15k_top28 <- topfeature_overall(28, train_soa_15k)
# training the model
result_svmradial_train_soa_15k_top28 <- 
  results_train_method(train_soa_15k_top28, test_soa_final, "svmRadial")
# extract the rmse from the results
rmse_results_soa <- 
  bind_rows(rmse_results_soa,
            tail(result_svmradial_train_soa_15k_top28$results, 1))
rmse_results_soa %>% knitr::kable()
# tidy
save(rmse_results_soa, file="rda/rmse_results_soa.rda")
save(result_svmradial_train_soa_15k_top28, 
     file="rda/result_svmradial_train_soa_15k_top28.rda")
rm(result_svmradial_train_soa_15k_top28)
rm(index)
rm(train_soa_15k)


#### GAM 28
# this uses the mgcv and nlme packages
if (!require('nlme')) install.packages('nlme'); library('nlme')
if (!require('mgcv')) install.packages('mgcv'); library('mgcv')
# with the soa top 28 training set
result_gam_train_soa_top28 <- 
  results_train_method(train_soa_top28, test_soa_final, "gam")
# extract the rmse from the results
rmse_results_soa <- bind_rows(rmse_results_soa, 
                              tail(result_gam_train_soa_top28$results, 1))
rmse_results_soa %>% knitr::kable()
# with the 5k top28 SOA training set
result_gam_train_soa_5k_top28 <- 
  results_train_method(train_soa_5k_top28, test_soa_final, "gam")
# extract the rmse from the results
rmse_results_soa <- bind_rows(rmse_results_soa, 
                              tail(result_gam_train_soa_5k_top28$results, 1))
rmse_results_soa %>% knitr::kable()
#checking the variable importance
importance <- varImp(result_gam_train_soa_5k_top28$train, scale=FALSE)
plot(importance, 20)
# tidy
save(rmse_results_soa, file="rda/rmse_results_soa.rda")
save(result_gam_train_soa_top28, file="rda/result_gam_train_soa_top28.rda")
rm(result_gam_train_soa_top28)
rm(result_gam_train_soa_5k_top28)
rm(importance)

# with the 10k top28 SOA training set
result_gam_train_soa_10k_top28 <- 
  results_train_method(train_soa_10k_top28, test_soa_final, "gam")
# extract the rmse from the results
rmse_results_soa <- bind_rows(rmse_results_soa, 
                              tail(result_gam_train_soa_10k_top28$results, 1))
rmse_results_soa %>% knitr::kable()
# tidy
save(rmse_results_soa, file="rda/rmse_results_soa.rda")
save(result_gam_train_soa_10k_top28, 
     file="rda/result_gam_train_soa_10k_top28.rda")
rm(result_gam_train_soa_10k_top28)

# with the 15k top28 SOA training set
result_gam_train_soa_15k_top28 <- 
  results_train_method(train_soa_15k_top28, test_soa_final, "gam")
# extract the rmse from the results
rmse_results_soa <- 
  bind_rows(rmse_results_soa,
            tail(result_gam_train_soa_15k_top28$results, 1))
rmse_results_soa %>% knitr::kable()
# tidy
save(rmse_results_soa, file="rda/rmse_results_soa.rda")
save(result_gam_train_soa_15k_top28, 
     file="rda/result_gam_train_soa_15k_top28.rda")
rm(result_gam_train_soa_15k_top28)
rmse_results_soa %>% 
  arrange(rmse) %>% knitr::kable()
rm(train_soa_top28)
rm(train_soa_10k_top28, train_soa_15k_top28)
rm(train_soa_5k_top10, train_soa_5k_top15)
rm(train_soa_5k_top28, train_soa_5k_top5)
rm(feature_rank)

### ensemble with SOA data
#ensemble of SVM Radial and GBM
load("rda/result_svmradial_train_soa_15k_top28.rda")
load("rda/result_gbm_train_soa.rda")
y_hat_ens_table6 <- 
  data.frame(svmradial_y_hat = result_svmradial_train_soa_15k_top28$y_hat, 
             gbm_y_hat = result_gbm_train_soa$y_hat) %>%
  #calculate average of the y_hats
  mutate(y_hat_ave = rowMeans(.))
#test against the actual values
rmse_ens <- rmse(test_soa_final$y, y_hat_ens_table6$y_hat_ave)
rmse_results_soa <- bind_rows(rmse_results_soa,
                          tibble(method="Ensemble svmRadial, gbm",  
                                 rmse = rmse_ens))
rmse_results_soa %>%
  # arrange(rmse) %>% head(10) %>% knitr::kable()
  arrange(rmse) %>% knitr::kable()
# tidy up
save(y_hat_ens_table6, file="rda/y_hat_ens_table6.rda")
save(rmse_results_soa, file="rda/rmse_results_soa.rda")
rm(rmse_ens)
rm(result_svmradial_train_soa_15k_top28, result_gbm_train_soa)

## visualisation of errors
# looking at the best ensemble model, and constituent ones svmRadial and gbm
load("rda/y_hat_ens_table6.rda")
# plot of y against y_hat for all three
y_hat_ens_table6 %>%
  cbind(y = test_soa_final$y) %>%
  dplyr::rename(ensemble_y_hat=y_hat_ave) %>%
  pivot_longer(cols=contains("y_hat"), names_to="model", values_to="y_hat") %>%
  ggplot(aes(x=y_hat, y=y, col=model)) +
  geom_point() +  
  geom_abline(slope = 1, intercept = 0, col="black")  +
  facet_grid(. ~model) +
  ggtitle("Comparing ensemble SOA predictions vs the outcome y") +
  xlab("model predictions") +
  ylab("actual values, y") + 
  theme(legend.position = "bottom")

# histogram of deltas for all three
y_hat_ens_table6 %>%
  cbind(y = test_soa_final$y) %>%
  mutate(svmradial_delta = (y-svmradial_y_hat)) %>%
  mutate(gbm_delta = (y-gbm_y_hat)) %>%
  mutate(ensemble_delta = (y-y_hat_ave)) %>%
  dplyr::select(-svmradial_y_hat, -gbm_y_hat, -y_hat_ave) %>%
  pivot_longer(cols=contains("delta"), names_to="model", values_to="delta") %>%
  ggplot(aes(delta, fill=model)) +
  geom_histogram(bins = 30) +
  facet_grid(. ~model) +
  ggtitle("Comparing ensemble SOA predictions vs the outcome y") +
  xlab("delta between y and the model predictions") +
  ylab("number of predictions in each interval") +
  theme(legend.position = "bottom")

# comparing the svmradial and gam models
y_hat_ens_table6 %>%
  cbind(y = test_soa_final$y) %>%
  dplyr::select(-y_hat_ave) %>%
  pivot_longer(cols=contains("y_hat"), names_to="model", values_to="y_hat") %>%
  ggplot(aes(x=y_hat, y=y, col=model)) +
  geom_point()  +
  geom_abline(slope = 1, intercept = 0, col="black") +
  ggtitle("Comparing gbm and svm radial predictions vs the outcome y") +
  xlab("model predictions") +
  ylab("actual values, y") 
# tidy
rm(y_hat_ens_table6)


#### feature importance ####
# plotting the top 9 for SOA (9 as it fits 3 by 3 in the plot)
load("rda/feature_rank_soa.rda")
feature_rank_soa <- feature_rank
rm(feature_rank)
feature_top9_soa <- feature_rank_soa %>% 
  dplyr::select(feature, mean_rank, mean_overall) %>%
  arrange(desc(mean_overall)) %>% head(9) %>%
  pull(feature)
# feature_top9_soa
# feature_rank_soa
#create the plot
train_soa_final %>%  
  # select only the top 9 features
  dplyr::select(y, all_of(feature_top9_soa)) %>%
  #tidy the names, removing the _25_64
  rename_at(vars(contains("_25_64")), ~str_replace_all(., "_25_64", "")) %>%
  #pivot to a long version with a row per feature/value
  pivot_longer(cols = !"y", 
               names_to = "feature", 
               values_to = "value") %>%
  ggplot(aes(y, value, col=feature))  + 
  geom_point(size=0.5) +
  # put in to separate graphs
  facet_wrap(. ~feature,  scales = "free_y") +
  ggtitle("Outcome y for important features for SOA areas") +
  xlab("y, proportion of managerial & professional") +
  ylab("feature values")
#looking at correlation with y
data.frame(cor(train_soa_final)) %>%
  dplyr::select(y) %>%
  rownames_to_column(var="feature") %>%
  filter(feature %in% feature_top9_soa) %>% 
  arrange(desc(abs(y))) %>%
  knitr::kable()
# tidy
rm(feature_rank_soa, feature_top9_soa)

## qualification and level 4 deep dive
# qualification features SOA
load('rda/qualifications.rda')
qualifications_features <- names(qualifications) %>% str_subset("25_64")
train_soa_final %>%  
  dplyr::select(y, all_of(qualifications_features)) %>%
  #tidy the names, removing the _25_64
  rename_at(vars(contains("_25_64")), ~str_replace_all(., "_25_64", "")) %>%
  # pivot longer with the features
  pivot_longer(cols = -y,
               names_to = "feature", 
               values_to = "proportion") %>%
  ggplot(aes(y, proportion))  + 
  facet_wrap(.~feature, scales = "free_y") +
  geom_point(size=0.5) +
  geom_smooth(method = "lm") +
  ggtitle("Outcome y for qualification features in SOA") +
  xlab("y, proportion of managerial & professional") +
  ylab("proportion of qualification type")
# qualification features MSOA
train_set_final %>%  
  dplyr::select(y, all_of(qualifications_features)) %>%
  #tidy the names, removing the _25_64
  rename_at(vars(contains("_25_64")), ~str_replace_all(., "_25_64", "")) %>%
  # pivot longer with the features
  pivot_longer(cols = -y,
               names_to = "feature", 
               values_to = "proportion") %>%
  ggplot(aes(y, proportion))  + 
  facet_wrap(.~feature, scales = "free_y") +
  geom_point(size=0.5) +
  geom_smooth(method = "lm") +
  ggtitle("Outcome y for qualification features in MSOA") +
  xlab("y, proportion of managerial & professional") +
  ylab("proportion of qualification type")

# Plot of y vs level 4 with colour as proportion of white uk
# initial prep of the SOA data, retaining only the relevant columns
train_soa_final_excerpt <- train_soa_final %>%  
  dplyr::select(y, level4_25_64, white_uk_25_64) %>%
  dplyr::rename(SOA = y) %>%
  # pivot longer with the lavel soa
  pivot_longer(cols = SOA,
               names_to = "area_type", 
               values_to = "y")
train_set_final %>%
  dplyr::select(y, level4_25_64, white_uk_25_64) %>%
  dplyr::rename(MSOA = y) %>%
  # pivot longer with the label MSOA
  pivot_longer(cols = MSOA,
               names_to = "area_type", 
               values_to = "y") %>%
  # adding the SOA data
  rbind(train_soa_final_excerpt) %>%
  ggplot(aes(y, level4_25_64, col=white_uk_25_64))  + 
  facet_grid(.~area_type) +
  geom_point(size=0.7) +
  geom_smooth(method = "lm") +
  ggtitle("Outcome y for level4 features") +
  xlab("y, proportion of managerial & professional") +
  ylab("proportion of level4 qualifications")
# looking at stratification for the white uk, soa
train_soa_final %>%  
  dplyr::select(y, level4_25_64, white_uk_25_64) %>%
  # round to nearest 0.1
  mutate(white_uk_strata = round(white_uk_25_64, 1)) %>%
  ggplot(aes(y, level4_25_64, col=white_uk_25_64))  + 
  facet_wrap(.~white_uk_strata) +
  geom_point(size=0.5) +
  geom_smooth(method = "lm") +
  ggtitle("Outcome y for level4 features, stratified by 'white UK' in SOA") +
  xlab("y, proportion of managerial & professional") +
  ylab("proportion of level4 qualifications")
# stratification and correlation
train_soa_final %>%  
  dplyr::select(y, level4_25_64, white_uk_25_64) %>%
  # round to nearest 0.1
  mutate(white_uk_strata = round(white_uk_25_64, 1)) %>%
  group_by(white_uk_strata) %>%
  dplyr::summarize(correlation = cor(y, level4_25_64)) %>%
  knitr::kable()
# tidy
rm(qualifications)
rm(train_soa_final_excerpt)

#industry features SOA
load('rda/industry.rda')
industry_features <- names(industry) %>% str_subset("25_64")
train_soa_final %>% dplyr::select(y, all_of(industry_features)) %>%
  #tidy the names, removing the _25_64
  rename_at(vars(contains("_25_64")), ~str_replace_all(., "_25_64", "")) %>%
  # pivot longer with the features
  pivot_longer(cols = -y,
               names_to = "feature", 
               values_to = "proportion") %>%
  ggplot(aes(y, proportion))  + 
  facet_wrap(.~feature, scales = "free_y") +
  geom_point(size=0.5) +
  geom_smooth(method = "lm") +
  ggtitle("Outcome y for industry features in SOA") +
  xlab("y, proportion of managerial & professional") +
  ylab("proportion of industry type")
#industry features MSOA
train_set_final %>%  dplyr::select(y, all_of(industry_features)) %>%
  #tidy the names, removing the _25_64
  rename_at(vars(contains("_25_64")), ~str_replace_all(., "_25_64", "")) %>%
  # pivot longer with the features
  pivot_longer(cols = -y,
               names_to = "feature", 
               values_to = "proportion") %>%
  ggplot(aes(y, proportion))  + 
  facet_wrap(.~feature, scales = "free_y") +
  geom_point(size=0.5) +
  geom_smooth(method = "lm") +
  ggtitle("Outcome y for industry features in MSOA") +
  xlab("y, proportion of managerial & professional") +
  ylab("proportion of industry type")
# tidy
rm(industry, industry_features)

# household features soa
load('rda/household.rda')
household_features <- names(household) %>% str_subset("25_64")
train_soa_final %>%  dplyr::select(y, all_of(household_features)) %>%
  #tidy the names, removing the _25_64
  rename_at(vars(contains("_25_64")), ~str_replace_all(., "_25_64", "")) %>%
  # pivot longer with the features
  pivot_longer(cols = -y,
               names_to = "feature", 
               values_to = "proportion") %>%
  ggplot(aes(y, proportion))  + 
  facet_wrap(.~feature, scales = "free_y") +
  geom_point(size=0.5) +
  geom_smooth(method = "lm") +
  ggtitle("Outcome y for household features in SOA") +
  xlab("y, proportion of managerial & professional") +
  ylab("proportion of household type")
# household features msoa
# load("rda/train_set_final.rda")
train_set_final %>%  dplyr::select(y, all_of(household_features)) %>%
  #tidy the names, removing the _25_64
  rename_at(vars(contains("_25_64")), ~str_replace_all(., "_25_64", "")) %>%
  # pivot longer with the features
  pivot_longer(cols = -y,
               names_to = "feature", 
               values_to = "proportion") %>%
  ggplot(aes(y, proportion))  + 
  facet_wrap(.~feature, scales = "free_y") +
  geom_point(size=0.5) +
  geom_smooth(method = "lm") +
  ggtitle("Outcome y for household features in MSOA") +
  xlab("y, proportion of managerial & professional") +
  ylab("proportion of household type")
# tidy
rm(household, household_features)

#marital features soa
load('rda/marital.rda')
marital_features <- names(marital) %>% str_subset("25_64")
train_soa_final %>%  dplyr::select(y, all_of(marital_features)) %>%
  #tidy the names, removing the _25_64
  rename_at(vars(contains("_25_64")), ~str_replace_all(., "_25_64", "")) %>%
  # pivot longer with the features
  pivot_longer(cols = -y,
               names_to = "feature", 
               values_to = "proportion") %>%
  ggplot(aes(y, proportion))  + 
  facet_wrap(.~feature, scales = "free_y") +
  geom_point(size=0.5) +
  geom_smooth(method = "lm") +
  ggtitle("Outcome y for marital features in SOA") +
  xlab("y, proportion of managerial & professional") +
  ylab("proportion of marital type")
#marital features msoa
# load("rda/train_set_final.rda")
train_set_final %>%  dplyr::select(y, all_of(marital_features)) %>%
  #tidy the names, removing the _25_64
  rename_at(vars(contains("_25_64")), ~str_replace_all(., "_25_64", "")) %>%
  # pivot longer with the features
  pivot_longer(cols = -y,
               names_to = "feature", 
               values_to = "proportion") %>%
  ggplot(aes(y, proportion))  + 
  facet_wrap(.~feature, scales = "free_y") +
  geom_point(size=0.5) +
  geom_smooth(method = "lm") +
  ggtitle("Outcome y for marital features in MSOA") +
  xlab("y, proportion of managerial & professional") +
  ylab("proportion of marital type")
# tidy up
rm(marital, marital_features)


#### running without the qualification features ####
## msoa data
# removing the qualification data
# get the names of the features
# load('rda/qualifications.rda')
load("rda/test_set_final.rda")
# load("rda/rmse_results.rda")
# qualifications_features <- names(qualifications) %>% str_subset("25_64")
train_msoa_noqual <- train_set_final %>% 
  dplyr::select(-all_of(qualifications_features))
# train with the no qualification data set
result_glm_train_msoa_noqual <- 
  results_train_method(train_msoa_noqual, test_set_final, "glm")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_glm_train_msoa_noqual$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance
importance <- varImp(result_glm_train_msoa_noqual$train, scale=FALSE)
plot(importance, 20)
importance$importance
# tidy up
save(result_glm_train_msoa_noqual, file="rda/result_glm_train_msoa_noqual.rda")
# load("rda/result_glm_train_msoa_noqual.rda")
save(rmse_results, file="rda/rmse_results.rda")
rm(qualifications, qualifications_features)
rm(train_msoa_noqual, result_glm_train_msoa_noqual)
rm(importance)

#### ethnicity deep dive ####
#ethnicity features vs y soa
load('rda/ethnicity.rda')
ethnicity_features <- names(ethnicity) %>% str_subset("25_64")
train_soa_final %>% 
  #select only the ethnicity features
  dplyr::select(y, all_of(ethnicity_features)) %>%
  #tidy the names, removing the _25_64
  rename_at(vars(contains("_25_64")), ~str_replace_all(., "_25_64", "")) %>%
  # rename(str_replace_all(., "_25_64", "")) %>%
  # pivot longer with the features
  pivot_longer(cols = -y,
               names_to = "feature", 
               values_to = "proportion") %>%
  # str_replace_all(., "_25_64", "") %>%
  ggplot(aes(y, proportion))  + 
  facet_wrap(.~feature, scales = "free_y") +
  geom_point(size=0.5) +
  geom_smooth(method = "lm") +
  ggtitle("Outcome y for ethnicity features in SOA") +
  xlab("y, proportion of managerial & professional") +
  ylab("proportion of ethnicity type")
#ethnicity features msoa
# load("rda/train_set_final.rda")
train_set_final %>%  
  #select only the ethnicity features
  dplyr::select(y, all_of(ethnicity_features)) %>%
  #tidy the names, removing the _25_64
  rename_at(vars(contains("_25_64")), ~str_replace_all(., "_25_64", "")) %>%
  # pivot longer with the features
  pivot_longer(cols = -y,
               names_to = "feature", 
               values_to = "proportion") %>%
  ggplot(aes(y, proportion))  + 
  facet_wrap(.~feature, scales = "free_y") +
  geom_point(size=0.5) +
  geom_smooth(method = "lm") +
  ggtitle("Outcome y for ethnicity features in MSOA") +
  xlab("y, proportion of managerial & professional") +
  ylab("proportion of ethnicity type")

# correlation with p value
# uses the Hmisc package
if (!require('Hmisc')) install.packages('Hmisc'); library('Hmisc')
# extract the ethnicity features for SOA
# calculate the correlation matrix and p value matrix
train_soa_rcorr <- rcorr(as.matrix(train_soa_final))
# extract the p-value result for y
train_soa_ethnicity_p <- 
  data.frame(train_soa_rcorr$P) %>%
  # select the y column
  dplyr::select(y) %>%
  rownames_to_column(var="feature") %>%
  # choose only the ethnicity features
  filter(feature %in% ethnicity_features) %>%
  dplyr::rename("p_value" = "y")
# extract correlation, and display with p-value result
data.frame(train_soa_rcorr$r) %>%
  # select the y column
  dplyr::select(y) %>%
  rownames_to_column(var="feature") %>%
  # choose only the ethnicity features
  filter(feature %in% ethnicity_features) %>%
  dplyr::rename("y_correlation_coefficient" = "y") %>%
  #include the p_value
  left_join(train_soa_ethnicity_p) %>%
  # reorder and display
  arrange(desc(abs(y_correlation_coefficient))) %>%
  knitr::kable(caption = "Correlation and P results for ethnicity and y in SOA") 

# extract the ethnicity features for MSOA
# calculate the correlation matrix and p valuer matrix
train_msoa_rcorr <- rcorr(as.matrix(train_set_final))
# extract the p-value result for y
train_msoa_ethnicity_p <- 
  data.frame(train_msoa_rcorr$P) %>%
  # select the y column
  dplyr::select(y) %>%
  rownames_to_column(var="feature") %>%
  # choose only the ethnicity features
  filter(feature %in% ethnicity_features) %>%
  dplyr::rename("p_value" = "y")
# extract correlation, and display with p-value result
data.frame(train_msoa_rcorr$r) %>%
  # select the y column
  dplyr::select(y) %>%
  rownames_to_column(var="feature") %>%
  # choose only the ethnicity features
  filter(feature %in% ethnicity_features) %>%
  dplyr::rename("y_correlation_coefficient" = "y") %>%
  #include the p_value
  left_join(train_msoa_ethnicity_p) %>%
  # reorder and display
  arrange(desc(abs(y_correlation_coefficient))) %>%
  knitr::kable(caption = "Correlation and P results for ethnicity and y in MSOA") 
#ethnicity features vs level4 soa
# load('rda/ethnicity.rda')
# ethnicity_features <- names(ethnicity) %>% str_subset("25_64")
train_soa_final %>% 
  #select only the ethnicity features
  dplyr::select(level4_25_64, all_of(ethnicity_features)) %>%
  #tidy the names, removing the _25_64
  rename_at(vars(contains("_25_64")), ~str_replace_all(., "_25_64", "")) %>%
  # rename(str_replace_all(., "_25_64", "")) %>%
  # pivot longer with the features
  pivot_longer(cols = -level4,
               names_to = "feature", 
               values_to = "proportion") %>%
  # str_replace_all(., "_25_64", "") %>%
  ggplot(aes(level4, proportion))  + 
  facet_wrap(.~feature, scales = "free_y") +
  geom_point(size=0.5) +
  geom_smooth(method = "lm") +
  ggtitle("Level4 qualifications for ethnicity features in SOA") +
  xlab("level4, proportion of degree or equivalent") +
  ylab("proportion of ethnicity type")
#ethnicity features msoa
# load("rda/train_set_final.rda")
train_set_final %>%  
  #select only the ethnicity features
  dplyr::select(level4_25_64, all_of(ethnicity_features)) %>%
  #tidy the names, removing the _25_64
  rename_at(vars(contains("_25_64")), ~str_replace_all(., "_25_64", "")) %>%
  # pivot longer with the features
  pivot_longer(cols = -level4,
               names_to = "feature", 
               values_to = "proportion") %>%
  ggplot(aes(level4, proportion))  + 
  facet_wrap(.~feature, scales = "free_y") +
  geom_point(size=0.5) +
  geom_smooth(method = "lm") +
  ggtitle("Level4 qualifications for ethnicity features in MSOA") +
  xlab("level4, proportion of degree or equivalent") +
  ylab("proportion of ethnicity type")

# extract the ethnicity features for SOA
# extract the p-value result for level4
train_soa_level4_p <- 
  data.frame(train_soa_rcorr$P) %>%
  # select the level4 columns
  dplyr::select(level4_25_64) %>%
  rownames_to_column(var="feature") %>%
  # choose only the ethnicity features
  filter(feature %in% ethnicity_features) %>%
  dplyr::rename("p_value" = "level4_25_64")
# extract correlation, and display with p-value result
data.frame(train_soa_rcorr$r) %>%
  # select the level4 columns
  dplyr::select(level4_25_64) %>%
  rownames_to_column(var="feature") %>%
  # choose only the ethnicity features
  filter(feature %in% ethnicity_features) %>%
  dplyr::rename("level4_correlation_coefficient" = "level4_25_64") %>%
  #include the p_value
  left_join(train_soa_level4_p) %>%
  # reorder and display
  arrange(desc(abs(level4_correlation_coefficient))) %>%
  knitr::kable(caption = "Correlation and P results for ethnicity and level4 in SOA") 

# extract the ethnicity features for MSOA
# extract the p-value result for level4
train_msoa_level4_p <- 
  data.frame(train_msoa_rcorr$P) %>%
  # select the level4 columns
  dplyr::select(level4_25_64) %>%
  rownames_to_column(var="feature") %>%
  # choose only the ethnicity features
  filter(feature %in% ethnicity_features) %>%
  dplyr::rename("p_value" = "level4_25_64")
# extract correlation, and display with p-value result
data.frame(train_msoa_rcorr$r) %>%
  # select the level4 columns
  dplyr::select(level4_25_64) %>%
  rownames_to_column(var="feature") %>%
  # choose only the ethnicity features
  filter(feature %in% ethnicity_features) %>%
  dplyr::rename("level4_correlation_coefficient" = "level4_25_64") %>%
  #include the p_value
  left_join(train_msoa_level4_p) %>%
  # reorder and display
  arrange(desc(abs(level4_correlation_coefficient))) %>%
  knitr::kable(caption = "Correlation and P results for ethnicity and level4 in MSOA") 

# looking at stratification for the black african, soa
train_soa_final %>%
  # select columns
  dplyr::select(y, level4_25_64, black_african_25_64) %>%
  # round to nearest 0.5
  mutate(black_african_strata = (round(2*black_african_25_64, 1))/2) %>%
  # filter to those strata with more than 50 samples
  filter(black_african_strata <= 0.4) %>%
  ggplot(aes(y, level4_25_64, col=black_african_25_64))  + 
  facet_wrap(.~black_african_strata) +
  geom_point(size=0.5) +
  geom_smooth(method = "lm") +
  ggtitle("Outcome y for level4 features, stratified by 'Black African' in SOA") +
  xlab("y, proportion of managerial & professional") +
  ylab("proportion of level4 qualifications")

# looking at stratification for the black other, soa
train_soa_final %>%
  # select columns
  dplyr::select(y, level4_25_64, black_other_25_64) %>%
  # round to nearest 0.5
  mutate(black_other_strata = (round(2*black_other_25_64, 1))/2) %>%
  # filter to those strata with more than 50 samples
  filter(black_other_strata <= 0.3) %>%
  ggplot(aes(y, level4_25_64, col=black_other_25_64))  + 
  facet_wrap(.~black_other_strata) +
  geom_point(size=0.5) +
  geom_smooth(method = "lm") +
  ggtitle("Outcome y for level4 features, stratified by 'Black other' in SOA") +
  xlab("y, proportion of managerial & professional") +
  ylab("proportion of level4 qualifications")

# graphing the correlation for each ethnicity
# create a list of a correlation table for each ethnicity
eth_corr_list <- lapply(ethnicity_features, function(ethnicity){
  print(paste("working with", ethnicity, "feature"))
  tmp <- train_soa_final %>% 
    # round to nearest 0.5
    dplyr::mutate(strata = round(.data[[ethnicity]], 1)) %>%
    # filter to those strata with more than 50 samples
    filter(strata <= 1) %>%
    # group the data by strata
    group_by(strata) %>%
    dplyr::summarise(correlation = cor(y, level4_25_64))
  colnames(tmp) <- c("strata", ethnicity)
  tmp
})
# create starter data frame
eth_corr <- data.frame(eth_corr_list[1]) 
# loop around and add a column from each table to the data frame
for(i in 2:length(eth_corr_list)) {
  eth_corr <- 
    eth_corr %>%
    left_join(data.frame(eth_corr_list[i]))
}
# graph the proportions
eth_corr %>%
  pivot_longer(cols = -strata, 
             names_to = "ethnicity", 
             values_to = "correlation") %>%
  filter(correlation >= 0.4) %>%
  ggplot(aes(strata, correlation, col=ethnicity))  + 
  geom_line() +
  geom_hline(yintercept=0.905) +
  geom_text(aes(0.5, 0.905,
                label="Overall correlation coefficient"),
            nudge_y=0.010, size = 3.5, col="black") +
  ggtitle("Correlation by ethnicity between y and level4 in SOA") +
  xlab("proportion of residents with ethnicity") +
  ylab("correlation between y and level4 qualifications")

# tidy
rm(eth_corr_list, eth_corr, ethnicity) 
rm(train_msoa_ethnicity_p, train_msoa_level4_p)
rm(train_msoa_rcorr, train_soa_rcorr)
rm(train_soa_level4_p, train_soa_ethnicity_p)
rm(i, ethnicity_features)

# look at ratios of ethnicity and occupation and qualification in countries in the UK
## download the data
#  <option value="TYPE499">countries</option>
# load in the Highest level of qualification by ethnic group by age
# https://www.nomisweb.co.uk/census/2011/dc5202ew
# load in the Occupation by ethnic group by sex by age data
# https://www.nomisweb.co.uk/census/2011/dc6213ew
censusdata_list <- c("dc5202ew", "dc6213ew")
# run the function to download the csv files
download_censusdata("TYPE499", censusdata_list)

# load in the Highest level of qualification by ethnic group by age
# https://www.nomisweb.co.uk/census/2011/dc5202ew
geographytype <- "TYPE499"
data <- ingest("dc5202ew", geographytype)
# put together a table with the level4 qualifications by ethnicity
ethnicity_level4_25_64_raw <- data %>%
  #keeping only the data relating to age 25 to 64
  dplyr::select(contains("geography") | 
           contains("Age 25 to 34") |
           contains("Age 35 to 49") | 
           contains("Age 50 to 64") ) %>%
  #keeping only the data relating to Highest Level of Qualification: Level 4 qualifications 
  dplyr::select(contains("geography") | contains("Highest Level of Qualification: Level 4 qualifications") ) %>%
  # adding up the year groups
  rowwise() %>%
  dplyr::mutate("all_25_64" = 
           sum(across(contains("Ethnic Group: All categories"))) ) %>%
  dplyr::mutate("white_uk_25_64" = 
           sum(across(contains("Ethnic Group: White: English/Welsh/Scottish/Northern Irish/British"))) ) %>%
  dplyr::mutate("white_irish_25_64" = 
             sum(across(contains("Ethnic Group: White: Irish"))) ) %>%
  dplyr::mutate("white_other_25_64" = 
           sum(across(contains("Ethnic Group: White: Other White"))) ) %>%
  dplyr::mutate("mixed_25_64" = 
             sum(across(contains("Ethnic Group: Mixed/multiple ethnic group"))) ) %>%
  dplyr::mutate("asian_25_64" = 
           sum(across(contains("Ethnic Group: Asian/Asian British"))) ) %>%
  dplyr::mutate("black_25_64" = 
             sum(across(contains("Ethnic Group: Black/African/Caribbean/Black British"))) ) %>%
  dplyr::mutate("other_25_64" = 
           sum(across(contains("Ethnic Group: Other ethnic group"))) ) %>%
  dplyr::rename("geo_name" = "geography") %>%
  dplyr::select(contains("geo_name") | contains("25_64")) %>%
  filter(geo_name == "England and Wales")

# put together a table with all the 25 to 64 residents with the same data
ethnicity_all_25_64_raw <- data %>%
  #keeping only the data relating to age 25 to 64
  dplyr::select(contains("geography") | 
           contains("Age 25 to 34") |
           contains("Age 35 to 49") | 
           contains("Age 50 to 64") ) %>%
  #keeping only the data relating to Highest Level of Qualification: All, to give all residents 25 to 64 by ethinicity 
  dplyr::select(contains("geography") | 
           contains("Highest Level of Qualification: All categories:") ) %>%
  # adding up the year groups
  rowwise() %>%
  dplyr::mutate("all_25_64" = 
           sum(across(contains("Ethnic Group: All categories"))) ) %>%
  dplyr::mutate("white_uk_25_64" = 
           sum(across(contains("Ethnic Group: White: English/Welsh/Scottish/Northern Irish/British"))) ) %>%
  dplyr::mutate("white_irish_25_64" = 
           sum(across(contains("Ethnic Group: White: Irish"))) ) %>%
  dplyr::mutate("white_other_25_64" = 
           sum(across(contains("Ethnic Group: White: Other White"))) ) %>%
  dplyr::mutate("mixed_25_64" = 
           sum(across(contains("Ethnic Group: Mixed/multiple ethnic group"))) ) %>%
  dplyr::mutate("asian_25_64" = 
           sum(across(contains("Ethnic Group: Asian/Asian British"))) ) %>%
  dplyr::mutate("black_25_64" = 
           sum(across(contains("Ethnic Group: Black/African/Caribbean/Black British"))) ) %>%
  dplyr::mutate("other_25_64" = 
           sum(across(contains("Ethnic Group: Other ethnic group"))) ) %>%
  dplyr::rename("geo_name" = "geography") %>%
  dplyr::select(contains("geo_name") | contains("25_64")) %>%
  filter(geo_name == "England and Wales")

# load in the Occupation by ethnic group by sex by age data
# https://www.nomisweb.co.uk/census/2011/dc6213ew
# geographytype <- "TYPE499"
data <- ingest("dc6213ew", geographytype)

# put together a table with the level4 qualifications by ethnicity
ethnicity_y_25_64_raw <- data %>%
  #keeping only the data relating to all (not by gender)
  dplyr::select(contains("geography") | 
           contains("Sex: All persons") ) %>%
  #keeping only the data relating to age 25 to 64
  dplyr::select(contains("geography") | 
           contains("Age 25 to 49") | 
           contains("Age 50 to 64") ) %>%
  #keeping only the data relating to Highest Level of Qualification: All, to give all residents 25 to 64 by ethinicity 
  dplyr::select(contains("geography") | 
           contains("Occupation: 1. Managers") |
           contains("Occupation: 2. Professional") ) %>%
  # adding up the year groups
  rowwise() %>%
  dplyr::mutate("all_25_64" = 
           sum(across(contains("Ethnic Group: All categories"))) ) %>%
  dplyr::mutate("white_uk_25_64" = 
           sum(across(contains("Ethnic Group: White: English/Welsh/Scottish/Northern Irish/British"))) ) %>%
  dplyr::mutate("white_irish_25_64" = 
           sum(across(contains("Ethnic Group: White: Irish"))) ) %>%
  dplyr::mutate("white_other_25_64" = 
           sum(across(contains("Ethnic Group: White: Other White"))) +
           sum(across(contains("Ethnic Group: White: Gypsy or Irish Traveller"))) ) %>%
  dplyr::mutate("mixed_25_64" = 
           sum(across(contains("Ethnic Group: Mixed/multiple ethnic group: Total"))) ) %>%
  dplyr::mutate("asian_25_64" = 
           sum(across(contains("Ethnic Group: Asian/Asian British: Total"))) ) %>%
  dplyr::mutate("black_25_64" = 
           sum(across(contains("Ethnic Group: Black/African/Caribbean/Black British: Total"))) ) %>%
  dplyr::mutate("other_25_64" = 
           sum(across(contains("Ethnic Group: Other ethnic group: Total"))) ) %>%
  dplyr::rename("geo_name" = "geography") %>%
  dplyr::select(contains("geo_name") | contains("25_64")) %>%
  filter(geo_name == "England and Wales")

# join together
# pivot to long form
ethnicity_y_25_64_long <- ethnicity_y_25_64_raw %>%
  rename_at(vars(contains("_25_64")), ~str_replace_all(., "_25_64", "")) %>%
  pivot_longer(cols = -geo_name,
               names_to = "ethnicity",
               values_to = "occupation_25_64")
ethnicity_level4_25_64_long <- ethnicity_level4_25_64_raw %>%
  rename_at(vars(contains("_25_64")), ~str_replace_all(., "_25_64", "")) %>%
  pivot_longer(cols = -geo_name,
               names_to = "ethnicity",
               values_to = "qualification_25_64")
# join in to a single table
ethnicity_25_64_raw <- ethnicity_all_25_64_raw %>%
  rename_at(vars(contains("_25_64")), ~str_replace_all(., "_25_64", "")) %>%
  pivot_longer(cols = -geo_name,
               names_to = "ethnicity",
               values_to = "all_25_64") %>%
  left_join(ethnicity_y_25_64_long) %>%
  left_join(ethnicity_level4_25_64_long)

# tidy
save(ethnicity_25_64_raw, file="rda/ethnicity_25_64_raw.rda")
rm(ethnicity_level4_25_64_long, ethnicity_y_25_64_long, ethnicity_all_25_64_raw)
rm(ethnicity_level4_25_64_raw, ethnicity_y_25_64_raw)
rm(data, censusdata_list)

# graph
load("rda/ethnicity_25_64_raw.rda")
# calculate the ratio of the working population for each ethnicity
ethnicity_25_64 <- ethnicity_25_64_raw %>%
  mutate(senior_occupation = occupation_25_64 / all_25_64) %>%
  mutate(level4_qualification  = qualification_25_64 / all_25_64) %>%
  dplyr::select(ethnicity, senior_occupation, level4_qualification)
# put together a list in order based on the senior_occupation value
ethnicity_graph_order <- ethnicity_25_64 %>%
  arrange(senior_occupation) %>% .$ethnicity
# plot the graph
ethnicity_25_64 %>% 
  # pivot longer to include the proportions in one columns
  pivot_longer(cols = -ethnicity,
               names_to = "ratios",
               values_to = "population_proportion") %>%
  ggplot(aes(ethnicity, population_proportion, fill=ratios)) +
  geom_bar(stat="identity", position="dodge") +
  geom_hline(aes(yintercept=ethnicity_25_64$senior_occupation[1])) +
  geom_text(aes(4.5, ethnicity_25_64$senior_occupation[1],
             label="senior occupation for all residents 25 to 64"), 
            nudge_y=0.015, size = 3.5) + 
  geom_hline(yintercept=ethnicity_25_64$level4_qualification[1]) +
  geom_text(aes(4.5, ethnicity_25_64$level4_qualification[1],
                label="level4 qualification for all residents 25 to 64"), 
            nudge_y=0.015, size = 3.5) + 
# using the list earlier for the order on the x axis
  scale_x_discrete(limits=ethnicity_graph_order) +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1)) +
  ggtitle("Proportions of 25 to 64 year olds in England and Wales") +
  xlab("Ethnic groups") +
  ylab("Proportion of each ethnic group")

#####  the end ######