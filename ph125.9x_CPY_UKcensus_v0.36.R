#### ph125.9x Capstone choose your own project - UK census ####
# Paul Ceely
# 07/07/2020
####################initial set up##########################
setwd("~/Documents/study/Data-Science-R/ph125.9.Capstone/ChooseYourOwn")
# raw data in ./data, R objects in ./rda
# set up libraries
library(tidyverse)
# library(ggplot2)
library(stringr)
library(rvest)
# library(tidyr)
# library(purrr)
library(caret)
options(digits = 6)


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

# https://www.nomisweb.co.uk/census/2011/ks101uk

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


# Initially, TYPE480 regions
# then TYPE297 'super output areas - middle layer 2011
geographytype <- "TYPE297" #super output areas - middle layer
# geographytype <- "TYPE499" #countries
geographytype <- "TYPE480" #regions
# nomis_census_csv(censusdata, geographytype)
#creating list of census tables to take
censusdata_list <- c("ks101ew", "ks608ew","ks102ew", "ks201ew","ks501ew", "ks103ew", "ks209ew", "ks105ew", "ks605ew", "qs203ew")
# apply the list to download all of the data
tmp <- lapply(censusdata_list, function(censusdata){
  # pause for a few random seconds to avoid annoying nomis
  time <- sample(4:17,1)
  print(paste("pausing", time, "seconds"))
  Sys.sleep(time)
  #run the function to generate and download the file
  nomis_census_csv(censusdata, geographytype)
})
### tidy, take backup:
rm(tmp, filled_form, nomis_form, nomis_session, nomis_session2, filename, nomis_url, nomis_url_root, censusdata_list, censusdata, time)


############ Import and clean the prediction #####################
options(digits = 3)
# create function to read file
ingest <- function(censustable){
  print(paste("ingesting", censustable))
  # generate file name for loading
  filename <- paste(censustable, geographytype, sep = "_") %>%
    paste0(., ".csv") %>%
    paste0("data/", .)
  print(filename)
  #read the file to data
  # data <- read_csv(filename)
  return(read_csv(filename))
}


#load in occupation data
# https://www.nomisweb.co.uk/census/2011/ks608ew
# import the new table with occupation data
data <- ingest("ks608ew")
names(data)
# create mgr-prf ratios
occupation <- data %>%
  rename("geo_code" = "geography code", "geo_name" = "geography", "geo_type" = "Rural Urban", "occupation_all" = "Sex: All persons; Occupation: All categories: Occupation; measures: Value") %>%
  rowwise() %>%
  # calculate the ratiof of managers and professionals
  mutate(y = 
           (sum(across(contains("Sex: All persons; Occupation: 1. Managers"))) +
           sum(across(contains("Sex: All persons; Occupation: 2. Professional "))) )/occupation_all) %>% 
  select(geo_name, geo_code, geo_type, y, occupation_all)
occupation

#now creating a function for repeatability
occupation_target <- function(data){
  occupation <- data %>%
    rename("geo_code" = "geography code", "geo_name" = "geography", "geo_type" = "Rural Urban", "occupation_all" = "Sex: All persons; Occupation: All categories: Occupation; measures: Value") %>%
    rowwise() %>%
    # calculate the ratiof of managers and professionals
    mutate(y = 
             (sum(across(contains("Sex: All persons; Occupation: 1. Managers"))) +
                sum(across(contains("Sex: All persons; Occupation: 2. Professional "))) )/occupation_all) %>% 
    select(geo_name, geo_code, geo_type, y, occupation_all)
  #save file
  save(occupation, file='rda/occupation.rda')
}

#running the function on the data set
occupation_target(data)
#adding to main data set
load('rda/occupation.rda')
data_set <- occupation
#tidy up
save(data_set, file='rda/data_set.rda')
rm(occupation, occup_tmp)


############ Import and clean the data #####################
#step wise adding more tables from the census to put together potential predictors
#load in residents and sex data
data <- ingest("ks101ew")
names(data)
colnames(data) <- c("date", "geo_name", "geo_code", "geo_type", "all_residents", "males", "females")
sex <- data %>% 
  select(2:7) %>%
  mutate(female_ratio = females/all_residents)
#adding to main data set
data_set <- data_set %>%
  left_join(sex) %>%
  select(-females, -males)
save(data_set, file='rda/data_set.rda')
save(sex, file='rda/sex.rda')


# load in age data
# https://www.nomisweb.co.uk/census/2011/ks102ew
# import the new table with age data
data <- ingest("ks102ew")
names(data)
#look at all of the ages, to find the right ones to work on
age <- data %>%
  rename_at(vars(contains("Age")), ~str_replace_all(., "; measures: Value", "")) %>%
  rename_at(vars(contains("Age")), ~str_replace_all(., "Age: Age ", "age_")) %>%
  rename_at(vars(contains("Age")), ~str_replace_all(., " to ", "_")) %>% 
  rename_at(vars(contains("over")), ~str_replace_all(., " and ", "_")) %>% 
  rename("geo_code" = "geography code", "all_residents" = "Age: All usual residents", "age_median" = "Age: Median Age") %>% 
  mutate("under_16" = age_0_4 + age_5_7 + age_8_9 + age_10_14 + age_15) %>%
  mutate("over_74" = age_75_84 + age_85_89 + age_90_over) %>%
  mutate("age_16_to_74" = all_residents - over_74 - under_16) %>%
  mutate("age_20_to_64" = (age_16_to_74 - age_16_17 - age_18_19 - age_65_74)) %>%
  select(geo_code, all_residents, age_16_to_74, age_20_to_64, age_median)
names(age)
data_set %>%
#comparing to the all occupation number in the data set
  left_join(age) %>%
  select(geo_name, occupation_all, all_residents, age_16_to_74, age_20_to_64) %>%
  knitr::kable()
age <- age %>%
  select(-age_16_to_74, -age_20_to_64)

# rewriting as a function for the median age only
age_predictors <- function(data){
  age <- data %>%
    rename("geo_code" = "geography code", "all_residents" = "Age: All usual residents; measures: Value", "age_median" = "Age: Median Age; measures: Value") %>% select(geo_code, all_residents, age_median)
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
save(data_set, file='rda/data_set.rda')
rm(age)
names(data_set)


# trying to find the source of the Occupation total of people
# checking economic activity
nomis_census_csv("ks601ew", geographytype)
data <- ingest("ks601ew")
names(data)
economic <- data %>%
  rename_at(vars(contains("Sex")), ~str_replace_all(., "; measures: Value", "")) %>%
  rename_at(vars(contains("Sex")), ~str_replace_all(., "Sex: All persons; Economic Activity: ", "")) %>%
  select(2:22) %>% select(-"Rural Urban") %>%
  rename("geo_code" = "geography code", "geo_name" = "geography")
# names(economic)
#table comparing the numbers
economic %>%
  left_join(data_set) %>%
  select(-y, -female_ratio, -occ_ratio, -age_median) %>%
  select(geo_name, geo_code, "All usual residents aged 16 to 74", 
  "Economically active", "Economically active: In employment", 
  "occupation_all", "all_residents") %>%
  knitr::kable()
rm(economic, occupation)

# next to load in the ethnicity data
# https://www.nomisweb.co.uk/census/2011/lc2101ew
# updating with age and ethnicity
nomis_census_csv("lc2101ew", geographytype)
data <- ingest("lc2101ew")
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
  mutate("age25_64 All categories: Ethnic group" = sum(across(contains("All categories: Ethnic group"))), .before = "Age 25 to 49; All categories: Ethnic group") %>%
  mutate("age25_64 White: English/Welsh/Scottish/Northern Irish/British" = sum(across(contains("White: English/Welsh/Scottish/Northern Irish/British"))), .before = "Age 25 to 49; All categories: Ethnic group") %>%
  mutate("age25_64 White: Irish" = sum(across(contains("White: Irish"))), .before = "Age 25 to 49; All categories: Ethnic group") %>%
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
save(ethnicity_raw, file='rda/ethicity_raw.rda')
names(ethnicity_raw)

# reordering to identify which are the largest groups to retain
ethnicity_ordered <- column_to_rownames(ethnicity_raw, var="geography_code")
ethnicity_ordered <- ethnicity_ordered %>%  rbind("total" = colSums(ethnicity_ordered))
ethnicity_ordered <- ethnicity_ordered[, order(-ethnicity_ordered[which(rownames(ethnicity_ordered) == 'total'), ]) ]
names(ethnicity_ordered) %>% head(12)
names(ethnicity_ordered)

ethnicity_aggregated <- ethnicity_raw %>%
  rename("all_25_64" = "age25_64_All_categories_Ethnic_group",
  white_uk_25_64 = "age25_64_White_English_Welsh_Scottish_Northern_Irish_British") %>%
  mutate("white_other_25_64" = age25_64_White_Other_White + age25_64_White_Gypsy_or_Irish_Traveller, .after = white_uk_25_64) %>%
  select(-age25_64_White_Other_White, -age25_64_White_Gypsy_or_Irish_Traveller) %>% 
  rename(indian_25_64 = "age25_64_Asian_Asian_British_Indian",
  pakistani_25_64 = "age25_64_Asian_Asian_British_Pakistani",
  black_african_25_64 = "age25_64_Black_African_Caribbean_Black_British_African",
  asian_other_25_64 = "age25_64_Asian_Asian_British_Other_Asian") %>%
  mutate("black_other_25_64" = age25_64_Black_African_Caribbean_Black_British_Caribbean + age25_64_Black_African_Caribbean_Black_British_Other_Black, .after = asian_other_25_64) %>%
  select(-age25_64_Black_African_Caribbean_Black_British_Caribbean, -age25_64_Black_African_Caribbean_Black_British_Other_Black) %>%
  rename(white_irish_25_64 = "age25_64_White_Irish",
  chinese_25_64 = "age25_64_Asian_Asian_British_Chinese",
  bangladeshi_25_64 = "age25_64_Asian_Asian_British_Bangladeshi") %>%
  mutate(other_ethnicity_25_64 = age25_64_Other_ethnic_group_Any_other_ethnic_group +
           age25_64_Mixed_multiple_ethnic_group_White_and_Black_Caribbean +
           age25_64_Other_ethnic_group_Arab +
           age25_64_Mixed_multiple_ethnic_group_Other_Mixed +
           age25_64_Mixed_multiple_ethnic_group_White_and_Asian +
           age25_64_Mixed_multiple_ethnic_group_White_and_Black_African, .after = bangladeshi_25_64) %>%
  select(-age25_64_Other_ethnic_group_Any_other_ethnic_group, -age25_64_Mixed_multiple_ethnic_group_White_and_Black_Caribbean, -age25_64_Other_ethnic_group_Arab, -age25_64_Mixed_multiple_ethnic_group_Other_Mixed, -age25_64_Mixed_multiple_ethnic_group_White_and_Asian, -age25_64_Mixed_multiple_ethnic_group_White_and_Black_African) %>%
  rename("geo_code" = "geography_code") %>%
  rowwise() %>%
  mutate(checksum_all2 = sum(across(4:14)), .after = checksum_all)
# sum(ethnicity_aggregated$checksum_all2)
# After this aggregation, I now need to divide by the sums of the rows, to have a normalised value for each of the ethnicities.
ethnicity <- sweep(ethnicity_aggregated[,5:15], 1, rowSums(ethnicity_aggregated[,5:15]), FUN = "/")  %>%
  cbind(ethnicity_aggregated[,1])
#tmp, as a check
ethn_orig <- ethnicity

# creating a function for repeatability
ethnicity_predictors <- function(data){
  print("manipulating raw data")
  ethnicity_raw <- data %>%
    #keeping only the data relating to age 25 to 64
    select(contains("geography code") | contains("Age 25 to 49") | contains("Age 50 to 64") ) %>%
    # adding up the year groups
    rowwise() %>%
    mutate("all_25_64" = 
             sum(across(contains("All categories: Ethnic group"))) ) %>%
    mutate("white_uk_25_64" = 
             sum(across(contains("White: English/Welsh/Scottish/Northern Irish/British"))) ) %>%
    mutate("white_other_25_64" = 
             sum(across(contains("White: Other White"))) + 
             sum(across(contains("White: Gypsy or Irish Traveller"))) ) %>%
    mutate("white_irish_25_64" = 
             sum(across(contains("White: Irish"))) ) %>%
    mutate("indian_25_64" = 
             sum(across(contains("Asian/Asian British: Indian"))) ) %>%
    mutate("pakistani_25_64" = 
             sum(across(contains("Asian/Asian British: Pakistani"))) ) %>%
    mutate("bangladeshi_25_64" = 
             sum(across(contains("Asian/Asian British: Bangladeshi"))) ) %>%
    mutate("other_ethnicity_25_64" = 
             sum(across(contains("Other ethnic group: Any other ethnic group"))) +
             sum(across(contains("Mixed/multiple ethnic group: White and Black Caribbean"))) +
             sum(across(contains("Other ethnic group: Arab"))) +
             sum(across(contains("Mixed/multiple ethnic group: Other Mixed"))) +
             sum(across(contains("Mixed/multiple ethnic group: White and Asian"))) +
             sum(across(contains("Mixed/multiple ethnic group: White and Black African")))) %>%
    mutate("chinese_25_64" = 
             sum(across(contains("Asian/Asian British: Chinese"))) ) %>%
    mutate("asian_other_25_64" =
             sum(across(contains("Asian/Asian British: Other Asian"))) ) %>%
    mutate("black_other_25_64" = 
             sum(across(contains("Black/African/Caribbean/Black British: Caribbean"))) + 
             sum(across(contains("Black/African/Caribbean/Black British: Other Black"))) ) %>%
    mutate("black_african_25_64" = 
             sum(across(contains("Black/African/Caribbean/Black British: African"))) ) %>%
    rename("geo_code" = "geography code") %>%
    print(names(ethnicity)) %>%
    select(contains("geo_code") | contains("25_64")) %>%
    print(names(ethnicity))
  # After this aggregation, I now need to divide by the sums of the rows, to have a normalised value for each of the ethnicities.
  print("normalising the data")
  ethnicity <- sweep(ethnicity_raw[,3:13], 1, rowSums(ethnicity_raw[,3:13]), FUN = "/")  %>%
    cbind(ethnicity_raw[,1])
  print("saving the file")
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
save(data_set, file='rda/data_set.rda')
rm(ethnicity_aggregated, ethnicity_ordered, ethnicity_raw, ethnicity, ethn_orig)
rm(ethnicity_aggregated, ethnicity_ordered, ethnicity_raw, ethnicity)

# next to load in the sex/gender data, for ages 25 to 64 
# https://www.nomisweb.co.uk/census/2011/lc2101ew
# using the ethnicity data, which includes gender
data <- ingest("lc2101ew")
#creating the raw file
sex_25_64 <- data %>%
  #filtering all ethnicities
  select(contains("Ethnic Group: All categories:") | contains("geography code") ) %>%
  #filtering to the age groups in question
  select(contains("Age 25 to 49") | contains("Age 50 to 64") | contains("geography code") ) %>%
  #mutate to create the sums for comparison
  rowwise() %>%
  mutate("all_25_64"= sum(across(contains("Sex: All Persons")))) %>%
  mutate("female_25_64"= sum(across(contains("Sex: Female")))) %>%
  mutate("male_25_64"= sum(across(contains("Sex: Males")))) %>%
  mutate("checksum" = female_25_64 + male_25_64) %>%
  mutate(female_ratio_25_64 = female_25_64/all_25_64)%>% 
  rename("geo_code" = "geography code") 
#limit to the two columns necessary
sex_25_64 <- sex_25_64 %>%
  select(contains("female_ratio_25_64") | contains("geo_code") )

#recreating as a function for reuse
sex_predictors <- function(data){
  sex_25_64 <- data %>%
    select(contains("Ethnic Group: All categories:") | contains("geography code") ) %>%
    select(contains("Age 25 to 49") | contains("Age 50 to 64") | contains("geography code") ) %>%
    rowwise() %>%
    #create the ratio necessary
    mutate(female_ratio_25_64 = sum(across(contains("Sex: Female")))/sum(across(contains("Sex: All Persons")))) %>%
    rename("geo_code" = "geography code")  %>%
    select(contains("female_ratio_25_64") | contains("geo_code") )
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
# tidy from earlier female ratio for all ages
# data_set <- data_set %>%
#   select(-female_ratio)
save(data_set, file='rda/data_set.rda')
rm(sex, sex_25_64, sex_orig)


# qualifications
# https://www.nomisweb.co.uk/census/2011/lc5102ew
# import the new table with qualifications
nomis_census_csv("lc5102ew", geographytype)
data <- ingest("lc5102ew")
names(data)
# manipulate to be the 25 to 64 only
qualifications_raw <- data %>%
  #save only the 25 to 64 years
  select(contains("Age 25 to 34") | contains("Age 35 to 49") | contains("Age 50 to 64") | contains("geography code")) %>%
  # tidy names to help understand them
  rename_at(vars(contains("measures")), ~str_replace_all(., "; measures: Value", "")) %>%
  rename("geo_code" = "geography code") %>%
  rename_at(vars(contains("; Highest Level of Qualification: ")), ~str_replace_all(., "; Highest Level of Qualification: ", "_")) %>%
  #calculate the sums for the retained predictors
  rowwise() %>%
  mutate("all_qual_25_64"= sum(across(contains("All categories: ")))) %>%
  mutate("no_qual_25_64"= sum(across(contains("No qualifications")))) %>%
  mutate("level1_25_64"= sum(across(contains("Level 1 qualification")))) %>%
  mutate("level2_25_64"= sum(across(contains("Level 2 qualification")))) %>%
  mutate("level3_25_64"= sum(across(contains("Level 3 qualification")))) %>%
  mutate("level4_25_64"= sum(across(contains("Level 4 qualification")))) %>%
  mutate("other_qual_25_64"= sum(across(contains("Other qualifications")))) %>%
  mutate("apprentice_25_64"= sum(across(contains("Apprenticeship")))) %>%
  select( contains("geo_code") | contains("25_64")) %>%
  #checksum to check for errors
  mutate(checksum_all = sum(across(contains("25_64"))) - all_qual_25_64, .after = geo_code)

# normalise per area
qualifications <- sweep(qualifications_raw[,4:10], 1, rowSums(qualifications_raw[,4:10]), FUN = "/")  %>%
  cbind(qualifications_raw[,1])
# checksum to ensure no errors/typos...
# %>%
#   rowwise() %>%
#   mutate(checksum_all = sum(across(1:7)))

#to check the function works
# qual_orig <- qualifications

#creating a function for repeatability
qualifications_predictors <- function(data){
  qualifications_raw <- data %>%
    #save only the 25 to 64 years
    select(contains("Age 25 to 34") | contains("Age 35 to 49") | contains("Age 50 to 64") | contains("geography code")) %>%
    rename("geo_code" = "geography code") %>%
    #calculate the sums for the retained predictors
    rowwise() %>%
    mutate("all_qual_25_64"= sum(across(contains("All categories: ")))) %>%
    mutate("no_qual_25_64"= sum(across(contains("No qualifications")))) %>%
    mutate("level1_25_64"= sum(across(contains("Level 1 qualification")))) %>%
    mutate("level2_25_64"= sum(across(contains("Level 2 qualification")))) %>%
    mutate("level3_25_64"= sum(across(contains("Level 3 qualification")))) %>%
    mutate("level4_25_64"= sum(across(contains("Level 4 qualification")))) %>%
    mutate("other_qual_25_64"= sum(across(contains("Other qualifications")))) %>%
    mutate("apprentice_25_64"= sum(across(contains("Apprenticeship")))) %>%
    select( contains("geo_code") | contains("25_64"))
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
save(data_set, file='rda/data_set.rda')
rm(qualifications_raw, qualifications, qual_orig)
rm(tmp, tmp2)


# next marital status
# https://www.nomisweb.co.uk/census/2011/lc1101ew
# import the new table with marital status
nomis_census_csv("lc1101ew", geographytype)
data <- ingest("lc1101ew")
names(data)
# manipulate to be the 25 to 64 only
marital_raw <- data %>%
  #only all genders
  select(contains("Sex: All persons;") | contains("geography code")) %>%
  #save only the 25 to 64 years
  select(contains("Age 25 to 34") | contains("Age 35 to 49") | contains("Age 50 to 64") | contains("geography code")) %>%
  rename("geo_code" = "geography code") %>%
  #calculate for retained predictors
  rowwise() %>%
  mutate("all_marital_25_64"= sum(across(contains("All categories: ")))) %>%
  mutate("single_25_64"= sum(across(contains("Marital Status: Single")))) %>%
  mutate("married_25_64"= sum(across(contains("Marital Status: Married;")))) %>%
  mutate("civil_25_64"= sum(across(contains("Marital Status: In a registered same-sex civil")))) %>%
  mutate("separated_25_64"= sum(across(contains("Marital Status: Separated")))) %>%
  mutate("divorced_25_64"= sum(across(contains("Marital Status: Divorced")))) %>%
  mutate("widowed_25_64"= sum(across(contains("Marital Status: Widowed")))) %>%
  select(contains("geo_code") | contains("25_64")) %>%
  #checksum to confirm works ok
  mutate(checksum_all = sum(across(contains("25_64"))) - all_marital_25_64, .after = geo_code)
# normalise per area
marital <- sweep(marital_raw[,4:9], 1, rowSums(marital_raw[,4:9]), FUN = "/")  %>%
  cbind(marital_raw[,1])
# checksum to ensure no errors/typos...
# %>%
#   rowwise() %>%
#   mutate(checksum_all = sum(across(1:6)))
#to check the function works
# marital_orig <- marital

#creating a function for repeatability
marital_predictors <- function(data){
  marital_raw <- data %>%
    #only all genders
    select(contains("Sex: All persons;") | contains("geography code")) %>%
    #save only the 25 to 64 years
    select(contains("Age 25 to 34") | contains("Age 35 to 49") | contains("Age 50 to 64") | contains("geography code")) %>%
    rename("geo_code" = "geography code") %>%
    #calculate for retained predictors
    rowwise() %>%
    mutate("all_marital_25_64"= sum(across(contains("All categories: ")))) %>%
    mutate("single_25_64"= sum(across(contains("Marital Status: Single")))) %>%
    mutate("married_25_64"= sum(across(contains("Marital Status: Married;")))) %>%
    mutate("civil_25_64"= sum(across(contains("Marital Status: In a registered same-sex civil")))) %>%
    mutate("separated_25_64"= sum(across(contains("Marital Status: Separated")))) %>%
    mutate("divorced_25_64"= sum(across(contains("Marital Status: Divorced")))) %>%
    mutate("widowed_25_64"= sum(across(contains("Marital Status: Widowed")))) %>%
    select(contains("geo_code") | contains("25_64"))
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
#tidy up
save(data_set, file='rda/data_set.rda')
rm(marital, marital_raw, marital_orig)


# next religion
# https://www.nomisweb.co.uk/census/2011/lc2107ew
# import the new table with religion
nomis_census_csv("lc2107ew", geographytype)
data <- ingest("lc2107ew")
names(data)
# manipulate to be the 25 to 64 only
religion_raw <- data %>%
  # all genders
  select(contains("Sex: All persons;") | contains("geography code")) %>%
  #select the working age only
  select(contains("Age 25 to 34") | contains("Age 35 to 49") | contains("Age 50 to 64") | contains("geography code")) %>%
  rename("geo_code" = "geography code") %>%
  #calculate the sums across ages
  rowwise() %>%
  mutate("all_religion_25_64"= sum(across(contains("All categories:")))) %>%
  mutate("christian_25_64"= sum(across(contains("Christian")))) %>%
  mutate("buddhist_25_64"= sum(across(contains("Buddhist")))) %>%
  mutate("hindu_25_64"= sum(across(contains("Hindu")))) %>%
  mutate("jewish_25_64"= sum(across(contains("Jewish")))) %>%
  mutate("muslim_25_64"= sum(across(contains("Muslim")))) %>%
  mutate("sikh_25_64"= sum(across(contains("Sikh")))) %>%
  mutate("other_25_64"= sum(across(contains("Other religion;")))) %>%
  mutate("no_religion_25_64"= sum(across(contains("No religion;")))) %>%
  mutate("religion_not_stated_25_64"= sum(across(contains("Religion not stated;")))) %>%
  select(contains("geo_code") | contains("25_64")) %>%
  #checksum to ensure everything covered
  mutate(checksum_all = sum(across(contains("25_64"))) - all_religion_25_64, .after = geo_code)
# checking for how many as a proportion
religion_ordered <- column_to_rownames(religion_raw, var="geo_code")
religion_ordered <- religion_ordered %>%  
  rbind("total" = colSums(religion_ordered)) %>%
  rbind("proportion" = 100*colSums(religion_ordered)/29615071) 
religion_ordered <- religion_ordered[, order(-religion_ordered[which(rownames(religion_ordered) == 'total'), ]) ]
names(religion_ordered)
# normalise per area
religion <- sweep(religion_raw[,4:12], 1, rowSums(religion_raw[,4:12]), FUN = "/")  %>%
  cbind(religion_raw[,1])
# checksum to ensure no errors/typos...
# %>%
#   rowwise() %>%
#   mutate(checksum_all = sum(across(1:9)))
#check function works
# religion_orig <- religion

#creating a function for repeatability
religion_predictors <- function(data){
  religion_raw <- data %>%
    # all genders
    select(contains("Sex: All persons;") | contains("geography code")) %>%
    #select the working age only
    select(contains("Age 25 to 34") | contains("Age 35 to 49") | contains("Age 50 to 64") | contains("geography code")) %>%
    rename("geo_code" = "geography code") %>%
    #calculate the sums across ages
    rowwise() %>%
    mutate("all_religion_25_64"= sum(across(contains("All categories:")))) %>%
    mutate("christian_25_64"= sum(across(contains("Christian")))) %>%
    mutate("buddhist_25_64"= sum(across(contains("Buddhist")))) %>%
    mutate("hindu_25_64"= sum(across(contains("Hindu")))) %>%
    mutate("jewish_25_64"= sum(across(contains("Jewish")))) %>%
    mutate("muslim_25_64"= sum(across(contains("Muslim")))) %>%
    mutate("sikh_25_64"= sum(across(contains("Sikh")))) %>%
    mutate("other_25_64"= sum(across(contains("Other religion;")))) %>%
    mutate("no_religion_25_64"= sum(across(contains("No religion;")))) %>%
    mutate("religion_not_stated_25_64"= sum(across(contains("Religion not stated;")))) %>%
    select(contains("geo_code") | contains("25_64"))
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
save(data_set, file='rda/data_set.rda')
rm(religion, religion_raw, religion_ordered, religion_orig)


#next household composition
# https://www.nomisweb.co.uk/census/2011/lc1109ew
# import the new table with household
nomis_census_csv("lc1109ew", geographytype)
data <- ingest("lc1109ew")
names(data)
# manipulate to be the 25 and over only
household_raw <- data %>%
  # retain only the all sex (not female or male)
  select(contains("Sex: All persons;") | contains("geography code")) %>%
  select(contains("Age 25 to 34") | contains("Age 35 to 49") | contains("Age 50 and over") | contains("geography code")) %>%
  rename("geo_code" = "geography code") %>%
  # rename so that it is easier to understand the breakdowns
  rename_at(vars(contains("; Household Composition: ")), ~str_replace_all(., "; Household Composition: ", "_")) %>%
  rename_at(vars(contains("Sex: All persons; Age: Age")), ~str_replace_all(., "Sex: All persons; Age: Age ", "Age ")) %>%
  rename_at(vars(contains("; measures: Value")), ~str_replace_all(., "; measures: Value", "")) %>%
  # retain the totals, the total amount, and the over 65 columns
  select(contains("geo_code") | contains("All categories: Household composition") | contains(": Total") | contains("ged 65 and over")) %>%
  # #checksum, make sure the totals add up for 25 to 34
  # select(contains("geo_code") | contains("Age 25 to 34")) %>%
  # rowwise() %>%
  # mutate(checksum_25_34 = sum(across(contains("Age 25 to 34"))) - 
  #          sum(across(contains("All categories: Household composition"))) - 
  #          sum(across(contains("One family only: Total"))) -
  #          sum(across(contains("Other household types: Other"))), .after = geo_code) %>%
  # mutate(checksum_25_34_2 = 
  #          sum(across(contains("One person household: Total"))) + 
  #          sum(across(contains("Married or same-sex"))) + 
  #          sum(across(contains("Cohabiting couple:"))) +
  #          sum(across(contains("Lone parent:"))) +
  #          sum(across(contains("Other household types: Total"))), .after = geo_code)
  rowwise() %>%
  # combine the age categories to create the predictors
  mutate("all_household_25_64"= sum(across(contains("All categories:"))) -
           sum(across(contains("One person household: Aged 65 and over"))) -
           sum(across(contains("One family only: All aged 65 and over")))) %>%
  mutate("oneperson_25_64"= sum(across(contains("One person household: Total"))) -
           sum(across(contains("One person household: Aged 65 and over")))) %>%
  mutate("marriedfamily_25_64"= sum(across(contains("Married")))) %>%
  mutate("cohoabiting_25_64"= sum(across(contains("Cohabiting")))) %>%
  mutate("loneparent_25_64"= sum(across(contains("Lone parent")))) %>%
  mutate("otherhousehold_25_64"= sum(across(contains("Other household types: Total")))) %>%
  select(contains("geo_code") | contains("25_64")) %>%
  mutate(checksum_all = sum(across(contains("25_64"))) - all_household_25_64, .after = geo_code)
# normalise per area
household <- sweep(household_raw[,4:8], 1, rowSums(household_raw[,4:8]), FUN = "/")  %>%
  cbind(household_raw[,1])
# checksum to ensure no errors/typos...
# %>%
#   rowwise() %>%
#   mutate(checksum_all = sum(across(1:5)))

# rerunning as a function for reuse
household_predictors <- function(data){
  household_raw <- data %>%
    # retain only the all sex (not female or male)
    select(contains("Sex: All persons;") | contains("geography code")) %>%
    # retain only the specific age groups
    select(contains("Age 25 to 34") | contains("Age 35 to 49") | contains("Age 50 and over") | contains("geography code")) %>%
    rename("geo_code" = "geography code") %>%
    # retain the totals, the total amount, and the over 65 columns
    select(contains("geo_code") | contains("All categories: Household composition") | contains(": Total") | contains("ged 65 and over")) %>%
    rowwise() %>%
    # combine the age categories to create the predictors
    mutate("all_household_25_64"= sum(across(contains("All categories:"))) -
             sum(across(contains("One person household: Aged 65 and over"))) -
             sum(across(contains("One family only: All aged 65 and over")))) %>%
    mutate("oneperson_25_64"= sum(across(contains("One person household: Total"))) -
             sum(across(contains("One person household: Aged 65 and over")))) %>%
    mutate("marriedfamily_25_64"= sum(across(contains("Married")))) %>%
    mutate("cohoabiting_25_64"= sum(across(contains("Cohabiting")))) %>%
    mutate("loneparent_25_64"= sum(across(contains("Lone parent")))) %>%
    mutate("otherhousehold_25_64"= sum(across(contains("Other household types: Total")))) %>%
    select(contains("geo_code") | contains("25_64")) %>%
    mutate(checksum_all = sum(across(contains("25_64"))) - all_household_25_64, .after = geo_code)
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
save(data_set, file='rda/data_set.rda')
rm(household, household_raw, household_ordered, household.org, houseshold_predictors)


#next Industry
# https://www.nomisweb.co.uk/census/2011/lc6110ew
# import the new table with Industry
nomis_census_csv("lc6110ew", geographytype)
data <- ingest("lc6110ew")
names(data)
industry_raw <- data %>%
  # retain only the specific age groups
  select(contains("Age 25 to 34") | contains("Age 35 to 49") | contains("Age 50 to 64") | contains("geography code")) %>%
  rename("geo_code" = "geography code") %>%  
# combine the age categories to create the predictors
  rowwise() %>%
  mutate("all_industry_25_64"= sum(across(contains("All categories:")))) %>%
  mutate("industry_abde_25_64"= sum(across(contains("Industry: A, B, D, E ")))) %>%
  mutate("manufacturing_25_64"= sum(across(contains("Manufacturing")))) %>%
  mutate("construction_25_64"= sum(across(contains("Construction")))) %>%
  mutate("industry_gi_25_64"= sum(across(contains("Industry: G, I")))) %>%
  mutate("industry_hj_25_64"= sum(across(contains("Industry: H, J ")))) %>%
  mutate("industry_klmn_25_64"= sum(across(contains("Industry: K, L, M, N")))) %>%
  mutate("industry_opq_25_64"= sum(across(contains("Industry: O, P, Q ")))) %>%
  mutate("Industry_rstu_25_64"= sum(across(contains("Industry: R, S, T, U ")))) %>%
  select(contains("geo_code") | contains("25_64")) %>%
  mutate(checksum_all = sum(across(contains("25_64"))) - all_industry_25_64, .after = geo_code)
# normalise per area
industry <- sweep(industry_raw[,4:11], 1, rowSums(industry_raw[,4:11]), FUN = "/")  %>%
  cbind(industry_raw[,1]) 
# checksum to ensure no errors/typos...
 # %>%
  # rowwise() %>%
  # mutate(checksum_all = sum(across(1:8)))

# rerunning as a function for reuse
industry_predictors <- function(data){
  industry_raw <- data %>%
    # retain only the specific age groups
    select(contains("Age 25 to 34") | contains("Age 35 to 49") | contains("Age 50 to 64") | contains("geography code")) %>%
    rename("geo_code" = "geography code") %>%  
    # combine the age categories to create the predictors
    rowwise() %>%
    mutate("all_industry_25_64"= sum(across(contains("All categories:")))) %>%
    mutate("industry_abde_25_64"= sum(across(contains("Industry: A, B, D, E ")))) %>%
    mutate("manufacturing_25_64"= sum(across(contains("Manufacturing")))) %>%
    mutate("construction_25_64"= sum(across(contains("Construction")))) %>%
    mutate("industry_gi_25_64"= sum(across(contains("Industry: G, I")))) %>%
    mutate("industry_hj_25_64"= sum(across(contains("Industry: H, J ")))) %>%
    mutate("industry_klmn_25_64"= sum(across(contains("Industry: K, L, M, N")))) %>%
    mutate("industry_opq_25_64"= sum(across(contains("Industry: O, P, Q ")))) %>%
    mutate("Industry_rstu_25_64"= sum(across(contains("Industry: R, S, T, U ")))) %>%
    select(contains("geo_code") | contains("25_64")) %>%
    mutate(checksum_all = sum(across(contains("25_64"))) - all_industry_25_64, .after = geo_code)
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
save(data_set, file='rda/data_set.rda')
rm(industry, industry_raw, indust_orig)


# next country of birth
# https://www.nomisweb.co.uk/census/2011/lc2103ew
# import the new table with country of birth
nomis_census_csv("lc2103ew", geographytype)
data <- ingest("lc2103ew")
names(data)
# manipulate to be the 25 to 64 only
country_raw <- data %>%
  # retain only the all sex (not female or male)
  select(contains("Sex: All persons;") | contains("geography code")) %>%
  select(contains("Age 25 to 34") | contains("Age 35 to 49") | contains("Age 50 to 64") | contains("geography code")) %>%
  rename("geo_code" = "geography code") %>%
  # rename so that it is easier to understand the breakdowns
  rename_at(vars(contains("; Country of Birth: ")), ~str_replace_all(., "; Country of Birth: ", "_")) %>%
  rename_at(vars(contains("Sex: All persons; Age: Age")), ~str_replace_all(., "Sex: All persons; Age: Age ", "Age ")) %>%
  rename_at(vars(contains("; measures: Value")), ~str_replace_all(., "; measures: Value", "")) %>%
  # #checksum, make sure the totals add up for 25 to 34
  # select(contains("geo_code") | contains("Age 25 to 34")) %>%
  # rowwise() %>%
  # mutate(checksum_25_34 = 
  #          sum(across(contains("United Kingdom: Total"))) +
  #          sum(across(contains("Europe: Ireland"))) +
  #          sum(across(contains("Other Europe: EU countries: Total"))) +
  #          sum(across(contains("Other Europe: Rest of Europe"))) +
  #          sum(across(contains("Africa"))) +
  #          sum(across(contains("Middle East and Asia"))) +
  #          sum(across(contains("The Americas and the Caribbean"))) +
  #          sum(across(contains("Antarctica, Oceania"))), .after = geo_code)
  #          , .after = geo_code)
# combine the age categories to create the predictors
  rowwise() %>%
  mutate("all_country_25_64"= sum(across(contains("All categories:")))) %>%
  mutate("uk_25_64"= sum(across(contains("United Kingdom: Total")))) %>%
  mutate("eu_2001_25_64"= sum(across(contains("Other Europe: EU countries: Member countries in March 2001"))) + sum(across(contains("Europe: Ireland")))) %>%
  mutate("eu_rest_25_64"= sum(across(contains("Other Europe: EU countries: Accession countries April 2001 to March 2011"))) ) %>%
  mutate("europe_rest_25_64"= sum(across(contains("Other Europe: Rest of Europe")))) %>%
  mutate("africa_25_64"= sum(across(contains("Africa")))) %>%
  mutate("me_asia_25_64"= sum(across(contains("Middle East and Asia")))) %>%
  mutate("americas_25_64"= sum(across(contains("The Americas and the Caribbean")))) %>%
  mutate("other_country_25_64"= sum(across(contains("Antarctica, Oceania (including Australasia) and other")))) %>%
  select(contains("geo_code") | contains("25_64")) %>%
  mutate(checksum_all = sum(across(contains("25_64"))) - all_country_25_64, .after = geo_code)
# normalise per area
country <- sweep(country_raw[,4:11], 1, rowSums(country_raw[,4:11]), FUN = "/")  %>%
  cbind(country_raw[,1])
# checksum to ensure no errors/typos...
# %>%
#   rowwise() %>%
#   mutate(checksum_all = sum(across(1:9)))
# to check the function
# country_orig <- country
# country_raw_orig <- country_raw

# rerunning as a function for reuse
country_predictors <- function(data){
  country_raw <- data %>%
    # retain only the all sex (not female or male)
    select(contains("Sex: All persons;") | contains("geography code")) %>%
    select(contains("Age 25 to 34") | contains("Age 35 to 49") | contains("Age 50 to 64") | contains("geography code")) %>%
    rename("geo_code" = "geography code") %>%
    # combine the age categories to create the predictors
    rowwise() %>%
    mutate("all_country_25_64"= sum(across(contains("All categories:")))) %>%
    mutate("uk_25_64"= sum(across(contains("United Kingdom: Total")))) %>%
    mutate("eu_2001_25_64"= sum(across(contains("Other Europe: EU countries: Member countries in March 2001"))) + sum(across(contains("Europe: Ireland")))) %>%
    mutate("eu_rest_25_64"= sum(across(contains("Other Europe: EU countries: Accession countries April 2001 to March 2011"))) ) %>%
    mutate("europe_rest_25_64"= sum(across(contains("Other Europe: Rest of Europe")))) %>%
    mutate("africa_25_64"= sum(across(contains("Africa")))) %>%
    mutate("me_asia_25_64"= sum(across(contains("Middle East and Asia")))) %>%
    mutate("americas_25_64"= sum(across(contains("The Americas and the Caribbean")))) %>%
    mutate("other_country_25_64"= sum(across(contains("Antarctica, Oceania (including Australasia) and other")))) %>%
    select(contains("geo_code") | contains("25_64")) %>%
    mutate(checksum_all = sum(across(contains("25_64"))) - all_country_25_64, .after = geo_code)
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
save(data_set, file='rda/data_set.rda')
rm(country, country_raw, country_raw_orig, country_orig)

# check the function
# data_set_maincode <- data_set


####### repeatable code ##########
geographytype <- "TYPE480" #regions
geographytype <- "TYPE297" #super output areas - middle layer
# nomis_census_csv(censusdata, geographytype)
#creating list of census tables to take
censusdata_list <- c("ks608ew", "ks102ew", "lc2101ew", "lc2103ew","lc6110ew", "lc1109ew", "lc2107ew", "lc1101ew", "lc5102ew")
# apply the list to download all of the data
tmp <- lapply(censusdata_list, function(censusdata){
  # pause for a few random seconds to avoid annoying nomis
  time <- sample(4:17,1)
  print(paste("pausing", time, "seconds"))
  Sys.sleep(time)
  print(paste("downloading geography area", geographytype, "for table", censusdata))
  #run the function to generate and download the file
  nomis_census_csv(censusdata, geographytype)
})
rm(tmp)

#load in occupation data
# https://www.nomisweb.co.uk/census/2011/ks608ew
# import the new table with occupation data
data <- ingest("ks608ew")
#running the function on the data set
occupation_target(data)
#adding to main data set
load('rda/occupation.rda')
data_set <- occupation
save(data_set, file='rda/data_set.rda')
rm(occupation)

# load in age data
# https://www.nomisweb.co.uk/census/2011/ks102ew
# import the new table with age data
data <- ingest("ks102ew")
#running the function on the data set
age_predictors(data)
#adding to main data set
load('rda/age.rda')
data_set <- data_set %>%
  left_join(age)
rm(age)

# next to load in the ethnicity data
# https://www.nomisweb.co.uk/census/2011/lc2101ew
# updating with age and ethnicity
data <- ingest("lc2101ew")
#running the function on the data set
ethnicity_predictors(data)
#adding to main data set
load('rda/ethnicity.rda')
data_set <- data_set %>%
  left_join(ethnicity)
rm(ethnicity)

# https://www.nomisweb.co.uk/census/2011/lc2101ew
# next to load in the sex/gender data, for ages 25 to 64 
# using the ethnicity data, which includes gender
data <- ingest("lc2101ew")
#running the function on the data set
sex_predictors(data)
#adding to main data set
load('rda/sex_25_64.rda')
data_set <- data_set %>%
  left_join(sex_25_64)
rm(sex_25_64)

# qualifications
# https://www.nomisweb.co.uk/census/2011/lc5102ew
# import the new table with qualifications
data <- ingest("lc5102ew")
#running the function on the data set
qualifications_predictors(data)
#adding to main data set
load('rda/qualifications.rda')
data_set <- data_set %>%
  left_join(qualifications)
rm(qualifications)

# next marital status
# https://www.nomisweb.co.uk/census/2011/lc1101ew
# import the new table with marital
data <- ingest("lc1101ew")
#running the function on the data set
marital_predictors(data)
#adding to main data set
load('rda/marital.rda')
data_set <- data_set %>%
  left_join(marital)
rm(marital)

# next religion
# https://www.nomisweb.co.uk/census/2011/lc2107ew
# import the new table with religion
data <- ingest("lc2107ew")
#running the function on the data set
religion_predictors(data)
#adding to main data set
load('rda/religion.rda')
data_set <- data_set %>%
  left_join(religion)
rm(religion)

#next household composition
# https://www.nomisweb.co.uk/census/2011/lc1109ew
# import the new table with household
data <- ingest("lc1109ew")
#running the function on the data set
household_predictors(data)
#adding to main data set
load('rda/household.rda')
data_set <- data_set %>%
  left_join(household)
rm(household)

#next Industry
# https://www.nomisweb.co.uk/census/2011/lc6110ew
# import the new table with Industry
data <- ingest("lc6110ew")
#running the function on the data set
industry_predictors(data)  
#adding to main data set
load('rda/industry.rda')
data_set <- data_set %>%
  left_join(industry)
rm(industry)
# names(data_set)

# next country of birth
# https://www.nomisweb.co.uk/census/2011/lc2103ew
# import the new table with country of birth
data <- ingest("lc2103ew")
#running the function on the country of birth data set
country_predictors(data)
#adding to main data set
load('rda/country.rda')
data_set <- data_set %>%
  left_join(country)
rm(country, data)

#check function vs the main code
# names(data_set)
# names(data_set_maincode)
# identical(data_set, data_set_main2)

#save the data set
filename <- paste("data_set", geographytype, sep = "_") %>%
  paste0(., ".rda")
destfile <- paste0("rda/", filename)
save(data_set, file=destfile)
#clean up
rm(filename, destfile)
rm(ingest, marital_predictors, country_predictors, occupation_target)
rm(ethnicity_predictors, household_predictors, religion_predictors)
rm(industry_predictors, sex_predictors, qualifications_predictors, age_predictors)

############ Create main set, validation set #####################

# Validation set will be 10% of the data
set.seed(2011, sample.kind="Rounding")
test_index <- createDataPartition(y = data_set$y, times = 1, p = 0.1, list = FALSE)
main <- data_set[-test_index,]
validation <- data_set[test_index,]
### tidy, take backup:
save(main, file='rda/main.rda')
save(validation, file='rda/validation.rda')
rm(validation, data_set, test_index)


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
  #pivot to a long version with a row per feature/value
  pivot_longer(cols = (contains("25_64") | contains("ratio")), 
                       names_to = "feature", 
                       values_to = "proportion") %>%
  select(-occupation_all, -all_residents, -geo_type, -geo_name)
#plot the box plot
main_tidy %>%
  ggplot(aes(x=reorder(feature, proportion, FUN=mean), y=proportion))  + 
  geom_boxplot() +
  ggtitle("Boxplot of features") +
  ylab("proportion") + 
  theme(axis.text.x=element_text(angle = 90, hjust=1, vjust=0.5))

#choosing the 10 features with the lowest mean
main_tidy %>% group_by(feature) %>%
  summarise(mean=mean(proportion), sd=sd(proportion), max=max(proportion), min=min(proportion)) %>%
  arrange(mean) %>% head(10) %>%
  knitr::kable()

### calculating the correlation between features
correlationmatrix <- cor(main[,7:63])
# image of correlation matrix
heatmap(x = correlationmatrix, col = RColorBrewer::brewer.pal(11, "Spectral"))

# find attributes that are highly corrected
highlycorrelated <- findCorrelation(correlationmatrix, cutoff=0.8, exact = TRUE, names=TRUE)
highlycorrelated

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

# finally geography
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
  #for the MSOA
  select(MSOA11CD, LAD11CD) %>%
  rename(local_area = LAD11CD, geo_code = MSOA11CD) %>%
  #for the SOA
  # select(OA11CD, LAD11CD) %>%
  # rename(local_area = LAD11CD, geo_code = OA11CD) %>%
  unique()
head(geo_lookup)

#however, for many models to work, the feature needs to be numeric, so I need to convert the character strings to numbers
#calculating a list of strings within the local area to be rewritten as numbers.  Rewriting as much as possible
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

#but this may not work for some algorithms
# in addition creating a column per region
# https://opendata.arcgis.com/datasets/0aac9db88cbb4f8d9db42ad20b03cb71_0.csv
#download file
geo_url <- "https://opendata.arcgis.com/datasets/0aac9db88cbb4f8d9db42ad20b03cb71_0.csv"
download.file(geo_url, 'data/0aac9db88cbb4f8d9db42ad20b03cb71_0.csv')
# load in file
region_mapping<- read.csv('data/0aac9db88cbb4f8d9db42ad20b03cb71_0.csv')
save(region_mapping, file='rda/region_mapping.rda')
head(region_mapping)
levels(as.factor(region_mapping$RGN11NM))
load('rda/region_mapping.rda')
#checking the data
levels(as.factor(region_mapping$RGN11NM))
# some have empty region name
# visual inspection shows that the unnamed regions are all Wales, and the local area ids start with "W"
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
levels(as.factor(region_lookup$region))
# update empty fields to be "wales"
# not using this as it matches the isles of scilly as well
# index <- str_length(region_mapping$region) == "0"
index <- str_which(region_lookup$local_area, "W0")
region_lookup[index,]
region_lookup$region[index] <- "wales"
levels(as.factor(region_lookup$region))
#removing the blank line
region_lookup <- region_lookup %>%
  filter(region != "")
#confirm that the regions are now correct
levels(as.factor(region_lookup$region))
# updating the names
region_lookup$region <- str_to_lower(region_lookup$region)
region_lookup$region <- str_replace_all(region_lookup$region, " and the ", "_")
region_lookup$region <- str_replace_all(region_lookup$region, " of ", "_")
region_lookup$region <- str_replace_all(region_lookup$region, " ", "_")
levels(as.factor(region_lookup$region))
#create a list of regions
region_list <- unique(region_lookup$region)
#create a table, with a column per region
region_table <- sapply(region_list, function(region){
  ifelse(str_detect(region_lookup$region, region), 1, 0)
})
head(region_table)
head(region_lookup, 20)
region_lookup <- region_lookup %>% cbind(as.data.frame(region_table))
# save completed lookup for later
save(region_lookup, file='rda/region_lookup.rda')
head(region_lookup, 50)
tail(region_lookup, 20)
head(geo_lookup)
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
print("number of local areas")
length(levels(as.factor(geo_lookup$area_code)))
#tidy up
save(geo_lookup, file='rda/geo_lookup.rda')

# load("rda/main.rda")
# load("rda/main_tidy.rda")
load("rda/geo_lookup.rda")
#add to the data set
main_new <- main %>%
  left_join(geo_lookup) %>%
  select(-local_area)
#number of local areas
print("number of local areas")
length(levels(as.factor(main_new$area_code)))
#tidy up
rm(geo_mapping, geo_url, area_update, index, i, newprefix, oldprefix, geo_lookup)
rm(tmp, wales_mapping, wales_list,tmp2)
rm(region_lookup, region_mapping, region_table, region_list)

#a quick visual check to see whether regions are relevant
#graph including regions, need the regions from earlier
load("rda/region_lookup.rda")
load("rda/geo_lookup.rda")
# use the geo lookup and region from the region_lookup
geo_lookup %>% 
  select(geo_code, local_area) %>%
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
save(main_tidy, file='rda/main_tidy.rda')
rm(main, main_new, main_tidy, geo_lookup, region_lookup, correlationmatrix, tmp)

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
rm(main, main_tidy, main_new, test_index)
load("rda/test_set.rda")
load("rda/train_set.rda")


#### data cleanse ####
# carry out the following
# * create ratio of occupied/all
# * create local area features
# * remove occupation & all_residents population numbers, geo name  & geo type
load("rda/geo_lookup.rda")
#currently working for MSOA
data_cleanse <- function(data_set_name){
  #generate geo mapping info
  load("rda/geo_lookup.rda")
  #modify to update the data
  tmp <- data_set_name %>%
    mutate(occ_ratio = occupation_all/all_residents, .after = all_residents) %>%
    select(-occupation_all, -all_residents, -geo_type, -geo_name) %>%
    left_join(geo_lookup) %>%
    select(-local_area, -geo_code)
  #return the new object
  return(tmp)
}
# names(geo_lookup)
# sum(is.na(geo_lookup))
# head(geo_lookup)

#run on the test set and train set
test_set_final <- data_cleanse(test_set)
train_set_final <- data_cleanse(train_set)
#check
names(test_set_final)
names(train_set_final)
head(test_set)
head(test_set_final)
sum(is.na(train_set_final))
test_set_final$wales
#check the number of local areas is correct
length(levels(as.factor(train_set_final$area_code)))
# tidy up
save(test_set_final, file="rda/test_set_final.rda")
save(train_set_final, file="rda/train_set_final.rda")
rm(test_set, train_set, geo_mapping, tmp, tmp1, i, newprefix, oldprefix, geo_lookup, main)

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
rmse_results %>% knitr::kable()


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
  knitr::kable()
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
categorical_features <- names(geo_lookup[4:13])
categorical_features

#highly correlated
names(train_set_final)
correlationmatrix <- cor(train_set_final[,3:69])
# image of correlation matrix
heatmap(x = correlationmatrix, col = RColorBrewer::brewer.pal(11, "Spectral"))
# find attributes that are highly corrected
highlycorrelated <- findCorrelation(correlationmatrix, cutoff=0.8, exact = TRUE, names=TRUE)
#listing the correlations for a specific one of the selected features, say, 2, and 3
#create index for the feature, and show highly correlated other features
highlycorrelated[3]
index <- str_which(names(data.frame(correlationmatrix)), highlycorrelated[4])
correlationmatrix[,index][abs(correlationmatrix[,index]) > 0.7]
highlycorrelated[2]
index <- str_which(names(data.frame(correlationmatrix)), highlycorrelated[2])
correlationmatrix[,index][abs(correlationmatrix[,index]) > 0.7]
#tidy up
save(highlycorrelated, file="rda/highlycorrelated.rda")
save(categorical_features, file="rda/categorical_features.rda")
save(low_variability_list, file="rda/low_variability_list.rda")
rm(low_variability)
rm(rmse_ave, index, train_set_lon, train_set_svm, low_variability)

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
#add 1 and add back in the y values
index2 <- append(1, (index+1))
train_smaller <- train_smaller[,index2]
#tidy up
rm(index, index2, tmp, index3, train_tmp, train_matrix, low_variability)
save(train_small, file="rda/train_small.rda")
save(train_smaller, file="rda/train_smaller.rda")
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
#add one to include the y values
index2 <- append(1, (index+1))
train_smaller_cat <- train_smaller_cat[,index2]
#tidy up
rm(index, index2, tmp, index3, train_tmp, train_matrix, low_variability, train_small_orig)
save(train_small_cat, file="rda/train_small_cat.rda")
save(train_smaller_cat, file="rda/train_smaller_cat.rda")
names(train_small_cat)
names(train_smaller_cat)

#remove categorical columns
train_final_nocat <- train_set_final %>%
  select(-all_of(categorical_features))
names(train_final_nocat)

# tidy up
save(train_final_nocat, file="rda/train_final_nocat.rda")
rm(rmse_ave, mu_hat, index, correlationmatrix, low_variability)
rm(train_small)

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
  #pivot to a long version with a row per feature/value
  rename_at(vars(ends_with("25_64")), ~str_replace_all(., "_25_64", "")) %>%
  pivot_longer(cols = !"y", 
               names_to = "feature", 
               values_to = "value") %>%
  filter(feature %in% important_features) %>% 
  ggplot(aes(y, value, col=feature))  + 
  geom_point() +
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
#checking the variable importance, mnot working
importance <- varImp(result_knn_train_smaller$train, scale=FALSE)

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
# knn does work...

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
load("rda/train_knn.rda")
train_knn <- train(y ~ ., method = "knn", data = train_set_knn_final_nocat, tuneLength=7)
# train_knnsmaller <- train(y ~ ., method = "knn", data = train_smaller, preProcess = c("center","scale"), tuneLength=7)
# 13 neighbours is best
plot(train_knn)
#making the prediction
yhat_knn <- predict(train_knn, test_set_final)
yhat_knn <- predict(train_knnsmaller, test_set_final)
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
rm(result_knn_train_final_norm, result_knn_train_set_knn_final_nocat)
rm(result_knn_train_smaller, result_knn_train_small)
rm(result_knn_train_set_knn_small)
rm(result_knn_train_final_nocat, result_knn_train_smaller_cat)
rm(train_knn, train_knnsmaller, train_set_knn_final_nocat)


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
# importance <- varImp(result_svm_train_smaller$train, scale=FALSE)

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
# using the gbmm plyr, package
if (!require('gbm')) install.packages('gbm'); library('gbm')
if (!require('plyr')) install.packages('plyr'); library('plyr')

# with the smaller training set
result_gbm_train_smaller <- 
  results_train_method(train_smaller, test_set_final, "gbm")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, 
                          tail(result_gbm_train_smaller$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance, doesn't work
# importance <- varImp(result_gbm_train_smaller$train, scale=FALSE)
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

#checking the tuning, seems ok
plot(result_gbm_train_set_final$train)

#tidy
save(result_gbm_train_set_final, file="rda/result_gbm_train_set_final.rda")
save(rmse_results, file="rda/rmse_results.rda")
rm(result_gbm_train_smaller, result_gbm_train_small)
rm(result_gbm_train_final_nocat, result_gbm_train_set_final)


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
if (!require('mgcv')) install.packages('mgcv'); library('mgcv')
if (!require('nlme')) install.packages('nlme'); library('nlme')

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
result_gam_train_small <- 
  results_train_method(train_small, test_set_final, "gam")
# did not complete, took too long

#tidy
save(rmse_results, file="rda/rmse_results.rda")
rm(result_gam_train_smaller)


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
# importance <- varImp(result_lasso_train_smaller$train, scale=FALSE)
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

# does it work with categortical?
train_lasso <- train(y ~ ., method = "lasso", data = train_smaller_cat)
#yes it does

#compare glm and lasso predictions
result_lasso_train_set_final <- 
  results_train_method(train_set_final, test_set_final, "lasso")
rmse_results <- bind_rows(rmse_results,
                          tail(result_lasso_train_set_final$results, 1))
result_lasso_train_set_final$results$rmse
load("rda/result_lasso_train_set_final.rda")
load("rda/result_glm_train_set_final.rda")
result_glm_train_set_final$results$rmse
# check whether the RMSE are identical, or almost
identical(result_lasso_train_set_final$results$rmse,
          result_glm_train_set_final$results$rmse)
all.equal(result_lasso_train_set_final$results$rmse,
          result_glm_train_set_final$results$rmse)
# check whether the RMSE are identical, or almost
identical(result_lasso_train_set_final$y_hat,
          result_glm_train_set_final$y_hat)
all.equal(result_lasso_train_set_final$y_hat,
          result_glm_train_set_final$y_hat)


# comparing gam and gamloess
# plotting graphs
data.frame(result_lasso_train_set_final$y_hat,
           result_glm_train_set_final$y_hat) %>%
  ggplot(aes(result_lasso_train_set_final$y_hat, 
             result_glm_train_set_final$y_hat)) + 
  geom_point() +
  ggtitle("Comparing the GLM and LASSO algorithms") +
  xlab("GLM predictions") +
  ylab("LASSO predictions")


#tidy
save(result_lasso_train_set_final, file="rda/result_lasso_train_set_final.rda")
rm(train_lasso, result_lasso_train_smaller, result_lasso_train_small) 
rm(result_lasso_train_set_final, result_lasso_train_final_nocat)
rm(train_lasso)
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
# importance <- varImp(result_pcr_train_smaller$train, scale=FALSE)
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
rm(result_pcr_train_top28, result_pcr_train_final_nocat)
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
# importance <- varImp(result_bayesglm_train_smaller$train, scale=FALSE)
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
# check whether the RMSE are identical, or almost
identical(result_bayesglm_train_set_final$y_hat,
          result_glm_train_set_final$y_hat)
all.equal(result_bayesglm_train_set_final$y_hat,
          result_glm_train_set_final$y_hat)
y_hat_bayesglm <- result_bayesglm_train_set_final$y_hat

#tidy
rm(y_hat_bayesglm, result_bayesglm_train_smaller)
rm(result_bayesglm_train_set_final, result_bayesglm_train_small)
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
result_gamloess_train_small <- 
  results_train_method(train_small, test_set_final, "gamLoess")
# did not complete

#tidy
save(rmse_results, file="rda/rmse_results.rda")
rm(result_gamloess_train_smaller)


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
load("rda/result_ranger_train_set_final.rda")
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

# with the full training set, no categorical features
result_glmboost_train_final_nocat <- 
  results_train_method(train_final_nocat, test_set_final, "glmboost")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results,
                          tail(result_glmboost_train_final_nocat$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance
importance <- varImp(result_glmboost_train_final_nocat$train, scale=FALSE)
plot(importance, 20)

# with the full training set
result_glmboost_train_set_final <- 
  results_train_method(train_set_final, test_set_final, "glmboost")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results,
                          tail(result_glmboost_train_set_final$results, 1))
rmse_results %>% knitr::kable()
#checking the variable importance
importance <- varImp(result_glmboost_train_set_final$train, scale=FALSE)
plot(importance, 20)

#checking tuning
plot(result_glmboost_train_set_final$train)

#tidy
importance_glmboost <- importance 
save(result_glmboost_train_set_final, file="rda/result_glmboost_train_set_final.rda")
save(importance_glmboost, file="rda/importance_glmboost.rda")
save(rmse_results, file="rda/rmse_results.rda")
# load("rda/result_glmboost_train_set_final.rda")
rm(importance)
rm(result_glmboost_train_smaller, result_glmboost_train_small) 
rm(result_glmboost_train_smaller_cat, result_glmboost_train_final_nocat) 
rm(result_glmboost_train_set_final)


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

# with the full train set, less categorical
result_avnnet_train_final_nocat <- 
  results_train_method(train_final_nocat, test_set_final, "avNNet")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results,
                          tail(result_avnnet_train_final_nocat$results, 1))
rmse_results %>% knitr::kable()

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

#tidy 
save(rmse_results, file="rda/rmse_results.rda")
save(result_avnnet_train_set_final, file="rda/result_avnnet_train_set_final.rda")
load("rda/result_avnnet_train_set_final.rda")
rm(result_avnnet_train_small, result_avnnet_train_smaller) 
rm(result_avnnet_train_final_nocat, result_avnnet_train_set_final)


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

# with the full train set, less categorical
result_svm2_train_final_nocat <- 
  results_train_method(train_final_nocat, test_set_final, "svmLinear2")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results,
                          tail(result_svm2_train_final_nocat$results, 1))
rmse_results %>% knitr::kable()

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
load("rda/result_svm2_train_set_final.rda")
load("rda/result_svm_train_set_final.rda")
result_svm2_train_set_final$results$rmse
result_svm_train_set_final$results$rmse
y_hat_svm2 <- result_svm2_train_set_final$y_hat
y_hat_svm <- result_svm_train_set_final$y_hat

# check whether the RMSE are identical, or almost
identical(result_svm2_train_set_final$results$rmse,
          result_svm_train_set_final$results$rmse)
all.equal(result_svm2_train_set_final$results$rmse,
          result_svm_train_set_final$results$rmse)
# check whether the RMSE are identical, or almost
identical(result_svm2_train_set_final$y_hat,
          result_svm_train_set_final$y_hat)
all.equal(result_svm2_train_set_final$y_hat,
          result_svm_train_set_final$y_hat)

#tidy 
save(result_svm2_train_set_final, file="rda/result_svm2_train_set_final.rda")
save(rmse_results, file="rda/rmse_results.rda")
rm(result_svm2_train_smaller, result_svm2_train_small)
rm(result_svm2_train_final_nocat, result_svm2_train_set_final)
rm(y_hat_svm, y_hat_svm2)


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

# with the full train set, less categorical
result_svmradial_train_final_nocat <- 
  results_train_method(train_final_nocat, test_set_final, "svmRadial")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results,
                          tail(result_svmradial_train_final_nocat$results, 1))
rmse_results %>% knitr::kable()

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
rm(result_svmradial_train_final_nocat)
rm(result_svmradial_train_set_final)

##### ensemble   #####
# Identify the leading models, the top 5
rmse_results %>%
  arrange(rmse) %>% 
  # filter(str_detect(method, "train_set_final") ) %>%
  filter(str_detect(method, "glm") ) %>%
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
  data.frame(number = 1:length(result_glm_train_set_final$y_hat)) %>%
  cbind(data.frame(result_glm_train_set_final$y_hat)) %>%
  cbind(data.frame(result_gbm_train_set_final$y_hat)) %>%
  cbind(data.frame(result_svm_train_set_final$y_hat)) %>%
  cbind(data.frame(result_svmradial_train_set_final$y_hat)) %>%
  cbind(data.frame(result_ranger_train_set_final$y_hat)) %>%
  select(-number) %>%
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
rm(result_avnnet_train_set_final)

#### ensemble 2 ####
#a smaller ensemble of the three best models
y_hat_ens_table2 <- 
  data.frame(number = 1:length(result_glm_train_set_final$y_hat)) %>%
  cbind(data.frame(result_glm_train_set_final$y_hat)) %>%
  cbind(data.frame(result_svm_train_set_final$y_hat)) %>%
  cbind(data.frame(result_svmradial_train_set_final$y_hat)) %>%
  select(-number) %>%
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
rm(result_avnnet_train_set_final, result_gbm_train_set_final)


#### feature prioritisation ####
# other promising models, looks like GAM, GAMloess and KNN could be interesting
rmse_results %>%
  filter(str_detect(method, "smaller") ) %>%
  arrange(rmse) %>% head(15) %>% knitr::kable()

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
  select(-Overall)
head(gbm_rank)
#create ranking for glm algorithm, with normalised score
glm_rank <- importance_glm$importance %>%
  rownames_to_column(var = "feature") %>%
  arrange(desc(Overall)) %>% 
  cbind(data.frame(glm_rank = 1:length(importance_glm$importance$Overall))) %>%
  mutate("glm_overall" = Overall/max(Overall)) %>% 
  select(-Overall)
head(glm_rank)
#create ranking for glmboost algorithm, with normalised score
glmboost_rank <- importance_glmboost$importance %>%
  rownames_to_column(var = "feature") %>%
  arrange(desc(Overall)) %>% 
  cbind(data.frame(glmboost_rank = 1:length(importance_glmboost$importance$Overall))) %>%
  mutate("glmboost_overall" = Overall/max(Overall)) %>% 
  select(-Overall)

# combined ranking table, with mean, and reordered
feature_rank <- glm_rank %>%
  left_join(gbm_rank) %>%
  left_join(glmboost_rank) %>%
  mutate(mean_overall = (glm_overall+gbm_overall+glmboost_overall)/3, .after = feature) %>%
  mutate(mean_rank = (glm_rank+gbm_rank+glmboost_rank)/3, .after = feature) 
# arrange by mean ranking and display top 15
feature_rank %>% select(feature, mean_rank, mean_overall) %>%
  arrange(mean_rank) %>% head(15) %>% knitr::kable()
# arrange by mean score and display top 15
feature_rank %>% select(feature, mean_rank, mean_overall) %>%
  arrange(desc(mean_overall)) %>% head(15) %>% knitr::kable()

# plotting the top 9
feature_top9 <- feature_rank %>% 
  select(feature, mean_rank, mean_overall) %>%
  arrange(desc(mean_overall)) %>% head(9) %>%
  pull(feature)
#create the plot
train_set_final %>%  
  #pivot to a long version with a row per feature/value
  pivot_longer(cols = !"y", 
               names_to = "feature", 
               values_to = "value") %>%
  filter(feature %in% feature_top9) %>% 
  #tidy the name to make more readable
  rename_at(vars(ends_with("25_64")), ~str_replace_all(., "_25_64", "")) %>%
  ggplot(aes(y, value, col=feature))  + 
  geom_point() +
  facet_wrap(. ~feature,  scales = "free_y") +
  ggtitle("Outcome y for important features")
xlab("y, proportion of managerial & professional")
ylab("feature values")

#looking at correlation with y
data.frame(cor(train_set_final)) %>%
  select(y) %>%
  rownames_to_column(var="feature") %>%
  filter(feature %in% feature_top9) %>% 
  arrange(desc(abs(y))) %>%
  knitr::kable()

# looking at the rmse with numbers of features
#creating a function to choose the top n features, based on overall score
topfeature_overall <- function(topn, traindata){
  feature_n <- feature_rank %>% 
    select(feature, mean_rank, mean_overall) %>%
    arrange(desc(mean_overall)) %>% head(topn) %>%
    pull(feature)
  train_topn <- traindata %>%
    select(y, all_of(feature_n))
  return(train_topn)
}
#creating a function to choose the top n features, based on rank
topfeature_rank <- function(topn, traindata){
  feature_n <- feature_rank %>% 
    select(feature, mean_rank, mean_overall) %>%
    arrange(mean_rank) %>% head(topn) %>%
    pull(feature)
  train_topn <- traindata %>%
    select(y, all_of(feature_n))
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
train_top30 <- topfeature_overall(30, train_set_final)

# tidy up
save(feature_rank, file="rda/feature_rank.rda")
save(train_top15, file="rda/train_top15.rda")
save(train_top10, file="rda/train_top10.rda")
rm(gbm_rank, glm_rank, glmboost_rank, glmboost_rank2, tmp, tmp2, feature_top9)
rm(importance, importance_gbm, importance_glm, importance_glmboost)

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

save(glm_top10_top50, file="rda/glm_top10_top50.rda")
save(svmradial_top20_top40, file="rda/svmradial_top20_top40.rda")
save(svm_top20_top40, file="rda/svm_top20_top40.rda")
rm(svm_top20_top40, svm_top20_top40_tmp)
rm(svmradial_top20_top40, svmradial_top20_top40_tmp)
rm(glm_top10_top50, glm_top10_top50_tmp)
load("rda/svmradial_top20_top40.rda")

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
rmse_results <- bind_rows(rmse_results, tail(result_gam_train_top28$results, 1))
rmse_results %>% knitr::kable()
# tidy
save(result_gam_train_top28, file="rda/result_gam_train_top28.rda")
load("rda/result_gam_train_top28.rda")
rm(result_gam_train_top28)

# running on GAMloess with 28 features
# with the train_top28 training set
result_gamloess_train_top28 <- 
  results_train_method(train_top28, test_set_final, "gamLoess")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, tail(result_gamloess_train_top28$results, 1))
rmse_results %>% knitr::kable()
# tidy
save(result_gamLoess_train_top28, file="rda/result_gamLoess_train_top28.rda")
rm(result_gamLoess_train_top28)

#checking the overall performance
rmse_results %>% arrange(rmse) %>% head(10) %>%   knitr::kable() 

# comparing gam and gamloess
y_hat_gam <- result_gam_train_top28$y_hat
y_hat_gamLoess <- result_gamLoess_train_top28$y_hat
data.frame(y_hat_gam, y_hat_gamLoess) %>%
  ggplot(aes(y_hat_gam, y_hat_gamLoess)) + 
  geom_point() +
  ggtitle("Comparing GAM vs GAM Loess predictions") +
  xlab("GAM model predictions") +
  ylab("GAM Loess model predictions")
data.frame(y_hat_gam, y_hat_gamLoess) %>%
  mutate(delta = (y_hat_gam-y_hat_gamLoess)) %>%
  ggplot(aes(delta)) + 
  geom_histogram(bins = 30) +
  ggtitle("Comparing GAM vs GAM Loess predictions") +
  xlab("Delta between GAM and GAM Loess predictions") +
  ylab("number of predictions in each interval")

# running on knn with 28 features, less age and area
train_top28_knn <- train_top28 %>%
  select(-age_median, -area_code)
# with the train_top28 training set
result_knn_train_top28_knn <- 
  results_train_method(train_top28_knn, test_set_final, "knn")
# extract the rmse from the results
rmse_results <- bind_rows(rmse_results, tail(result_knn_train_top28_knn$results, 1))
rmse_results %>% knitr::kable()


#### ensemble 3 ####
# updating the ensemble with the new models
# Identify the leading models, the new top 5
# load("rda/rmse_results.rda")
rmse_results %>%
  arrange(rmse) %>% head(20) %>% knitr::kable()

#loading the separate model information
load("rda/result_glm_train_set_final.rda")
load("rda/result_svm_train_set_final.rda")
load("rda/result_svmradial_train_set_final.rda")
load("rda/result_gam_train_top28.rda")
load("rda/result_gamLoess_train_top28.rda")
y_hat_glm <- result_glm_train_set_final$y_hat

# an ensemble of the best five models
y_hat_ens_table3 <- 
  data.frame(number = 1:length(result_glm_train_set_final$y_hat)) %>%
  cbind(data.frame(result_glm_train_set_final$y_hat)) %>%
  cbind(data.frame(result_svm_train_set_final$y_hat)) %>%
  cbind(data.frame(result_svmradial_train_set_final$y_hat)) %>%
  cbind(data.frame(result_gam_train_top28$y_hat)) %>%
  cbind(data.frame(result_gamLoess_train_top28$y_hat)) %>%
  select(-number) %>%
  #calculate average of the y_hats
  mutate(y_hat_ave = rowMeans(.))
head(y_hat_ens_table3)
#test against the actual values
rmse_ens <- rmse(test_set_final$y, y_hat_ens_table3$y_hat_ave)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Ensemble3 glm, svm, svmRadial, gam, gamLoess", rmse = rmse_ens))
rmse_results %>% knitr::kable()

#### ensemble 4 ####
# an ensemble of the best three models
y_hat_ens_table4 <- 
  data.frame(number = 1:length(result_glm_train_set_final$y_hat)) %>%
  cbind(data.frame(result_glm_train_set_final$y_hat)) %>%
  cbind(data.frame(result_svmradial_train_set_final$y_hat)) %>%
  cbind(data.frame(result_gam_train_top28$y_hat)) %>%
  select(-number) %>%
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
rm(result_gamLoess_train_top28, result_svm_train_set_final)


#### Investigation and visualisation of errors ####
# looking at the best ensemble model, and constituent ones svmRadial and gam
# plot of y against y_hat for all three
y_hat_ens_table5 %>%
  cbind(y = test_set_final$y) %>%
  rename(ensemble_y_hat=y_hat_ave) %>%
  pivot_longer(cols=contains("y_hat"), names_to="model", values_to="y_hat") %>%
  ggplot(aes(x=y_hat, y=y, col=model)) +
  geom_point() +  
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
  select(-svmradial_y_hat, -gam_y_hat, -y_hat_ave) %>%
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
  select(-y_hat_ave) %>%
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


#### running on the validation / main set ####

# writing the code for the overall model to run

#run on the test set and train set
# load the files
load("rda/geo_lookup.rda")
load("rda/main.rda")
load("rda/validation.rda")
#cleanse the data, add the geo info
main_clean <- data_cleanse(main)
validation_clean <- data_cleanse(validation)
#check
names(main_clean)
names(validation_clean)
head(main_clean)
head(validation_clean)
sum(is.na(main_clean))
# tidy up
save(main_clean, file="rda/main_clean.rda")
save(validation_clean, file="rda/validation_clean.rda")
rm(geo_lookup, tmp, tmp1)
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
y_hat_ens_main <- 
  data.frame(svmradial_y_hat = result_svmradial_main$y_hat, 
             gam_y_hat = result_gam_main_top28$y_hat) %>%
  #calculate average of the y_hats
  mutate(y_hat_ave = rowMeans(.))
#test against the actual values
rmse_ens <- rmse(validation$y, y_hat_ens_main$y_hat_ave)
rmse_results_validation <- bind_rows(rmse_results_validation,
                          tibble(method="Ensemble svmRadial, gam28",  
                                 rmse = rmse_ens))
rmse_results_validation %>% knitr::kable()

# comparing the svmradial and gam models
y_hat_ens_main %>%
  cbind(y = validation$y) %>%
  select(-y_hat_ave) %>%
  pivot_longer(cols=contains("y_hat"), names_to="model", values_to="y_hat") %>%
  ggplot(aes(x=y_hat, y=y, col=model)) +
  geom_point()  +
  geom_abline(slope = 1, intercept = 0, col="black") +
  ggtitle("Comparing gam and svm radial predictions vs the outcome y") +
  xlab("model predictions") +
  ylab("actual values, y") 


# tidy up
save(rmse_results_validation, file="rda/rmse_results_validation.rda")
save(y_hat_ens_main, file="rda/y_hat_ens_main.rda")
load("rda/y_hat_ens_main.rda")
rm(rmse_ens, y_hat_ens_main)
rm(result_gam_main_top28)
rm(result_svmradial_main)
rm(result_glm_main)

#### running on the SOA  #####




#### feature importance ####




#### running without the qualification features ####




#### working code ####


# show the p value in addition to correlation
# https://www.displayr.com/how-to-create-a-correlation-matrix-in-r/
install.packages("Hmisc")
library("Hmisc")
# Use the following code to run the correlation matrix with p-values. Note that the data has to be fed to the rcorr function as a matrix.
mydata.rcorr = rcorr(as.matrix(mydata))
mydata.rcorr
# This generates one table of correlation coefficients (the correlation matrix) and another table of the p-values. 
mydata.coeff = mydata.rcorr$r
mydata.p = mydata.rcorr$P


