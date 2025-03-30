# libraries ---------------------------------------------------------------
library(openxlsx)
library(janitor)
library(dplyr)

library(tidyverse)

library(FactoMineR)
library(adklakedata)
library(factoextra)

# Data processing ---------------------------------------------------------

#collecting the metadata
censor.meta <- read.xlsx('Censor Code Metadata.xlsx')%>%
  clean_names()

variable.meta <- read.xlsx('Variable Metadata.xlsx')%>%
  clean_names()

site.meta <- read.xlsx('Site Metadata.xlsx')%>%
  clean_names()

ice.cover.meta<- read.xlsx('LG Ice Cover Metadata.xlsx')%>%
  clean_names()

exo.meta <- read.xlsx('Exo Profile Metadata.xlsx')%>%
  clean_names()

#collecting actual data
monthly.lake.level <- read.csv('monthly_lk_lvl_1980-2023_edit.csv')%>%
  clean_names()

ice.cover <-read.csv('lg_ice_cover.csv')%>%
  clean_names()

exo <-read.csv('exofull.csv')%>%
  clean_names()

atm.chem <-read.csv('atm_chem_v4.csv')%>%
  clean_names()

glimpse(monthly.lake.level)
#date needs to be converted
monthly.lake.level <- monthly.lake.level%>%
  mutate(date = mdy(date))

glimpse(ice.cover)
#year, date_in, date_out need to be converted
ice.cover <- ice.cover%>%
  mutate(date_in = mdy(date_in),
         date_out = mdy(date_out))

glimpse(exo)
#time and date need to be converted
exo <- exo%>%
  mutate(date = mdy(date),
         time = hms(time))

glimpse(atm.chem)
#collection_datetime_utc needs to be updated
atm.chem <- atm.chem %>%
  mutate(collection_datetime_utc = mdy_hm(collection_datetime_utc))

filenames = c("atm" = "data/atm_chem.RData",
              "monthly" = "data/monthly_lake_level.RData",
              "exo" = "data/exo.RData",
              "ice" = "data/ice_cover.RData")

is.convertible.to.date <- function(mydate) {
  tryCatch(!is.na(as.Date(mydate, tryFormats = c("%Y-%m-%d", "%Y/%m/%d","%d-%m-%Y","%m-%d-%Y"))),
           error = function(err) {FALSE})
}

get <- function(...){
  l <- list(...)
  files <- list()
  dates <- list()
  for (param in l){
    print(param)
    print(is.convertible.to.date(param))
    if(is.convertible.to.date(param)) {
        date <- as.Date(param, tryFormats = c("%Y-%m-%d", "%Y/%m/%d","%d-%m-%Y","%m-%d-%Y"))
        dates <- c(dates, date)

      }else {
        param = tolower(param)

        if (param %in% names(filenames)){
          file = match.arg(param, names(filenames))
          files <-c(files, file)

        }else{
          print("no such file")
        }
      }
  }
  print("files")
  for (file in files){
    print(file)
  }
  print("dates")
  for (date in dates){
    print(dates)
  }
}


#applying metadata variables to actual sheet (no longer doing this) --------------
#splitting up variable sheet
variable.meta <- variable.meta%>%
  clean_names()%>%
  separate_rows(code, variable, sep = "/") %>%
  mutate(variable = case_when(
    code == "TFP" ~ "Total Filterable Phosphorus",
    code == "TSP" ~ "Total Soluble Phosphorus",
    code == "TDP" ~ "Total Dissolved Phosphorus",
    code == "OP" ~ "Orthophosphate",
    code == "MRP" ~ "Molybdate",
    TRUE ~ variable
  ))%>%
  glimpse()


#applying metadata to exo
code.description <- site.meta%>%
  select(site_code, site_description)

exo <- merge(x=exo,
             y=code.description,
             by.x = "site_code",
             by.y = "site_code",
             all.x = TRUE)%>%
  mutate(site_name = site_description)%>%
  select(-site_code, -site_description)%>%
  glimpse()

#Delete the first 5 rows in exo.meta
exo.meta <- exo.meta[-c(1:5), ]
# Remove leading and trailing spaces from the description column
exo.meta$description <- trimws(exo.meta$description)
# Replace spaces in the description with underscores
exo.meta$description <- gsub(" ", "_", exo.meta$description)

# Merge the description and unit columns, but only add the unit if it is not "n/a"
exo.meta$description_unit <- ifelse(is.na(exo.meta$unit),
                                    exo.meta$description,
                                    paste(exo.meta$description, exo.meta$unit, sep = "_"))

#Rename the columns in exo using the description from exo.meta
#Create a named vector where names are the codes and values are the descriptions
rename_vector <- setNames(exo.meta$description_unit, exo.meta$code)

# Rename the columns in exo
colnames(exo) <- ifelse(colnames(exo) %in% exo.meta$code, rename_vector[colnames(exo)], colnames(exo))



#applying meta data to atm
variable.meta <- variable.meta %>%
  select(-unit)

atm.chem <-merge(x=atm.chem,
                    y=variable.meta,
                    by.x = "variable",
                    by.y = "code",
                    all.x = TRUE)%>%
  mutate(variable = ifelse(is.na(variable.y), variable, variable.y))%>%
  select(-variable.y)%>%
  glimpse()

atm.chem <-merge(x=atm.chem,
                 y=censor.meta,
                 by.x = "censor_code",
                 by.y = "censor_code",
                 all.x = TRUE)%>%
  mutate(censor_output = meaning)%>%
  select(-censor_code, -meaning)%>%
  glimpse()

atm.chem <- atm.chem %>%
  mutate(censor_output = ifelse(is.na(censor_output), "No Output", censor_output))

#applying metadata to ice cover
ice.cover <-merge(x=ice.cover,
                 y=ice.cover.meta,
                 by.x = "year",
                 by.y = "year",
                 all.x = TRUE)%>%
  glimpse()

save(atm.chem, file="data/atm_chem.RData")
save(exo, file="data/exo.RData")
save(ice.cover, file="data/ice_cover.RData")
save(monthly.lake.level, file="data/monthly_lake_level.RData")


