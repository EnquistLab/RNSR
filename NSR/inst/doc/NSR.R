## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  
#  library(devtools)
#  install_github("EnquistLab/RNSR/NSR")
#  
#  

## ------------------------------------------------------------------------
library(NSR)
NSR_super_simple(species = "Acer rubrum",
                 country =  "Canada", 
                 state_province = "Ontario")



## ------------------------------------------------------------------------

#First, we'll grab some occurrence records from BIEN
library(BIEN)

pima <- BIEN_occurrence_county(country = "United States",state = "Arizona",county = "Pima", all.taxonomy = T,natives.only = F,native.status = T,cultivated = T,political.boundaries = T, limit = 100)

#The NSR function expects 6 columns as input, however only the fields "species" and "country" need to be populated.
#If you ever forget, you can use the function NSR_template as a quick lookup, or to populate.

pima_query<-NSR_template(nrow = nrow(pima))

pima_query$family <- pima$scrubbed_family
pima_query$species <- pima$scrubbed_species_binomial
pima_query$country <- pima$country
pima_query$state_province <- pima$state_province
pima_query$county_parish <- pima$county

pima_nsr_results <- NSR(pima_query)

head(pima_nsr_results)


