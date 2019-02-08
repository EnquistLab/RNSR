## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------

library(devtools)
install_github("EnquistLab/RNSR/NSR")



## ------------------------------------------------------------------------
library(NSR)
NSR_super_simple(species = "Acer rubrum",
                 country =  "Canada", 
                 state_province = "Ontario")



