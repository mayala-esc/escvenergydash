

#### READ VEC DATA ####


##### PREAMBLE #####

library(readxl)
library(lubridate)
library(stringr)
library(tidyverse)
library(qs)
library(usethis)

# function to load data #

loadtabs <- function(){
  
  dbs <- qs::qread(file = "data/energydash_data.qs")
  
  return(dbs)
}

