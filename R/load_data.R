

# function to load data #

loadtabs <- function(){

  dbs <- qs::qread(file = "data/energydash_data.qs")

  return(dbs)
}

