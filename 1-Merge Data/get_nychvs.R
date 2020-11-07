get_nychvs <- function(year=2017, nrows=-1, col_keys = F){
  file <- paste0("Datasets//NYCHVS ",year," Occupied File for ASA Challenge_CSV.csv")
  header <- read.csv(file, nrows = 1)
  if (!col_keys){
    temp_data <- read.csv(file, skip=1, stringsAsFactors = F, nrows=nrows)
    colnames(temp_data) <- colnames(header)
  }
  if(!col_keys)
    return(temp_data)
  return(header)
}


## col_keys = T returns the column headers and descriptions.
## Taking the transpose gives a nice key for reference.
#
## Try
#
# library(dplyr)
# get_nychvs(year = 1991, col_keys = T) %>% select(X_28c, X_28d2, X_30a) %>% t()


