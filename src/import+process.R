# import + processing of data
# and process

source("src/packages.R")

# columns named according to DST. 
# use fill from tidyr to fill enhed column. 
# --> fills values top-down until it encounters next valid value

df <- read_xlsx("data/all_data.xlsx", skip = 2) %>% 
  rename(enhed=`...1`,varegruppe=`...2`) %>% 
  fill(enhed, .direction="down")

# convert all except first to columns to numeric 

df[, 3:ncol(df)] <- df[, 3:ncol(df)] %>% 
  mutate_if(is.character, as.numeric)
  
# check that was before ".." is now NA (i.e., no inconsistencies in formatting)

temp <- read_xlsx("data/all_data.xlsx", skip = 2) %>% 
  rename(enhed=`...1`,varegruppe=`...2`) %>% 
  fill(enhed, .direction="down")

sum(temp == "..") == sum(is.na(df))

##### Note: Jeg har de enkelte rækker, hvor der er NA hele vejen hen, udover enhed og varegruppe
