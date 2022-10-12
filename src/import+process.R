# import + processing of data
# and process

library(readxl)
library(tidyverse)

# columns named according to DST. 
# use fill from tidyr to fill enhed column. 
# --> fills values top-down until it encounters next valid value

df <- read_xlsx("../all_data.xlsx", skip = 2) %>% 
  rename(enhed=`...1`,varegruppe=`...2`) %>% 
  fill(enhed, .direction="down")

# convert all except first to columns to numeric 

df[, 3:ncol(df)] <- df[, 3:ncol(df)] %>% 
  mutate_if(is.character, as.numeric)
  
# check that was before ".." is now NA (i.e., no inconsistencies in formatting)

temp <- read_xlsx("../all_data.xlsx", skip = 2) %>% 
  rename(enhed=`...1`,varegruppe=`...2`) %>% 
  fill(enhed, .direction="down")

sum(temp == "..") == sum(is.na(df))

##### Note: Jeg har ikke fjernet NA rækker, men det burde man nok 

# have to convert month variables to "month" variable in order to plot data
# big data frame

df <- df %>% pivot_longer(cols=`2020M01`:`2022M08`,names_to = "maaned",values_to="vaerdi")

# instead of having three rows for one group with values (indeks, ...), 
# have one column per group with respective values
# split code and description afterwards
# rename columns

df <- df %>% 
  group_by(enhed) %>%
  pivot_wider(names_from = enhed, values_from = vaerdi) %>% 
  separate(varegruppe, into=c("kode","beskrivelse"),sep="\\s",extra="merge") %>% 
  rename(vaerdi_i = Indeks, vaerdi_ae_m = `Ændring i forhold til måneden før (pct.)`, vaerdi_ae_aa = `Ændring i forhold til samme måned året før (pct.)`)

# create code levels

df <- df %>% 
  mutate(niv_1 = ifelse(str_count(kode,"\\.")==0,kode,NA), 
         niv_2=ifelse(str_count(kode,"\\.")==1,kode,NA), 
         niv_3=ifelse(str_count(kode,"\\.")==2,kode,NA),
         niv_4=ifelse(str_count(kode,"\\.")==3,kode,NA))
