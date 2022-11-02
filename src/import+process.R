# import + processing of data
# and process

library(readxl)
library(tidyverse)

# columns named according to DST. 
# use fill from tidyr to fill enhed column. 
# --> fills values top-down until it encounters next valid value

df <- read_xlsx("../data/all_data.xlsx", skip = 2) %>% 
  rename(enhed=`...1`,varegruppe=`...2`) %>% 
  fill(enhed, .direction="down")

# convert all except first to columns to numeric 

df[, 3:ncol(df)] <- df[, 3:ncol(df)] %>% 
  mutate_if(is.character, as.numeric)
  
# check that was before ".." is now NA (i.e., no inconsistencies in formatting)

temp <- read_xlsx("../data/all_data.xlsx", skip = 2) %>% 
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
  rename(vaerdi_i = Indeks, vaerdi_ae_m = `?ndring i forhold til m?neden f?r (pct.)`, vaerdi_ae_aa = `?ndring i forhold til samme m?ned ?ret f?r (pct.)`)

# create code levels

df <- df %>%
  mutate(niv_1 = ifelse(str_count(kode,"\\.")==0 | str_count(kode,"\\.")==1 & nchar(kode)==3,kode,NA),
         niv_2=ifelse(str_count(kode,"\\.")==1 & nchar(kode) > 3,kode,NA),
         niv_3=ifelse(str_count(kode,"\\.")==2,kode,NA),
         niv_4=ifelse(str_count(kode,"\\.")==3,kode,NA))

# Adding column with date converted to type = Date
df <- df %>%
  mutate(Dato = str_replace(maaned, "M", "-")) %>%
  mutate(Dato = paste0(as.character(Dato), "-01")) %>% 
  mutate(Dato = as.Date(Dato, format = "%Y-%m-%d"))
