# import + processing of data
# and process

library(readxl)
library(tidyverse)

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
  rename(vaerdi_i = Indeks, vaerdi_ae_m = `Ændring i forhold til måneden før (pct.)`, vaerdi_ae_aa = `Ændring i forhold til samme måned året før (pct.)`)

# create code levels

df <- df %>%
  mutate(niv_1 = ifelse(str_count(kode,"\\.")==0 | str_count(kode,"\\.")==1 & nchar(kode)==3,kode,NA),
         niv_2=ifelse(str_count(kode,"\\.")==1 & nchar(kode) > 3,kode,NA),
         niv_3=ifelse(str_count(kode,"\\.")==2,kode,NA),
         niv_4=ifelse(str_count(kode,"\\.")==3,kode,NA))

# making sure every category is on same form
df <- df %>%
  mutate(niv_1 = ifelse(niv_1 == "00", "00.", niv_1),
         niv_1 = ifelse(niv_1 == "01", "01.", niv_1),
         niv_4 = ifelse(str_count(niv_4)==8, niv_4, substr(niv_4, star=1, stop = 8)))



df <- df %>%
  mutate(niv_3 = ifelse(is.na(niv_3), substr(niv_4, start=1, stop = 6), niv_3),
         niv_2 = ifelse(is.na(niv_2), substr(niv_3, start=1, stop = 4), niv_2),
         niv_1 = ifelse(is.na(niv_1), substr(niv_2, start=1, stop = 3), niv_1))

df <- df %>%
  mutate(beskrivelse_1 = ifelse(!is.na(niv_1) & is.na(niv_2) & is.na(niv_3) & is.na(niv_4),beskrivelse,NA),
         beskrivelse_2 = ifelse(!is.na(niv_1) & !is.na(niv_2) & is.na(niv_3) & is.na(niv_4),beskrivelse,NA),
         beskrivelse_3 = ifelse(!is.na(niv_1) & !is.na(niv_2) & !is.na(niv_3) & is.na(niv_4),beskrivelse,NA),
         beskrivelse_4 = ifelse(!is.na(niv_1) & !is.na(niv_2) & !is.na(niv_3) & !is.na(niv_4),beskrivelse,NA))

df <- df %>% 
  fill(beskrivelse_1) %>% 
  
  group_by(beskrivelse_1) %>% 
  fill(beskrivelse_2) %>% 
  ungroup() %>% 
  
  group_by(beskrivelse_1,beskrivelse_2) %>% 
  fill(beskrivelse_3) %>% 
  ungroup() %>% 
  
  group_by(beskrivelse_1,beskrivelse_2,beskrivelse_3) %>% 
  fill(beskrivelse_4) %>% 
  ungroup()

# Filling the description of level 3, with level 4 if empty
df <- df %>%
  mutate(beskrivelse_3 = ifelse(is.na(beskrivelse_3), beskrivelse_4, beskrivelse_3)) %>%
  mutate(beskrivelse_4 = ifelse(beskrivelse_3==beskrivelse_4, NA, beskrivelse_4)) %>%
  mutate(beskrivelse_2 = ifelse(is.na(beskrivelse_2), beskrivelse_3, beskrivelse_2)) %>%
  mutate(beskrivelse_3 = ifelse(beskrivelse_2==beskrivelse_3, NA, beskrivelse_3)) %>%
  mutate(beskrivelse_1 = ifelse(is.na(beskrivelse_1), beskrivelse_2, beskrivelse_1)) %>%
  mutate(beskrivelse_2 = ifelse(beskrivelse_1==beskrivelse_2, NA, beskrivelse_2))

# Adding column with date converted to type = Date
df <- df %>%
  mutate(Dato = str_replace(maaned, "M", "-")) %>%
  mutate(Dato = paste0(as.character(Dato), "-01")) %>% 
  mutate(Dato = as.Date(Dato, format = "%Y-%m-%d"))


#df <- df %>%
 # mutate(vaerdi_ae_m = ifelse(vaerdi_ae_m < 0, (-1)*exp(vaerdi_ae_m), exp(vaerdi_ae_m)))
  
  

# Viser problemet med værdier der er 0.0 i højere niveau da de ikke nødvendigvis er det i lavere niveauer


#df_try <- df[df$maaned == "2021M09" & df$niv_1 == "01.", ] 

#df_try <- df_try %>%
 # mutate(vaerdi_ae_m_exp = ifelse(vaerdi_ae_m < 0, (-1)*exp(vaerdi_ae_m), exp(vaerdi_ae_m)))
