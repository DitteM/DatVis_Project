# process data

df <- read_xlsx("../data/all_years.xlsx")

##### NOT FIRST TWO COLUMNS AS NUMERIC

df <- df %>% mutate_if(is.character, as.numeric)
