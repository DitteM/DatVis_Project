# process data

df <- read_xlsx("data/all_years.xlsx")

##### NOT FIRST TWO COLUMNS AS NUMERIC

df[, 3:ncol(df)] <- df[, 3:ncol(df)] %>% 
            mutate_if(is.character, as.numeric)


# Creating dataframes for each variable type

index_df <- df[df$...1 == "Indeks", 2:ncol(df)]
  
Com_1m_df <- df[df$...1 == "Ændring i forhold til måneden før (pct.)", 2:ncol(df)]
  
Com_12m_df <- df[df$...1 == "Ændring i forhold til samme måned året før (pct.)", 2:ncol(df)]



# Removing rows containing only NA

index_df <- index_df[rowSums(is.na(index_df[,1:ncol(index_df)])) < ncol(index_df), ]

Com_1m_df <- Com_1m_df[rowSums(is.na(Com_1m_df[,1:ncol(Com_1m_df)])) < ncol(Com_1m_df), ]

Com_12m_df <- Com_12m_df[rowSums(is.na(Com_12m_df[,1:ncol(Com_12m_df)])) < ncol(Com_12m_df), ]

