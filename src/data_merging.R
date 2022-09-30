## Data wrangling

library(readxl)
setwd("C:/Users/Laura Nilsson/OneDrive - Aarhus Universitet/kandidatuddannelse/1.semester/data_visualization/project/DatVis_Project/src")

first_2020 <- readxl::read_xlsx("../data/2020_01_05.xlsx", skip=2)
second_2020 <- readxl::read_xlsx("../data/2020_06_12.xlsx", skip=2)

first <- merge(first_2020,second_2020)

first_2021 <- readxl::read_xlsx("../data/2021_01_05.xlsx", skip=2)
second_2021 <- readxl::read_xlsx("../data/2021_06_12.xlsx", skip=2)

second <- merge(first_2021,second_2021)
third <- merge(first,second)

first_2022 <- readxl::read_xlsx("../data/2022_01_08.xlsx", skip=2)

final <- merge(third,first_2022)

unique(colnames(final))

writexl::write_xlsx(final, "../all_years.xlsx")

