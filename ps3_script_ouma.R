# ---- Part 2: FRED retail sales ----
library(tidyverse)

# 2.2a: guessed read-in
retail_raw <- read_csv("data/retail_sales_raw.csv")
head(retail_raw)
# 2.2a answer: observation_date is class <date>  

# 2.2b: explicit column types
retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)
glimpse(retail_raw)
src <- file.choose()
file.copy(src, "data/retail_sales_raw.csv", overwrite = TRUE)