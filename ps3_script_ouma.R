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

# 2.3a: wide -> long
retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  
  pivot_longer(
    cols = c(RSAFS, RSAFSNA),
    names_to = "series_id",
    values_to = "sales"
  )

head(retail_raw_long)
head(retail_raw)
nrow(retail_raw_long)   # should be 834
# At top of script, next to library(tidyverse):
library(fpp3)

# ---- Part 2.4: Build the tsibble ----

# 2.4a: monthly index + key
retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

# 2.4b: inspect and check for gaps
head(retail_ts)
has_gaps(retail_ts)
# ---- Part 2.5: The canadian_gas tsibble ----

# 2.5a: compare structure to retail_ts
head(canadian_gas)      # run this in the Console too
key_vars(canadian_gas)
index_var(canadian_gas)

# 2.5b: working copy + gap check
can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)
head(can_gas)

# ---- Part 2.6: Working with dates in a tsibble ----

# 2.6a: filter from January 1990 onward
can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))
head(can_gas_recent)

# 2.6b: extract year, month (label), quarter
can_gas_recent <- can_gas_recent |>
  mutate(year    = year(Month),
         month   = month(Month, label = TRUE),
         quarter = quarter(Month))

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

# 2.6c: date arithmetic on a yearmonth index
can_gas_recent |>
  mutate(next_month           = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

# 2.6d: monthly -> quarterly
can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)

# ---- Part 2.7: Plotting with autoplot() ----

# 2.7a: quick plot of both retail series
retail_ts |> autoplot(sales)

# 2.7b: add ggplot2 layers
retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

# 2.7c: Canadian gas production
can_gas |>
  autoplot(Volume) +
  labs(
    title = "Canadian Gas Production",
    x = NULL, y = "Volume (billions of cubic metres)"
  ) +
  theme_minimal()

# 2.7d: one extra layer, here a comma-formatted y-axis on retail_ts
retail_ts |>
  autoplot(sales) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()
# Added scale_y_continuous() with comma labels because the raw axis shows
# long numbers (e.g. 600000) that are hard to read at a glance.
