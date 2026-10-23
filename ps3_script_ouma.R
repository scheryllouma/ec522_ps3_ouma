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

# ---- Part 3.1: Log transformation ----

# 3.1a: (look at the can_gas plot from 2.7c; answer in a comment below)

# 3.1b: log of Volume, then plot the logged series
can_gas <- can_gas |>
  mutate(log_volume = log(Volume))

can_gas |>
  autoplot(log_volume) +
  labs(title = "Canadian Gas Production (log scale)",
       x = NULL, y = "log(Volume)") +
  theme_minimal()

# 3.1c: same exercise for NSA retail sales
retail_nsa <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

retail_nsa |>
  autoplot(sales) +
  labs(title = "NSA Retail Sales (level)", x = NULL, y = "Millions of dollars") +
  theme_minimal()

retail_nsa |>
  autoplot(log_sales) +
  labs(title = "NSA Retail Sales (log)", x = NULL, y = "log(sales)") +
  theme_minimal()

# ---- Part 3.2: Growth rates ----

# 3.2a: two ways to compute month-over-month growth
can_gas <- can_gas |>
  mutate(
    growth_direct  = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

can_gas |>
  autoplot(growth_direct) +
  labs(title = "Canadian Gas: Month-over-Month Growth (direct)",
       x = NULL, y = "Growth rate (%)") +
  theme_minimal()

# gap between the two methods
can_gas <- can_gas |>
  mutate(growth_gap = abs(growth_direct - growth_logdiff))

can_gas |>
  autoplot(growth_gap) +
  labs(title = "Gap: Direct vs. Log-Difference Growth",
       x = NULL, y = "Absolute gap (percentage points)") +
  theme_minimal()

# ---- Part 3.3: SA vs. NSA growth rates ----
retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()
# ---- Part 4.1: Season plots ----

# 4.1a: NSA retail sales
retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

# 4.1b: SA retail sales
retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

# 4.1c: Canadian gas (logged), no key so no filter needed
can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Canadian Gas Production (log)",
    y = "log(Volume)"
  ) +
  theme_minimal()

# ---- Part 4.2: Subseries plot ----
can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  )

# ---- Part 4.3: X-13ARIMA-SEATS ----

# 4.3a: decompose NSA retail sales
retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()

# 4.3b: compare my SA series to the Census Bureau's
sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()
# ---- Part 4.4: Linear trend + seasonal dummies ----

# 4.4a: fit the model to log gas production
can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)

# coefficient table
can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)

# 4.4b: fitted vs. actual
can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()

# 4.4c: residual diagnostics
can_gas_fit |> gg_tsresiduals()

can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)
