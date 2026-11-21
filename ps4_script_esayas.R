library(tidyverse)
library(fpp3)
library(BVAR)

# 2.1a
liquor_raw <- read_csv(
  "data/MRTSSM4453USN.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    MRTSSM4453USN = col_double()
  )
)

liquor <- liquor_raw |>
  transmute(Month = yearmonth(observation_date), sales = MRTSSM4453USN) |>
  as_tsibble(index = Month) |>
  filter(Month <= yearmonth("2019 Dec"))

has_gaps(liquor)

# 2.1b
p_liquor <- liquor |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail Sales: Beer, Wine, and Liquor Stores",
    subtitle = "Not seasonally adjusted, Jan 1992 - Dec 2019",
    x = NULL, y = "Millions of dollars"
  ) +
  theme_minimal()

p_liquor_season <- liquor |>
  gg_season(log(sales)) +
  labs(
    title = "Seasonal Plot: Log Liquor Store Sales",
    x = "Month", y = "log(Sales)"
  ) +
  theme_minimal()

p_liquor
p_liquor_season

# 2.2a
liquor_fit <- liquor |>
  model(arima = ARIMA(log(sales)),
        ets   = ETS(log(sales)))

liquor_fit |> select(arima) |> report()
liquor_fit |> select(ets) |> report()

# # 2.2b
# liquor_fit |> select(arima) |> gg_tsresiduals()

# liquor_fit |>
#   select(arima) |>
#   augment() |>
#   features(.innov, ljung_box, lag = 24, dof = ___)

#   # 2.3a
# liquor_cv <- liquor |>
#   stretch_tsibble(.init = 120, .step = 1) |>
#   filter(.id < max(.id))

# liquor_cv_fit <- liquor_cv |>
#   model(snaive = SNAIVE(sales),
#         ets    = ETS(log(sales)),
#         arima  = ARIMA(log(sales)))

# saveRDS(liquor_cv_fit, "results/liquor_cv_fit.rds")