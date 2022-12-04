library(tidyverse)
library(gt)
library(gtExtras)

exibble |> 
  select(-(row:group)) |> 
  gt() |> 
  opt_stylize(
    style = 3,
    color = "green"
  ) |> 
  fmt_number(
    columns = "num",
    decimals = 1,
    suffixing = TRUE
  ) |> 
  fmt_currency(
    columns = "currency",
    currency = "USD"
  )

exibble |> 
  gt() |> 
  opt_stylize(style = 3) |> 
  fmt_number(columns = 'num', decimals = 1) |>
  fmt_number(
    columns = 'currency', 
    decimals = 2, 
    locale = 'de', 
    pattern = '{x}???'
  ) |> 
  fmt_date(columns = 'date', date_style = "wday_month_day_year")  |> 
  fmt_time(columns = "time", time_style = "Hms")

exibble |> 
  select(num, currency, date, time, datetime, char) |> 
  gt() |> 
  opt_stylize(style = 3) |> 
  fmt_number(columns = 'num', decimals = 1) |>
  fmt_number(
    columns = 'currency', 
    decimals = 2, 
    locale = 'de',
    pattern = '{x}???'
  ) |> 
  fmt_date(columns = 'date', locale = 'de', date_style = "yMMMd") |> 
  fmt_time(columns = 'time', time_style = "Hms") |> 
  fmt_datetime(
    columns = 'datetime', 
    date_style = "yMMMd", 
    time_style = "Hms"
  ) |> 
  fmt(columns = 'char', fn = str_to_upper)
  

  