library(tidyverse)
library(gt)
library(gtExtras)

gapminder_data <- gapminder::gapminder |> 
  janitor::clean_names() |> 
  select(continent, country, year, life_exp) |> 
  mutate(
    year = as.character(year),
    country = as.character(country)
  )

gapminder_data |> 
  gt_plt_summary()

selected_countries <- gapminder_data |>
  filter(str_ends(year, pattern = "7")) |> 
  group_by(continent, country) |> 
  nest() |> 
  group_by(continent) |> 
  slice_sample(n = 2) |> 
  ungroup() |> 
  unnest(data) |> 
  pivot_wider(
    names_from = year,
    names_prefix = "year",
    values_from = life_exp
  )
  
# New column names
new_colnames <- colnames(selected_countries) |>
  str_remove('(country|year)')

names(new_colnames) <- colnames(selected_countries)
color_palette <- c("#CC79A7", "#009E73")

selected_countries |> 
  gt(groupname_col = 'continent') |> 
  tab_header(
    title = 'Life Expectancies over time',
    subtitle = 'Data is courtesy of the Gapminder foundation'
  ) |> 
  cols_label(.list = new_colnames) |> 
  fmt_number(columns = where(is.numeric), 
             decimals = 2) |> 
  gt_theme_538() |> 
  gt_color_rows(
    columns = year2007,
    palette = color_palette,
    domain = c(30, 85)
  )
  

