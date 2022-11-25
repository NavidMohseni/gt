library(tidyverse)
library(palmerpenguins)
library(gt)

#https://gt.albert-rapp.de/fancy_stuff.html

penguins <- penguins |> 
  filter(!is.na(sex))

penguins

penguins_count <- penguins |> 
  mutate(year = as.character(year)) |> 
  group_by(species, island, sex, year) |> 
  summarise(n = n(), .groups = "drop") 

penguins_count_wider <- penguins_count |>  
  pivot_wider(
    names_from = c(species, sex),
    values_from = n
  ) |> 
  mutate(across(.cols = -c(1:2), 
                .fns = ~replace_na(data = ., replace = 0))) |> 
  arrange(island, year)

penguins_count_wider |> 
  gt() |> 
  cols_label(
        island = 'Island',
        year = 'Year',
        Adelie_female = 'Adelie (female)',
        Adelie_male = 'Adelie (male)',
        Chinstrap_female = 'Chinstrap (female)',
        Chinstrap_male = 'Chinstrap (male)',
        Gentoo_female = 'Gentoo (female)',
        Gentoo_male = 'Gentoo (male)',
      ) |> 
  tab_spanner(
    label = md("**Adelie**"),
    columns = 3:4
  ) |> 
  tab_spanner(
    label = md("**Chinstrap**"),
    columns = 5:6
  ) |> 
  tab_spanner(
    label = md("**Gentoo**"),
    columns = contains("Gentoo")
  )


penguins_count_wider |> 
  gt() |> 
  cols_label(.list = desired_colnames) |> 
  tab_spanner(
    label = md("**Adelie**"),
    columns = 3:4
  ) |> 
  tab_spanner(
    label = md("**Chinstrap**"),
    columns = 5:6
  ) |> 
  tab_spanner(
    label = md("**Gentoo**"),
    columns = contains("Gentoo")
  )

spanners_and_header <- function(gt_tbl) {
  gt_tbl |> 
    tab_spanner(
      label = md('**Adelie**'),
      columns = 3:4
    ) |> 
    tab_spanner(
      label = md('**Chinstrap**'),
      columns = c('Chinstrap_female', 'Chinstrap_male')
    ) |> 
    tab_spanner(
      label =  md('**Gentoo**'),
      columns = contains('Gentoo')
    ) |> 
    tab_header(
      title = 'Penguins in the Palmer Archipelago',
      subtitle = 'Data is courtesy of the {palmerpenguins} R package'
    ) 
}

# This produces the same output
penguins_count_wider |> 
  gt() |> 
  cols_label(
    island = 'Island',
    year = 'Year',
    Adelie_female = 'Adelie (female)',
    Adelie_male = 'Adelie (male)',
    Chinstrap_female = 'Chinstrap (female)',
    Chinstrap_male = 'Chinstrap (male)',
    Gentoo_female = 'Gentoo (female)',
    Gentoo_male = 'Gentoo (male)')  |> 
  spanners_and_header() |> 
  cols_align(
    align = "right",
    columns = "year"  
  ) |> 
  cols_align(
    align = "left",
    columns = where(is.factor)
  )

penguins_count_wider |> 
  gt(groupname_col = "island",
     rowname_col = "year") |> 
  cols_label(
    island = 'Island',
    year = 'Year',
    Adelie_female = 'Adelie (female)',
    Adelie_male = 'Adelie (male)',
    Chinstrap_female = 'Chinstrap (female)',
    Chinstrap_male = 'Chinstrap (male)',
    Gentoo_female = 'Gentoo (female)',
    Gentoo_male = 'Gentoo (male)')  |> 
  spanners_and_header() |> 
  cols_align(
    align = "right",
    columns = "year"  
  ) |> 
  cols_align(
    align = "left",
    columns = where(is.factor)
  ) |> 
  sub_zero(zero_text = "-") |> 
  summary_rows(
    groups = TRUE,
    fns = list(
      'Maximum' = ~max(.),
      'Total' = ~sum(.)
    ), 
    formatter = fmt_number,
    decimals = 0 
  ) |> 
  tab_options(
    data_row.padding = px(2),
    summary_row.padding = px(3),
    row_group.padding = px(4)
  ) |> 
  opt_stylize(
    style = 3,
    color = "cyan"
  )
