library(fetch)
library(janitor)
library(tidyverse)

# To DO
# Get PWL
# Max date for multiple visits to the same site
# Filter trash and odor DF to only pwls with impairment status

source(here::here("subscripts", "fetch_smas_data.R"))
source(here::here("subscripts", "fetch_lmas_data.R"))

primary_prep <- bind_rows(
  lmas_prep,
  smas_prep
)

primary_counts_df <- primary_prep |> 
  count(value, program, name) |> 
  group_by(program, name) |> 
  mutate(total = sum(n)) |> 
  ungroup() |> 
  mutate(percent = round(n / total * 100, 2))


supplemental_prep <- bind_rows(
  lmas_trash_odor,
  smas_trash_odor
)

supplemental_counts_df <- supplemental_prep  |> 
  count(supplemental_value,
        program, supplemental_metric, primary_metric, primary_value) |> 
  group_by(program, supplemental_metric, primary_metric) |> 
  mutate(total = sum(n)) |> 
  ungroup() |> 
  mutate(percent = round(n / total * 100, 2))


