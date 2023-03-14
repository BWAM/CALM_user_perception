# pull data
library(fetch)
library(dplyr)

# Fetch Stream Data
userp_list <- fetch::fetch_field(
  output = "standard")

smas_prep <- userp_list$user_perception |> 
  select(
    site = UPFDH_EVENT_SMAS_HISTORY_ID,
    date = UPFDH_EVENT_SMAS_SAMPLE_DATE,
    primary = UPFDH_PRIMARY_CONTACT,
    secondary = UPFDH_SECONDARY_CONTACT
  ) |> 
  distinct() |> 
  mutate(
    program = "SMAS",
    primary = if_else(
      primary %in% c("D", "E"),
      "Impaired",
      "Attaining"
    ),
    secondary = if_else(
      secondary %in% c("D", "E"),
      "Impaired",
      "Attaining"
    )
  ) |> 
  pivot_longer(
    cols = primary:secondary,
    names_to = "primary_metric",
    values_to = "primary_value"
  )

smas_trash_odor <- userp_list$user_perception |> 
  select(
    site = UPFDH_EVENT_SMAS_HISTORY_ID,
    date = UPFDH_EVENT_SMAS_SAMPLE_DATE,
    odors = UPFDH_ODOR,
    trash = UPFDH_TRASH
  ) |> 
  distinct() |> 
  filter(trash != -9999,
         odors != -9999) |>
  mutate(
    odors = case_when(
      odors == 0 ~ "None",
      odors %in% 1:3 ~ "Low",
      odors %in% 4:6 ~ "Medium",
      odors %in% 7:10 ~ "High",
      TRUE ~ "ERROR"
    ),
    trash = case_when(
      trash == 0 ~ "None",
      trash %in% 1:3 ~ "Low",
      trash %in% 4:6 ~ "Medium",
      trash %in% 7:10 ~ "High",
      TRUE ~ "ERROR"
    )
  ) |> 
  pivot_longer(
    cols = trash:odors,
    names_to = "supplemental_metric",
    values_to = "supplemental_value"
  ) |> 
  filter(!is.na(supplemental_value)) |> 
  left_join(
    smas_prep,
    by = c("site", "date")
  )
