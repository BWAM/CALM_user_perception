# pull data
library(fetch)
library(dplyr)

#|> 

 
kel_path = "L:/BWAM Share/SMAS/data/cleaned_files"
# Fetch Stream Data
userp_list <- suppressWarnings(fetch::fetch_field(
  path = kel_path,
  output = "standard"))

primary_smas <- userp_list$user_perception |> 
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
  tidyr::pivot_longer(
    cols = primary:secondary,
    names_to = "primary_metric",
    values_to = "primary_value"
  )

supplemental_smas <- userp_list$user_perception |> 
  select(
    site = UPFDH_EVENT_SMAS_HISTORY_ID,
    date = UPFDH_EVENT_SMAS_SAMPLE_DATE,
    odors = UPFDH_ODOR,
    trash = UPFDH_TRASH,
    water_clarity = UPFDH_WATER_CLARITY,
    phytoplankton = UPFDH_SUSPENDED_PHYTOPLANKTON,
    periphyton = UPFDH_PERIPHYTON,
    macrophyte = UPFDH_MACROPHYTE,
    discharge_pipe = UPFDH_DISCHARGE_PIPE
  ) |> 
  distinct() |> 
  filter(trash != -9999,
         odors != -9999,
         water_clarity != -9999,
         phytoplankton != -9999,
         macrophyte != -9999,
         discharge_pipe != -9999
         
         ) |>
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
    ),
    water_clarity = case_when(
      water_clarity == 0 ~ "None",
      water_clarity %in% 1:3 ~ "Low",
      water_clarity %in% 4:6 ~ "Medium",
      water_clarity %in% 7:10 ~ "High",
      TRUE ~ "ERROR"
    ),
    phytoplankton = case_when(
      phytoplankton == 0 ~ "None",
      phytoplankton %in% 1:3 ~ "Low",
      phytoplankton %in% 4:6 ~ "Medium",
      phytoplankton %in% 7:10 ~ "High",
      TRUE ~ "ERROR"
    ),
    periphyton = case_when(
      periphyton == 0 ~ "None",
      periphyton %in% 1:3 ~ "Low",
      periphyton %in% 4:6 ~ "Medium",
      periphyton %in% 7:10 ~ "High",
      TRUE ~ "ERROR"
    ),
    macrophyte = case_when(
      macrophyte == 0 ~ "None",
      macrophyte %in% 1:3 ~ "Low",
      macrophyte %in% 4:6 ~ "Medium",
      macrophyte %in% 7:10 ~ "High",
      TRUE ~ "ERROR"
    ),
    discharge_pipe = case_when(
      discharge_pipe == 0 ~ "None",
      discharge_pipe %in% 1:3 ~ "Low",
      discharge_pipe %in% 4:6 ~ "Medium",
      discharge_pipe %in% 7:10 ~ "High",
      TRUE ~ "ERROR"
    ),
  ) |> 
  tidyr::pivot_longer(
    cols = odors:discharge_pipe,
    names_to = "supplemental_metric",
    values_to = "supplemental_value"
  ) |> 
  filter(!is.na(supplemental_value)) 



#grab sites table for PWL id
sites<-fetch::fetch_sites(path=kel_path)

sites<-sites |>
  select(eventSMASHistoryId,pwlID) |>
  rename(site = eventSMASHistoryId,
         pwl = pwlID)

primary_smas_raw<-merge(primary_smas,
                    sites,
                    by = "site",
                    all.x = TRUE)



primary_smas<-merge(
  primary_smas_raw,
  supplemental_smas,
  by = c("site", "date")
)

# filter to dates and pwls

primary_smas_mr<-primary_smas |>
  dplyr::group_by(pwl,program,
                  primary_metric, 
                  primary_value,
                  supplemental_metric) |>
  dplyr::slice(which.max(date))
