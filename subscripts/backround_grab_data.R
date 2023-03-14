# pull data
library(fetch)
library(dplyr)

# Fetch Stream Data
userp_list <- fetch::fetch_field(
  output = "standard")

userp_df <- userp_list$user_perception |> 
  select(
    site = UPFDH_EVENT_SMAS_HISTORY_ID,
    date = UPFDH_EVENT_SMAS_SAMPLE_DATE,
    primary = UPFDH_PRIMARY_CONTACT,
    secondary = UPFDH_SECONDARY_CONTACT,
    odors = UPFDH_ODOR,
    trash = UPFDH_TRASH
  ) |> 
  distinct() |> 
  mutate(
    program = "SMAS",
    odors = as.character(odors),
    trash = as.character(trash)
  ) |> 
  pivot_longer(
    cols = primary:trash
  )

substrRight <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}

userp_df$date <- substrRight(userp_df$EVENT_SMAS_ID, 8)

userp_df$date <- as.Date(userp_df$date, "%Y%m%d")
userp_df$year <- format(userp_df$date, "%Y")

#limit to last 10 years
userp_short <- userp_df %>%
  filter(year >= 2003)

userp_sum <- userp_short %>%
  dplyr::group_by(UPFDH_PRIMARY_CONTACT) %>%
  summarise(n = n())

userp_sum2 <- userp_short %>%
  dplyr::group_by(UPFDH_SECONDARY_CONTACT) %>%
  summarise(n = n())

userp_prim <- userp_short %>%
  select(UPFDH_PRIMARY_CONTACT, UPFDH_PRIMARY_VARIABLE) %>%
  group_by(UPFDH_PRIMARY_CONTACT, UPFDH_PRIMARY_VARIABLE) %>%
  summarise(n = n())

userp_prim2 <- userp_short %>%
  filter(
    UPFDH_PRIMARY_CONTACT %in% "D" |
      UPFDH_PRIMARY_CONTACT %in% "E",
    !UPFDH_TRASH %in% "-9999"
  ) %>%
  select(UPFDH_PRIMARY_CONTACT,
         UPFDH_TRASH) %>%
  group_by(UPFDH_PRIMARY_CONTACT, UPFDH_TRASH) %>%
  summarise(n = n())

userp_prim3 <- userp_short %>%
  filter(
    UPFDH_SECONDARY_CONTACT %in% "D" |
      UPFDH_SECONDARY_CONTACT %in% "E",
    !UPFDH_TRASH %in% "-9999"
  ) %>%
  select(UPFDH_SECONDARY_CONTACT,
         UPFDH_TRASH) %>%
  group_by(UPFDH_SECONDARY_CONTACT, UPFDH_TRASH) %>%
  summarise(n = n())

#probably need to merge with sites table to get the PWL id


#grab lakes data
lakes <- read.csv()
