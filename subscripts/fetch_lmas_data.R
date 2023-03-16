library(readr)
lmas_raw <- readr::read_csv(
  file.path(
    "L:/BWAM Share/SMAS/data/archive/lci.field.csv"
  ),
  col_types = readr::cols(
    LCIFD_LOCATION_HISTORY_ID = col_character(),
    LCIFD_EVENT_LMAS_SAMPLE_DATE = col_date(format = ""),
    LCIFD_EVENT_LMAS_DATA_PROVIDER = col_character(),
    LCIFD_EVENT_LMAS_SAMPLE_TIME = col_datetime(format = ""),
    LCIFD_SAMPLE_TYPE = col_character(),
    LCIFD_GLOBAL_ID = col_character(),
    LCIFD_WEATHER_48HR_PRECIP = col_character(),
    LCIFD_WETLAND = col_character(),
    LCIFD_LOW_ELEV_FLIGHT_HAZ_IND = col_character(),
    LCIFD_SAMPLE_LONG = col_character(),
    LCIFD_COMMENT = col_character(),
    LCIFD_BEACH = col_character(),
    LCIFD_USER_PERCEPT_RECREATION = col_double(),
    LCIFD_SAMPLE_LAT = col_character(),
    LCIFD_WEATHER_CURRENT_COND = col_character(),
    LCIFD_MOTORBOAT_DENSITY = col_character(),
    LCIFD_AGRICULTURE = col_character(),
    LCIFD_USER_PERCPT_PHYS_CND_CDE = col_double(),
    LCIFD_LMAS_SAMPLER = col_character(),
    LCIFD_WEATHER_CURRENT_PRECIP = col_character(),
    LCIFD_FOREST = col_character(),
    LCIFD_BARE_GROUND = col_character(),
    LCIFD_HAB_PRESENCE_IND = col_character(),
    LCIFD_WEATHER_WIND = col_character(),
    LCIFD_GRASS = col_character(),
    LCIFD_SHORELINE_MODS = col_character(),
    LCIFD_MAX_SOUND_DEPTH = col_character(),
    LCIFD_SHRUB = col_character(),
    LCIFD_DEVELOPMENT = col_character(),
    LCIFD_SITE_SOUND_DEPTH = col_character(),
    LCIFD_BOTTOM_WATER_ODOR_IND = col_character(),
    LCIFD_SECCHI_BOTTOM_IND = col_character(),
    LCIFD_EMERGENT_FLOATING_COVERG = col_character(),
    LCIFD_MACROPHTYE_DENSITY = col_character(),
    LCIFD_PRISTINE = col_double(),
    LCIFD_APPEALING = col_double(),
    LCIFD_ECOLOGICAL_INTEGRITY = col_character(),
    LCIFD_QAUL_TROPHIC_STATUS = col_character(),
    LCIFD_RESIDENCES = col_character(),
    LCIFD_MAINTAINED_LAWNS = col_character(),
    LCIFD_CONSTRUCTION = col_character(),
    LCIFD_PIPES_DRAINS = col_character(),
    LCIFD_DUMPING = col_character(),
    LCIFD_ROADS = col_character(),
    LCIFD_BRIDGES_CAUSEWAYS = col_character(),
    LCIFD_SEWAGE_TREATMENT = col_character(),
    LCIFD_HIKING_TRAILS = col_character(),
    LCIFD_PARKS_CAMPGROUNDS = col_character(),
    LCIFD_PRIMITIVE_PARKS_CAMPING = col_character(),
    LCIFD_RESORTS = col_character(),
    LCIFD_MARINAS = col_character(),
    LCIFD_TRASH_LITTER = col_character(),
    LCIFD_SURFACE_FILM_SCUM_SLICK = col_character(),
    LCIFD_CROPLAND = col_character(),
    LCIFD_PASTURE = col_character(),
    LCIFD_LIVESTOCK = col_character(),
    LCIFD_ORCHARD = col_character(),
    LCIFD_POULTRY = col_character(),
    LCIFD_FEEDLOT = col_character(),
    LCIFD_WATER_WITHDRAWL = col_character(),
    LCIFD_INDUSTRIAL_PLANTS = col_character(),
    LCIFD_MINE_QUARRY = col_character(),
    LCIFD_OIL_GAS_EXT = col_character(),
    LCIFD_POWER_PLANT = col_character(),
    LCIFD_LOGGING = col_character(),
    LCIFD_FIRE = col_character(),
    LCIFD_ODORS = col_character(),
    LCIFD_COMMERCIAL = col_character(),
    LCIFD_LIMING = col_character(),
    LCIFD_CHEMICAL_TREAT = col_character(),
    LCIFD_DRINKING_WATER_TREAT = col_character(),
    LCIFD_ANGLING_PRESS = col_character(),
    LCIFD_MACROPHYTE_CONTROL = col_character(),
    LCIFD_WATER_LVL_FLUCTUATION = col_character(),
    LCIFD_FISH_STOCK = col_character(),
    LCIFD_HYDRO_LAKE_TYPE = col_character(),
    LCIFD_OUTLET_DAMS = col_character(),
    LCIFD_SWIMABILITY = col_character(),
    LCIFD_SUBMERGENT_COVERAGE = col_character(),
    LCIFD_LAKE_LVL_CHANGES = col_double()
  )
)

primary_lmas <- lmas_raw |> 
  select(
    site = LCIFD_LOCATION_HISTORY_ID,
    date = LCIFD_EVENT_LMAS_SAMPLE_DATE,
    recreation = LCIFD_USER_PERCEPT_RECREATION,
    aesthetic = LCIFD_USER_PERCPT_PHYS_CND_CDE
  ) |> 
  distinct() |> 
  # group_by(site, year = lubridate::year(date)) |> 
  # mutate(n = n()) |> 
  # filter(year == max(year))
  mutate(
    program = "LMAS",
    recreation = if_else(
      recreation > 3,
      "Impaired",
      "Attaining"
    ),
    aesthetic = if_else(
      aesthetic  > 3,
      "Impaired",
      "Attaining"
    )
  ) |> 
  tidyr::pivot_longer(
    cols = recreation:aesthetic,
    names_to = "primary_metric",
    values_to = "primary_value"
  )

supplemental_lmas <- lmas_raw |> 
  select(
    site = LCIFD_LOCATION_HISTORY_ID,
    date = LCIFD_EVENT_LMAS_SAMPLE_DATE,
    trash = LCIFD_TRASH_LITTER,
    odors = LCIFD_ODORS
  ) |> 
  distinct() |> 
  tidyr::pivot_longer(
    cols = trash:odors,
    names_to = "supplemental_metric",
    values_to = "supplemental_value"
  ) |> 
  filter(!is.na(supplemental_value))

primary_lmas_raw<-primary_lmas

primary_lmas<-merge(
    primary_lmas_raw,
    supplemental_lmas,
    by = c("site", "date","primary_metric","primary_value","program")
  )

primary_lmas<-primary_lmas|> 
  distinct()|>
  mutate(pwl = site)
