library(tidyverse)
library(lubridate)
library(arrow)

# remotes::install_github("matthewgthomas/NHSWinterSitreps")
library(NHSWinterSitreps)

# ---- Load sitreps ----
sitrep2122 <- load_sitreps("2021-22")
sitrep2021 <- load_sitreps("2020-21")
sitrep1920 <- load_sitreps("2019-20")
sitrep1819 <- load_sitreps("2018-19")
sitrep1718 <- load_sitreps("2017-18")
sitrep1617 <- load_sitreps("2016-17")
sitrep1516 <- load_sitreps("2015-16")

# ---- Variable conversions ----
sitrep2021 <-
  sitrep2021 %>%
  mutate(across(-c(Code:Date), as.numeric))

sitrep2122 <-
  sitrep2122 %>%
  mutate(across(-c(Code:Date), as.numeric))

# ---- Get England-level data ----
# - Helper functions -
get_england <- function(d, two_englands = FALSE) {
  if (two_englands) {
    d <-
      d %>%
      filter(str_detect(Name, "ENGLAND \\(All Acute Trusts\\)"))
  } else {
    d <-
      d %>%
      filter(str_detect(Name, "ENGLAND"))
  }

  d %>%
    select(Date, `Occupancy rate`, starts_with("G&A"), contains("CC"), contains("Critical"), contains("long"), Closures, Diverts, contains("Delays")) %>%
    mutate(across(-Date, as.double))
}

# - Get all England data -
england <- bind_rows(
  get_england(sitrep2122, TRUE),
  get_england(sitrep2021, TRUE),
  get_england(sitrep1920),
  get_england(sitrep1819),
  get_england(sitrep1718),
  get_england(sitrep1617),
  get_england(sitrep1516)
)

# - Summarise by day/month -
# Set all dates to be in one year so this historical data overlays the 2021-22 data on the graphs
england <-
  england %>%
  mutate(
    year = case_when(
      Date >= dmy("01-11-2021") & Date <= dmy("01-05-2022") ~ "2021-22",
      Date >= dmy("01-11-2020") & Date <= dmy("01-05-2021") ~ "2020-21",
      Date >= dmy("01-11-2019") & Date <= dmy("01-05-2020") ~ "2019-20",
      Date >= dmy("01-11-2018") & Date <= dmy("01-05-2019") ~ "2018-19",
      Date >= dmy("01-11-2017") & Date <= dmy("01-05-2018") ~ "2017-18",
      Date >= dmy("01-11-2016") & Date <= dmy("01-05-2017") ~ "2016-17",
      Date >= dmy("01-11-2015") & Date <= dmy("01-05-2016") ~ "2015-16"
    ),
    day_of_year = if_else(month(Date) >= 11,
                          as.Date(paste("2021", month(Date), mday(Date), sep = "-")),
                          as.Date(paste("2022", month(Date), mday(Date), sep = "-")))
  )

# - Bed occupancy counts -
beds_england <- england %>%
  mutate(
    `Beds free` = `G&A Beds Open` - `G&A beds occ'd`,
    `CC beds free` = `CC Adult Open` - `CC Adult Occ`
  ) %>%
  select(year, day_of_year, `Beds occupied` = `G&A beds occ'd`, `Beds free`, `CC beds occupied` = `CC Adult Occ`, `CC beds free`) %>%
  pivot_longer(cols = -c(year, day_of_year))

beds_england_2122 <- beds_england %>%
  filter(year == "2021-22")

beds_england_2021 <- beds_england %>%
  filter(year == "2020-21")

beds_england_historical <- beds_england %>%
  filter(!year %in% c("2021-22", "2020-21")) %>%
  group_by(name, day_of_year) %>%
  summarise(value = median(value)) %>%
  ungroup() %>%
  mutate(year = "2015-16 to 2019-20")

beds_england <- bind_rows(
  beds_england_2122,
  beds_england_2021,
  beds_england_historical
)

# ---- Get Trust-level data ----
# - Helper functions -
get_trusts <- function(d) {
  d %>%
    filter(!str_detect(Name, "ENGLAND")) %>%
    select(Date, Name, `Occupancy rate`, starts_with("G&A"), contains("CC"), contains("Critical"), contains("long"), Closures, Diverts, contains("Delays")) %>%
    mutate(across(-c(Date, Name), as.double))
}

# - Get Trust bed occupancy data -
trusts <- bind_rows(
  get_trusts(sitrep2122),
  get_trusts(sitrep2021),
  get_trusts(sitrep1920),
  get_trusts(sitrep1819),
  get_trusts(sitrep1718),
  get_trusts(sitrep1617),
  get_trusts(sitrep1516)
)

# - Summarise by Trust and day/month -
# Set all dates to be in one year so this historical data overlays the 2021-22 data on the graphs
trusts <-
  trusts %>%
  mutate(
    year = case_when(
      Date >= dmy("01-11-2021") & Date <= dmy("01-05-2022") ~ "2021-22",
      Date >= dmy("01-11-2020") & Date <= dmy("01-05-2021") ~ "2020-21",
      Date >= dmy("01-11-2019") & Date <= dmy("01-05-2020") ~ "2019-20",
      Date >= dmy("01-11-2018") & Date <= dmy("01-05-2019") ~ "2018-19",
      Date >= dmy("01-11-2017") & Date <= dmy("01-05-2018") ~ "2017-18",
      Date >= dmy("01-11-2016") & Date <= dmy("01-05-2017") ~ "2016-17",
      Date >= dmy("01-11-2015") & Date <= dmy("01-05-2016") ~ "2015-16"
    ),
    day_of_year = if_else(month(Date) >= 11,
                          as.Date(paste("2021", month(Date), mday(Date), sep = "-")),
                          as.Date(paste("2022", month(Date), mday(Date), sep = "-")))
  )

# - Bed occupancy counts -
beds_trusts <- trusts %>%
  mutate(
    `Beds free` = `G&A Beds Open` - `G&A beds occ'd`,
    `CC beds free` = `CC Adult Open` - `CC Adult Occ`
  ) %>%
  select(year, day_of_year, Name, `Beds occupied` = `G&A beds occ'd`, `Beds free`, `CC beds occupied` = `CC Adult Occ`, `CC beds free`) %>%
  pivot_longer(cols = -c(year, day_of_year, Name))

beds_trusts_2122 <- beds_trusts %>%
  filter(year == "2021-22")

beds_trusts_2021 <- beds_trusts %>%
  filter(year == "2020-21")

beds_trusts_historical <- beds_trusts %>%
  filter(!year %in% c("2021-22", "2020-21")) %>%
  group_by(Name, name, day_of_year) %>%
  summarise(value = median(value)) %>%
  ungroup() %>%
  mutate(year = "2015-16 to 2019-20")

beds_trusts <- bind_rows(
  beds_trusts_2122,
  beds_trusts_2021,
  beds_trusts_historical
)

# ---- Save data ----
write_csv(england, "data/england.csv")
write_csv(beds_england, "data/england-beds.csv")
write_csv(trusts, "data/trusts.csv")
write_csv(beds_trusts, "data/trusts-beds.csv")

write_feather(england, "data/england.feather", compression = "uncompressed")
write_feather(beds_england, "data/england-beds.feather", compression = "uncompressed")
write_feather(trusts,     "data/trusts.feather", compression = "uncompressed")
write_feather(beds_trusts, "data/trusts-beds.feather", compression = "uncompressed")
