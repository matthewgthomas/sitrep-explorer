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
    select(Date, `Occupancy rate`, starts_with("G&A"), contains("Critical"), contains("long")) %>%
    mutate(across(-Date, as.double))
}

# get_months <- function(d) {
#   d %>%
#     mutate(MonthName = month.name[month(Date)]) %>%
#     mutate(MonthName = fct_relevel(MonthName, month.name[c(11, 12, 1:3)])) %>%
#     mutate(MonthNameShort = factor(str_sub(MonthName, 1, 3), levels = str_sub(month.name[c(11, 12, 1:3)], 1, 3)))
# }

# - Get England bed occupancy data -
england <- bind_rows(
  get_england(sitrep2122, TRUE),
  get_england(sitrep2021, TRUE),
  get_england(sitrep1920),
  get_england(sitrep1819),
  get_england(sitrep1718),
  get_england(sitrep1617),
  get_england(sitrep1516)
)

# - Summarise bed occupancies by day/month -
# eng_2122 <- england %>%
#   filter(Date >= dmy("01-11-2021") & Date <= dmy("01-05-2022")) %>%
#   mutate(day_of_year = as.Date(Date))
#
# eng_2021 <- england %>%
#   filter(Date >= dmy("01-11-2020") & Date <= dmy("01-05-2021")) %>%
#
#   # Set all dates to be in one year so this historical data overlays the 2021-22 data
#   mutate(day_of_year = if_else(month(Date) >= 11,
#                                as.Date(paste("2021", month(Date), mday(Date), sep = "-")),
#                                as.Date(paste("2022", month(Date), mday(Date), sep = "-"))))
#
# eng_hist_sum <- england %>%
#   filter(Date < dmy("01-11-2020")) %>%
#
#   # Set all dates to be in one year so this historical data overlays the 2021-22 data
#   mutate(day_of_year = if_else(month(Date) >= 11,
#                                as.Date(paste("2021", month(Date), mday(Date), sep = "-")),
#                                as.Date(paste("2022", month(Date), mday(Date), sep = "-")))) %>%
#
#   group_by(day_of_year) %>%
#   summarise(`Median occupancy rate` = median(`Occupancy rate`),
#             `Max occupancy rate` = max(`Occupancy rate`),
#             `Min occupancy rate` = min(`Occupancy rate`),
#
#             `Median beds open` = median(`G&A Beds Open`, na.rm = TRUE),
#             `Max beds open` = max(`G&A Beds Open`, na.rm = TRUE),
#             `Min beds open` = min(`G&A Beds Open`, na.rm = TRUE),
#
#             `Median beds occupied` = median(`G&A beds occ'd`, na.rm = TRUE),
#             `Max beds occupied` = max(`G&A beds occ'd`, na.rm = TRUE),
#             `Min beds occupied` = min(`G&A beds occ'd`, na.rm = TRUE),
#
#             `Median critical care beds occupancy rate` = median(`Critical care beds occupancy rate`),
#             `Max critical care beds occupancy rate` = max(`Critical care beds occupancy rate`),
#             `Min critical care beds occupancy rate` = min(`Critical care beds occupancy rate`),
#
#             `Median no. beds occupied by long-stay patients (> 7 days)` = median(`No. beds occupied by long-stay patients (> 7 days)`),
#             `Max no. beds occupied by long-stay patients (> 7 days)` = max(`No. beds occupied by long-stay patients (> 7 days)`),
#             `Min no. beds occupied by long-stay patients (> 7 days)` = min(`No. beds occupied by long-stay patients (> 7 days)`),
#
#             `Median no. beds occupied by long-stay patients (> 21 days)` = median(`No. beds occupied by long-stay patients (> 21 days)`),
#             `Max no. beds occupied by long-stay patients (> 21 days)` = max(`No. beds occupied by long-stay patients (> 21 days)`),
#             `Min no. beds occupied by long-stay patients (> 21 days)` = min(`No. beds occupied by long-stay patients (> 21 days)`))

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

# ---- Get Trust-level data ----
# - Helper functions -
get_trusts <- function(d) {
  d %>%
    filter(!str_detect(Name, "ENGLAND")) %>%
    select(Date, Name, `Occupancy rate`, starts_with("G&A"), contains("Critical"), contains("long")) %>%
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

# - Summarise bed occupancies by Trust and day/month -
# trust_2122 <- trusts %>%
#   filter(Date >= dmy("01-11-2021") & Date <= dmy("01-05-2022")) %>%
#   mutate(day_of_year = as.Date(Date))
#
# trust_2021 <- trusts %>%
#   filter(Date >= dmy("01-11-2020") & Date <= dmy("01-05-2021")) %>%
#
#   # Set all dates to be in one year so this historical data overlays the 2021-22 data
#   mutate(day_of_year = if_else(month(Date) >= 11,
#                                as.Date(paste("2021", month(Date), mday(Date), sep = "-")),
#                                as.Date(paste("2022", month(Date), mday(Date), sep = "-"))))
#
#
# trust_hist_sum <- trusts %>%
#   filter(Date < dmy("01-11-2020")) %>%
#
#   # Set all dates to be in one year so this historical data overlays the 2020-21 data
#   mutate(day_of_year = if_else(month(Date) >= 11,
#                                as.Date(paste("2021", month(Date), mday(Date), sep = "-")),
#                                as.Date(paste("2022", month(Date), mday(Date), sep = "-")))) %>%
#
#   group_by(day_of_year, Name) %>%
#   summarise(`Median occupancy rate` = median(`Occupancy rate`),
#             `Max occupancy rate` = max(`Occupancy rate`),
#             `Min occupancy rate` = min(`Occupancy rate`),
#
#             `Median beds open` = median(`G&A Beds Open`, na.rm = TRUE),
#             `Max beds open` = max(`G&A Beds Open`, na.rm = TRUE),
#             `Min beds open` = min(`G&A Beds Open`, na.rm = TRUE),
#
#             `Median beds occupied` = median(`G&A beds occ'd`, na.rm = TRUE),
#             `Max beds occupied` = max(`G&A beds occ'd`, na.rm = TRUE),
#             `Min beds occupied` = min(`G&A beds occ'd`, na.rm = TRUE),
#
#             `Median critical care beds occupancy rate` = median(`Critical care beds occupancy rate`),
#             `Max critical care beds occupancy rate` = max(`Critical care beds occupancy rate`),
#             `Min critical care beds occupancy rate` = min(`Critical care beds occupancy rate`),
#
#             `Median no. beds occupied by long-stay patients (> 7 days)` = median(`No. beds occupied by long-stay patients (> 7 days)`),
#             `Max no. beds occupied by long-stay patients (> 7 days)` = max(`No. beds occupied by long-stay patients (> 7 days)`),
#             `Min no. beds occupied by long-stay patients (> 7 days)` = min(`No. beds occupied by long-stay patients (> 7 days)`),
#
#             `Median no. beds occupied by long-stay patients (> 21 days)` = median(`No. beds occupied by long-stay patients (> 21 days)`),
#             `Max no. beds occupied by long-stay patients (> 21 days)` = max(`No. beds occupied by long-stay patients (> 21 days)`),
#             `Min no. beds occupied by long-stay patients (> 21 days)` = min(`No. beds occupied by long-stay patients (> 21 days)`))

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

# ---- Save data ----
# write_csv(eng_2122, "data/england-2021-22.csv")
# write_csv(eng_2021, "data/england-2020-21.csv")
# write_csv(eng_hist_sum, "data/england-historical.csv")
write_csv(england, "data/england.csv")
# write_csv(trust_2122, "data/trusts-2021-22.csv")
# write_csv(trust_2021, "data/trusts-2020-21.csv")
# write_csv(trust_hist_sum, "data/trusts-historical.csv")
write_csv(trusts, "data/trusts.csv")

# write_feather(eng_2122,       "data/england-2021-22.feather", compression = "uncompressed")
# write_feather(eng_2021,       "data/england-2020-21.feather", compression = "uncompressed")
# write_feather(eng_hist_sum,   "data/england-historical.feather", compression = "uncompressed")
write_feather(england,       "data/england.feather", compression = "uncompressed")
# write_feather(trust_2122,     "data/trusts-2021-22.feather", compression = "uncompressed")
# write_feather(trust_2021,     "data/trusts-2020-21.feather", compression = "uncompressed")
# write_feather(trust_hist_sum, "data/trusts-historical.feather", compression = "uncompressed")
write_feather(trusts,     "data/trusts.feather", compression = "uncompressed")
