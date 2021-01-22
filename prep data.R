library(tidyverse)
library(lubridate)
library(arrow)

# remotes::install_github("matthewgthomas/NHSWinterSitreps")
library(NHSWinterSitreps)

# ---- Load sitreps ----
sitrep2021 <- load_sitreps("2020-21")
sitrep1920 <- load_sitreps("2019-20")
sitrep1819 <- load_sitreps("2018-19")
sitrep1718 <- load_sitreps("2017-18")
sitrep1617 <- load_sitreps("2016-17")
sitrep1516 <- load_sitreps("2015-16")

# ---- Get England-level data ----
# - Helper functions -
get_england <- function(d) {
  d %>%
    filter(str_detect(Name, "ENGLAND")) %>%
    select(Date, `Occupancy rate`, contains("Critical"), contains("long")) %>%
    mutate(across(-Date, as.double))
}

# get_months <- function(d) {
#   d %>%
#     mutate(MonthName = month.name[month(Date)]) %>%
#     mutate(MonthName = fct_relevel(MonthName, month.name[c(11, 12, 1:3)])) %>%
#     mutate(MonthNameShort = factor(str_sub(MonthName, 1, 3), levels = str_sub(month.name[c(11, 12, 1:3)], 1, 3)))
# }

# - Get England bed occupancy data -
eng_beds <- bind_rows(
  get_england(sitrep2021),
  get_england(sitrep1920),
  get_england(sitrep1819),
  get_england(sitrep1718),
  get_england(sitrep1617),
  get_england(sitrep1516)
)

# - Summarise bed occupancies by day/month -
eng_2021 <- eng_beds %>%
  filter(Date >= dmy("01-11-2020")) %>%
  mutate(day_of_year = as.Date(Date))

eng_hist_sum <- eng_beds %>%
  filter(Date < dmy("01-11-2020")) %>%

  # Set all dates to be in one year so this historical data overlays the 2020-21 data
  mutate(day_of_year = if_else(month(Date) >= 11,
                               as.Date(paste("2020", month(Date), mday(Date), sep = "-")),
                               as.Date(paste("2021", month(Date), mday(Date), sep = "-")))) %>%

  group_by(day_of_year) %>%
  summarise(`Median occupancy rate` = median(`Occupancy rate`),
            `Max occupancy rate` = max(`Occupancy rate`),
            `Min occupancy rate` = min(`Occupancy rate`),

            `Median occupancy rate` = median(`Critical care beds occupancy rate`),
            `Max occupancy rate` = max(`Critical care beds occupancy rate`),
            `Min occupancy rate` = min(`Critical care beds occupancy rate`),

            `Median occupancy rate` = median(`No. beds  occupied by long-stay patients (> 7 days)`),
            `Max occupancy rate` = max(`No. beds  occupied by long-stay patients (> 7 days)`),
            `Min occupancy rate` = min(`No. beds  occupied by long-stay patients (> 7 days)`),

            `Median occupancy rate` = median(`No. beds occupied by long-stay patients (> 21 days)`),
            `Max occupancy rate` = max(`No. beds occupied by long-stay patients (> 21 days)`),
            `Min occupancy rate` = min(`No. beds occupied by long-stay patients (> 21 days)`))

# ---- Get Trust-level data ----
# - Helper functions -
get_trusts <- function(d) {
  d %>%
    filter(!str_detect(Name, "ENGLAND")) %>%
    select(Date, `Occupancy rate`, contains("Critical"), contains("long")) %>%
    mutate(across(-Date, as.double))
}

# - Get Trust bed occupancy data -
trust_beds <- bind_rows(
  get_trusts(sitrep2021),
  get_trusts(sitrep1920),
  get_trusts(sitrep1819),
  get_trusts(sitrep1718),
  get_trusts(sitrep1617),
  get_trusts(sitrep1516)
)

# - Summarise bed occupancies by Trust and day/month -
trust_2021 <- trust_beds %>%
  filter(Date >= dmy("01-11-2020")) %>%
  mutate(day_of_year = as.Date(Date))

trust_hist_sum <- trust_beds %>%
  filter(Date < dmy("01-11-2020")) %>%

  # Set all dates to be in one year so this historical data overlays the 2020-21 data
  mutate(day_of_year = if_else(month(Date) >= 11,
                               as.Date(paste("2020", month(Date), mday(Date), sep = "-")),
                               as.Date(paste("2021", month(Date), mday(Date), sep = "-")))) %>%

  group_by(day_of_year, Name) %>%
  summarise(`Median occupancy rate` = median(`Occupancy rate`),
            `Max occupancy rate` = max(`Occupancy rate`),
            `Min occupancy rate` = min(`Occupancy rate`),

            `Median occupancy rate` = median(`Critical care beds occupancy rate`),
            `Max occupancy rate` = max(`Critical care beds occupancy rate`),
            `Min occupancy rate` = min(`Critical care beds occupancy rate`),

            `Median occupancy rate` = median(`No. beds  occupied by long-stay patients (> 7 days)`),
            `Max occupancy rate` = max(`No. beds  occupied by long-stay patients (> 7 days)`),
            `Min occupancy rate` = min(`No. beds  occupied by long-stay patients (> 7 days)`),

            `Median occupancy rate` = median(`No. beds occupied by long-stay patients (> 21 days)`),
            `Max occupancy rate` = max(`No. beds occupied by long-stay patients (> 21 days)`),
            `Min occupancy rate` = min(`No. beds occupied by long-stay patients (> 21 days)`))

# ---- Save data ----
write_csv(eng_2021, "data/england-2020-21.csv")
write_csv(eng_hist_sum, "data/england-historical.csv")
write_csv(trust_2021, "data/trusts-2020-21.csv")
write_csv(trust_hist_sum, "data/trusts-historical.csv")

write_feather(eng_2021,       "data/england-2020-21.feather", compression = "uncompressed")
write_feather(eng_hist_sum,   "data/england-historical.feather", compression = "uncompressed")
write_feather(trust_2021,     "data/trusts-2020-21.feather", compression = "uncompressed")
write_feather(trust_hist_sum, "data/trusts-historical.feather", compression = "uncompressed")

# ---- Test plots ----
# - Bed occupancy over time -
eng_hist_sum %>%
  ggplot(aes(x = day_of_year, y = `Median occupancy rate`, group = 1)) +
  geom_ribbon(aes(ymin = `Min occupancy rate`, ymax = `Max occupancy rate`), fill = "grey", alpha = 0.4) +
  geom_line(colour = "grey", lty = 2, size = 1.1) +

  geom_line(data = eng_2021, aes(y = `Occupancy rate`), colour = "red", size = 1.1) +

  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B") +

  labs(x = NULL, y = "Bed occupancy rate", caption = "Source: BRC/I&I analysis of NHSE data") +
  theme_classic()

# - Bed occupancy over time for a Trust -
trust_hist_sum %>%
  filter(Name == "Airedale NHS Foundation Trust") %>%

  ggplot(aes(x = day_of_year, y = `Median occupancy rate`, group = 1)) +
  geom_ribbon(aes(ymin = `Min occupancy rate`, ymax = `Max occupancy rate`), fill = "grey", alpha = 0.4) +
  geom_line(colour = "grey", lty = 2, size = 1.1) +

  geom_line(data = trust_2021 %>% filter(Name == "Airedale NHS Foundation Trust"),
            aes(y = `Occupancy rate`), colour = "red", size = 1.1) +

  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B") +

  labs(x = NULL, y = "Bed occupancy rate", caption = "Source: BRC/I&I analysis of NHSE data") +
  theme_classic()
