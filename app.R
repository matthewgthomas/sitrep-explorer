library(shiny)
library(arrow)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(shinycssloaders)

eng_2122 <- read_feather("data/england-2021-22.feather")
eng_2021 <- read_feather("data/england-2020-21.feather")
eng_hist_sum <- read_feather("data/england-historical.feather")
trust_2122 <- read_feather("data/trusts-2021-22.feather")
trust_2021 <- read_feather("data/trusts-2020-21.feather")
trust_hist_sum <- read_feather("data/trusts-historical.feather")

england <- read_feather("data/england.feather")
trusts <- read_feather("data/trusts.feather")

# Set colours
england <-
  england %>%
  mutate(`Occupancy rate` = `G&A beds occ'd` / `G&A Beds Open`) %>%
  mutate(
    opacity = case_when(
      year == "2021-22" ~ 1,
      year == "2020-21" ~ 1,
      TRUE ~ 0.9
    ),
    colour = case_when(
      year == "2021-22" ~ "red",
      year == "2020-21" ~ "black",
      TRUE ~ "grey"
    )
  )

trusts <-
  trusts %>%
  mutate(colour = case_when(
    year == "2021-22" ~ "red",
    year == "2020-21" ~ "black",
    TRUE ~ "grey"
  ))

# ---- Pre-wrangle bed occupancy (count) data ----
beds_eng_2122 <- eng_2122 %>%
  mutate(`Beds free` = `G&A Beds Open` - `G&A beds occ'd`) %>%
  select(day_of_year, `Beds occupied` = `G&A beds occ'd`, `Beds free`) %>%
  pivot_longer(cols = -day_of_year)

beds_eng_2021 <- eng_2021 %>%
  mutate(`Beds free` = `G&A Beds Open` - `G&A beds occ'd`) %>%
  select(day_of_year, `Beds occupied` = `G&A beds occ'd`, `Beds free`) %>%
  pivot_longer(cols = -day_of_year)

beds_eng_hist <- eng_hist_sum %>%
  mutate(`Beds free` = `Median beds open` - `Median beds occupied`) %>%
  select(day_of_year, `Beds occupied` = `Median beds occupied`, `Beds free`) %>%
  pivot_longer(cols = -day_of_year)

beds_trust_2122 <- trust_2122 %>%
  mutate(`Beds free` = `G&A Beds Open` - `G&A beds occ'd`) %>%
  select(day_of_year, Name, `Beds occupied` = `G&A beds occ'd`, `Beds free`) %>%
  pivot_longer(cols = -c(day_of_year, Name))

beds_trust_2021 <- trust_2021 %>%
  mutate(`Beds free` = `G&A Beds Open` - `G&A beds occ'd`) %>%
  select(day_of_year, Name, `Beds occupied` = `G&A beds occ'd`, `Beds free`) %>%
  pivot_longer(cols = -c(day_of_year, Name))

beds_trust_hist <- trust_hist_sum %>%
  mutate(`Beds free` = `Median beds open` - `Median beds occupied`) %>%
  select(day_of_year, `Beds occupied` = `Median beds occupied`, `Beds free`) %>%
  pivot_longer(cols = -day_of_year)

beds_england <- england %>%
  mutate(`Beds free` = `G&A Beds Open` - `G&A beds occ'd`) %>%
  select(year, day_of_year, `Beds occupied` = `G&A beds occ'd`, `Beds free`) %>%
  pivot_longer(cols = -c(year, day_of_year))

beds_england_2122 <- beds_england %>%
  filter(year == "2021-22")

beds_england_2021 <- beds_england %>%
  filter(year == "2020-21")

beds_england_historical <- beds_england %>%
  filter(!year %in% c("2021-22", "2020-21")) %>%
  group_by(name, day_of_year) %>%
  summarise(value = median(value)) %>%
  ungroup()

beds_trusts <- trusts %>%
  mutate(`Beds free` = `G&A Beds Open` - `G&A beds occ'd`) %>%
  select(year, day_of_year, Name, `Beds occupied` = `G&A beds occ'd`, `Beds free`) %>%
  pivot_longer(cols = -c(year, day_of_year, Name))


# ---- Plot functions ----
plot_trends <- function(d, indicator, indicator_name, plotting_rates = TRUE) {
  # Get years for grey lines
  year_range <-
    d %>%
    select(year, colour, {{ indicator }}) %>%
    # select(year, colour, `No. beds occupied by long-stay patients (> 21 days)`) %>%  # <-- for testing
    filter(colour == "grey") %>%
    na.omit() %>%
    select(year) %>%
    distinct() %>%
    pull(year)

  year_text <- ""

  if (length(year_range) == 1) {
    year_text <- paste0("in ", year_range)
  } else {
    year_text <- paste0("between ", year_range[length(year_range)], " and ", year_range[1])
  }

  # Set y axis limit to 100% if plotting rates
  y_axis_format <- NULL
  y_limits <- NULL

  if (plotting_rates) {
    y_axis_format <- scales::percent
    y_limits <- c(NA, 1)
  } else {
    y_axis_format <- scales::comma
  }


  d %>%
    ggplot(aes(x = day_of_year, y = {{ indicator }}, group = year)) +

    geom_line(aes(colour = year, alpha = opacity), size = 1.1, show.legend = FALSE) +

    scale_y_continuous(labels = y_axis_format, limits = y_limits) +
    scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B") +

    scale_color_manual(values = c(rep("grey", 5), "black", "red")) +

    labs(
      title = paste0(indicator_name, " in England"),
      subtitle = paste0("Red line shows rates in 2021-22; back lines are rates in 2020-21; grey lines show rates ", year_text),
      x = NULL,
      y = indicator_name,
      caption = "Source: BRC/I&I analysis of NHSE data"
    ) +
    theme_classic()
}

plot_counts <- function() {
  # Plot bar graphs for bed occupancy counts
  plt_beds_2122 <-
    beds_england_2122 %>%
    ggplot(aes(x = day_of_year, y = value, fill = name)) +
    geom_col(show.legend = FALSE) +

    scale_y_continuous(labels = scales::comma, limits = c(0, 100000)) +

    labs(
      title = " ",
      subtitle = "2021-22",
      x = NULL,
      y = "Number of beds",
      fill = NULL
      # caption = "Source: BRC/I&I analysis of NHSE data"
    ) +
    theme_classic() +
    theme(legend.position = "bottom")

  plt_beds_2021 <-
    beds_england_2021 %>%
    ggplot(aes(x = day_of_year, y = value, fill = name)) +
    geom_col() +

    scale_y_continuous(labels = scales::comma, limits = c(0, 100000)) +

    labs(
      title = " ",
      subtitle = "2020-21",
      x = NULL,
      y = "Number of beds",
      fill = NULL
      # caption = "Source: BRC/I&I analysis of NHSE data"
    ) +
    theme_classic() +
    theme(legend.position = "bottom")

  plt_beds_hist <-
    beds_england_historical %>%
    ggplot(aes(x = day_of_year, y = value, fill = name)) +
    geom_col(show.legend = FALSE) +

    scale_y_continuous(labels = scales::comma, limits = c(0, 100000)) +

    labs(
      title = " ",
      subtitle = "Averages from 2012-13 to 2019-20",
      x = NULL,
      y = "Number of beds",
      fill = NULL,
      caption = "Source: BRC/I&I analysis of NHSE data"
    ) +
    theme_classic() +
    theme(legend.position = "bottom")

  plt_beds_2122 +plt_beds_2021 + plt_beds_hist
}

# ---- UI ----
ui <- fluidPage(

    titlePanel("NHS England winter situation report explorer"),

    sidebarLayout(
        sidebarPanel(
            selectizeInput(
              "indicator",
              "Select an indicator",
              choices = c(
                "Critical care bed occupancy (rates)",
                "General & acute bed occupancy (rates)",
                "General & acute bed occupancy (counts)",
                "Beds occupied by long-stay patients (> 7 days)",
                "Beds occupied by long-stay patients (> 21 days)"
              )
            ),

            radioButtons("eng_or_trusts", "Show data for England or for individual Trusts?", choices = c("England", "Trusts")),

            conditionalPanel(
              condition = "input.eng_or_trusts == 'Trusts'",

              selectizeInput("trust_name", "Select a Trust", sort(trust_2122$Name), selected = sort(trust_2122$Name)[1], multiple = FALSE),

              radioButtons("trust_comparison", "Compare this Trust to...", choices = c("Itself historically", "Other Trusts this year", "England averages"))
            )
        ),

        mainPanel(
            shinycssloaders::withSpinner(plotOutput("plot"), color = "red")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot <- renderPlot({
      if (input$eng_or_trusts == "England" & input$indicator == "Critical care bed occupancy (rates)") {

        england %>%
          plot_trends(`Critical care beds occupancy rate`, indicator_name = input$indicator)

      } else if (input$eng_or_trusts == "England" & input$indicator == "General & acute bed occupancy (rates)") {

        england %>%
          plot_trends(`Occupancy rate`, indicator_name = input$indicator)

      } else if (input$eng_or_trusts == "England" & input$indicator == "General & acute bed occupancy (counts)") {

        plot_counts()

      } else if (input$eng_or_trusts == "England" & input$indicator == "Beds occupied by long-stay patients (> 7 days)") {

        england %>%
          plot_trends(`No. beds occupied by long-stay patients (> 21 days)`, indicator_name = input$indicator, plotting_rates = FALSE)

      } else if (input$eng_or_trusts == "England" & input$indicator == "Beds occupied by long-stay patients (> 21 days)") {

        england %>%
          plot_trends(`No. beds occupied by long-stay patients (> 7 days)`, indicator_name = input$indicator, plotting_rates = FALSE)

      } # end if
    })
}

# Run the application
shinyApp(ui = ui, server = server)
