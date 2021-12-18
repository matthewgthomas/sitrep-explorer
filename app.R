library(shiny)
library(arrow)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
# library(patchwork)
library(shinycssloaders)

# eng_2122 <- read_feather("data/england-2021-22.feather")
# eng_2021 <- read_feather("data/england-2020-21.feather")
# eng_hist_sum <- read_feather("data/england-historical.feather")
# trust_2122 <- read_feather("data/trusts-2021-22.feather")
# trust_2021 <- read_feather("data/trusts-2020-21.feather")
# trust_hist_sum <- read_feather("data/trusts-historical.feather")

england <- read_feather("data/england.feather")
england_beds <- read_feather("data/england-beds.feather")
trusts <- read_feather("data/trusts.feather")
trusts_beds <- read_feather("data/trusts-beds.feather")

trust_names <-
  trusts %>%
  filter(year == "2021-22") %>%
  select(Name) %>%
  distinct() %>%
  arrange(Name) %>%
  pull(Name)

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

# ---- Pre-wrangle bed occupancy (count) data ----
# beds_eng_2122 <- eng_2122 %>%
#   mutate(`Beds free` = `G&A Beds Open` - `G&A beds occ'd`) %>%
#   select(day_of_year, `Beds occupied` = `G&A beds occ'd`, `Beds free`) %>%
#   pivot_longer(cols = -day_of_year)
#
# beds_eng_2021 <- eng_2021 %>%
#   mutate(`Beds free` = `G&A Beds Open` - `G&A beds occ'd`) %>%
#   select(day_of_year, `Beds occupied` = `G&A beds occ'd`, `Beds free`) %>%
#   pivot_longer(cols = -day_of_year)
#
# beds_eng_hist <- eng_hist_sum %>%
#   mutate(`Beds free` = `Median beds open` - `Median beds occupied`) %>%
#   select(day_of_year, `Beds occupied` = `Median beds occupied`, `Beds free`) %>%
#   pivot_longer(cols = -day_of_year)
#
# beds_trust_2122 <- trust_2122 %>%
#   mutate(`Beds free` = `G&A Beds Open` - `G&A beds occ'd`) %>%
#   select(day_of_year, Name, `Beds occupied` = `G&A beds occ'd`, `Beds free`) %>%
#   pivot_longer(cols = -c(day_of_year, Name))
#
# beds_trust_2021 <- trust_2021 %>%
#   mutate(`Beds free` = `G&A Beds Open` - `G&A beds occ'd`) %>%
#   select(day_of_year, Name, `Beds occupied` = `G&A beds occ'd`, `Beds free`) %>%
#   pivot_longer(cols = -c(day_of_year, Name))
#
# beds_trust_hist <- trust_hist_sum %>%
#   mutate(`Beds free` = `Median beds open` - `Median beds occupied`) %>%
#   select(day_of_year, `Beds occupied` = `Median beds occupied`, `Beds free`) %>%
#   pivot_longer(cols = -day_of_year)


# ---- Plot functions ----
plot_trends <- function(d, indicator, indicator_name, trust_name = NULL, plotting_rates = TRUE) {
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

  num_years <- length(year_range)

  if (num_years == 1) {
    year_text <- paste0("grey line is ", year_range)
  } else {
    year_text <- paste0("grey lines are between ", year_range[num_years], " and ", year_range[1])
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

  place <- ifelse(is.null(trust_name), "England", trust_name)

  d %>%
    select(day_of_year, year, colour, opacity, {{ indicator }}) %>%
    na.omit() %>%

    ggplot(aes(x = day_of_year, y = {{ indicator }}, group = year)) +

    geom_line(aes(colour = year, alpha = opacity), size = 1.1, show.legend = FALSE) +

    scale_y_continuous(labels = y_axis_format, limits = y_limits) +
    scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B") +

    scale_color_manual(values = c(rep("grey", num_years), "black", "red")) +

    labs(
      title = paste0(indicator_name, " in ", place),
      subtitle = paste0("Red line is 2021-22; black line is 2020-21; ", year_text),
      x = NULL,
      y = indicator_name,
      caption = "Source: BRC/I&I analysis of NHSE data"
    ) +
    theme_classic()
}

plot_counts <- function(d, indicator_name, trust_name = NULL) {
  # Set up dataframes to use for plotting
  # d_2122 <- NULL
  # d_2021 <- NULL
  # d_hist <- NULL
  #
  # if (is.null(trust_name)) {
  #   d_2122 <- beds_england_2122
  #   d_2021 <- beds_england_2021
  #   d_hist <- beds_england_historical
  #
  # } else {
  #   d_2122 <- beds_trusts_2122 %>% filter(Name == trust_name)
  #   d_2021 <- beds_trusts_2021 %>% filter(Name == trust_name)
  #   d_hist <- beds_trusts_historical %>% filter(Name == trust_name)
  #
  # }

  # max_beds <- max(max(d_2122$value), max(d_2021$value), max(d_hist$value))
  max_beds <- max(d$value, na.rm = TRUE)

  place <- ifelse(is.null(trust_name), "England", trust_name)

  d %>%
    ggplot(aes(x = day_of_year, y = value, fill = name)) +
    geom_col() +

    facet_wrap(~year) +

    scale_y_continuous(labels = scales::comma, limits = c(0, max_beds)) +
    scale_fill_manual(values = c("grey20", "red")) +

    labs(
      title = paste0(indicator_name, " in ", place),
      # subtitle = "2021-22",
      x = NULL,
      y = "Number of beds",
      fill = NULL,
      caption = "Source: BRC/I&I analysis of NHSE data"
    ) +
    theme_classic() +
    theme(legend.position = "bottom")

  # Plot bar graphs for bed occupancy counts
  # plt_beds_2122 <-
  #   d_2122 %>%
  #   ggplot(aes(x = day_of_year, y = value, fill = name)) +
  #   geom_col(show.legend = FALSE) +
  #
  #   scale_y_continuous(labels = scales::comma, limits = c(0, max_beds)) +
  #
  #   labs(
  #     title = " ",
  #     subtitle = "2021-22",
  #     x = NULL,
  #     y = "Number of beds",
  #     fill = NULL
  #     # caption = "Source: BRC/I&I analysis of NHSE data"
  #   ) +
  #   theme_classic() +
  #   theme(legend.position = "bottom")
  #
  # plt_beds_2021 <-
  #   d_2021 %>%
  #   ggplot(aes(x = day_of_year, y = value, fill = name)) +
  #   geom_col() +
  #
  #   scale_y_continuous(labels = scales::comma, limits = c(0, max_beds)) +
  #
  #   labs(
  #     title = " ",
  #     subtitle = "2020-21",
  #     x = NULL,
  #     y = "Number of beds",
  #     fill = NULL
  #     # caption = "Source: BRC/I&I analysis of NHSE data"
  #   ) +
  #   theme_classic() +
  #   theme(legend.position = "bottom")
  #
  # plt_beds_hist <-
  #   d_hist %>%
  #   ggplot(aes(x = day_of_year, y = value, fill = name)) +
  #   geom_col(show.legend = FALSE) +
  #
  #   scale_y_continuous(labels = scales::comma, limits = c(0, max_beds)) +
  #
  #   labs(
  #     title = " ",
  #     subtitle = "Averages from 2012-13 to 2019-20",
  #     x = NULL,
  #     y = "Number of beds",
  #     fill = NULL,
  #     caption = "Source: BRC/I&I analysis of NHSE data"
  #   ) +
  #   theme_classic() +
  #   theme(legend.position = "bottom")
  #
  # plt_beds_2122 +plt_beds_2021 + plt_beds_hist
}

plot_trust_comparison_trends <- function(d, indicator, indicator_name, trust_name, this_year = "2021-22", plotting_rates = TRUE) {
  # this_trust <-
  #   trusts %>%
  #   filter(Name == trust_name & year == year)
  #
  # other_trusts <-
  #   trusts %>%
  #   filter(Name != trust_name & year == year)

  # Set y axis limit to 100% if plotting rates
  y_axis_format <- NULL
  y_limits <- NULL

  if (plotting_rates) {
    y_axis_format <- scales::percent
    y_limits <- c(NA, 1)
  } else {
    y_axis_format <- scales::comma
  }

  ggplot() +
    geom_line(data = d %>% filter(Name != trust_name & year == this_year),
              aes(x = day_of_year, y = {{ indicator }}, group = Name),
              colour = "grey", alpha = 0.7) +

    geom_line(data = d %>% filter(Name == trust_name & year == this_year),
              aes(x = day_of_year, y = {{ indicator }}),
              colour = "red", size = 1.1) +

    scale_y_continuous(labels = y_axis_format, limits = y_limits) +
    scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B") +

    labs(
      title = paste0(indicator_name, " in ", trust_name, " compared to other Trusts"),
      subtitle = "Red line shows rates for this Trust; grey lines show other Trusts",
      x = NULL,
      y = indicator_name,
      caption = "Source: BRC/I&I analysis of NHSE data"
    ) +
    theme_classic()
}

plot_trust_england_comparison_trends <- function(d, indicator, indicator_name, trust_name, this_year = "2021-22", plotting_rates = TRUE) {
  # Set y axis limit to 100% if plotting rates
  y_axis_format <- NULL
  y_limits <- NULL

  if (plotting_rates) {
    y_axis_format <- scales::percent
    y_limits <- c(NA, 1)
  } else {
    y_axis_format <- scales::comma
  }

  england %>%
    filter(year == this_year) %>%

    ggplot(aes(x = day_of_year, y = {{ indicator }}, group = 1)) +

    geom_line(colour = "grey", lty = 2, size = 1.1) +

    geom_line(data = trusts %>% filter(Name == trust_name & year == this_year),
              aes(y = {{ indicator }}), colour = "red", size = 1.1) +

    scale_y_continuous(labels = y_axis_format) +
    scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B") +

    labs(
      title = paste0(indicator_name, " in ", trust_name, " compared to England as a whole"),
      subtitle = paste0("Red line shows ", this_year, " rates for this Trust; grey line show England rate"),
      x = NULL,
      y = indicator_name,
      caption = "Source: BRC/I&I analysis of NHSE data"
    ) +
    theme_classic()
}

empty_graph <-
  ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 8,
           label = "Graph not currently available.") +
  theme_void()

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
                "Critical care bed occupancy (counts)",
                "General & acute bed occupancy (rates)",
                "General & acute bed occupancy (counts)",
                "Beds occupied by long-stay patients (> 7 days)",
                "Beds occupied by long-stay patients (> 21 days)"
              )
            ),

            radioButtons("eng_or_trusts", "Show data for England or for individual Trusts?", choices = c("England", "Trusts")),

            conditionalPanel(
              condition = "input.eng_or_trusts == 'Trusts'",

              selectizeInput("trust_name", "Select a Trust", trust_names, selected = trust_names[1], multiple = FALSE),

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

      ##
      ## Plots for England
      ##
      if (input$eng_or_trusts == "England" & input$indicator == "Critical care bed occupancy (rates)") {

        england %>%
          plot_trends(`Critical care beds occupancy rate`, indicator_name = input$indicator)

      } else if (input$eng_or_trusts == "England" & input$indicator == "Critical care bed occupancy (counts)") {

        england_beds %>%
          filter(str_detect(name, "^CC")) %>%
          plot_counts(indicator_name = input$indicator)

      } else if (input$eng_or_trusts == "England" & input$indicator == "General & acute bed occupancy (rates)") {

        england %>%
          plot_trends(`Occupancy rate`, indicator_name = input$indicator)

      } else if (input$eng_or_trusts == "England" & input$indicator == "General & acute bed occupancy (counts)") {

        england_beds %>%
          filter(!str_detect(name, "^CC")) %>%
          plot_counts(indicator_name = input$indicator)

      } else if (input$eng_or_trusts == "England" & input$indicator == "Beds occupied by long-stay patients (> 7 days)") {

        england %>%
          plot_trends(`No. beds occupied by long-stay patients (> 7 days)`, indicator_name = input$indicator, plotting_rates = FALSE)

      } else if (input$eng_or_trusts == "England" & input$indicator == "Beds occupied by long-stay patients (> 21 days)") {

        england %>%
          plot_trends(`No. beds occupied by long-stay patients (> 21 days)`, indicator_name = input$indicator, plotting_rates = FALSE)

      ##
      ## Plots for Trusts
      ##
      # - Comparison with itself historically -
      } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "Itself historically" & input$indicator == "Critical care bed occupancy (rates)") {

        trusts %>%
          filter(Name == input$trust_name) %>%
          plot_trends(`Critical care beds occupancy rate`, indicator_name = input$indicator, trust_name = input$trust_name)

      } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "Itself historically" & input$indicator == "Critical care bed occupancy (counts)") {

        trusts_beds %>%
          filter(str_detect(name, "^CC") & Name == input$trust_name) %>%
          plot_counts(input$indicator, trust_name = input$trust_name)

      } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "Itself historically" & input$indicator == "General & acute bed occupancy (rates)") {

        trusts %>%
          filter(Name == input$trust_name) %>%
          plot_trends(`Occupancy rate`, indicator_name = input$indicator, trust_name = input$trust_name)

      } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "Itself historically" & input$indicator == "General & acute bed occupancy (counts)") {

        trusts_beds %>%
          filter(!str_detect(name, "^CC") & Name == input$trust_name) %>%
          plot_counts(input$indicator, trust_name = input$trust_name)

      } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "Itself historically" & input$indicator == "Beds occupied by long-stay patients (> 7 days)") {

        trusts %>%
          filter(Name == input$trust_name) %>%
          plot_trends(`No. beds occupied by long-stay patients (> 7 days)`, indicator_name = input$indicator, trust_name = input$trust_name, plotting_rates = FALSE)

      } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "Itself historically" & input$indicator == "Beds occupied by long-stay patients (> 21 days)") {

        trusts %>%
          filter(Name == input$trust_name) %>%
          plot_trends(`No. beds occupied by long-stay patients (> 21 days)`, indicator_name = input$indicator, trust_name = input$trust_name, plotting_rates = FALSE)

      # - Comparison with itself this year -
      } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "Other Trusts this year" & input$indicator == "Critical care bed occupancy (rates)") {

        trusts %>%
          plot_trust_comparison_trends(`Critical care beds occupancy rate`, indicator_name = input$indicator, trust_name = input$trust_name)

      } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "Other Trusts this year" & input$indicator == "General & acute bed occupancy (rates)") {

        trusts %>%
          plot_trust_comparison_trends(`Occupancy rate`, indicator_name = input$indicator, trust_name = input$trust_name)

      } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "Other Trusts this year" & input$indicator == "General & acute bed occupancy (counts)") {

        empty_graph

      } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "Other Trusts this year" & input$indicator == "Beds occupied by long-stay patients (> 7 days)") {

        trusts %>%
          plot_trust_comparison_trends(`No. beds occupied by long-stay patients (> 7 days)`, indicator_name = input$indicator, trust_name = input$trust_name, plotting_rates = FALSE)

      } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "Other Trusts this year" & input$indicator == "Beds occupied by long-stay patients (> 21 days)") {

        trusts %>%
          plot_trust_comparison_trends(`No. beds occupied by long-stay patients (> 21 days)`, indicator_name = input$indicator, trust_name = input$trust_name, plotting_rates = FALSE)

      } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "England averages" & input$indicator == "Critical care bed occupancy (rates)") {

        england %>%
          plot_trust_england_comparison_trends(`Critical care beds occupancy rate`, indicator_name = input$indicator, trust_name = input$trust_name)

      } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "England averages" & input$indicator == "General & acute bed occupancy (rates)") {

        england %>%
          plot_trust_england_comparison_trends(`Occupancy rate`, indicator_name = input$indicator, trust_name = input$trust_name)

      } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "England averages" & input$indicator == "General & acute bed occupancy (counts)") {

        empty_graph

      } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "England averages" & input$indicator == "Beds occupied by long-stay patients (> 7 days)") {

        empty_graph

      } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "England averages" & input$indicator == "Beds occupied by long-stay patients (> 21 days)") {

        empty_graph

      } # end if
    })
}

# Run the application
shinyApp(ui = ui, server = server)
