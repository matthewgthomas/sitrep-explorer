library(shiny)
library(arrow)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(leaflet)
library(lubridate)
library(shinycssloaders)

source("plot-functions.R")
source("summaries.R")

# ---- Load data ----
england <- read_feather("data/england.feather")
england_beds <- read_feather("data/england-beds.feather")
england_summary <- read_feather("data/england-summary.feather")

trusts <- read_feather("data/trusts.feather")
trusts_beds <- read_feather("data/trusts-beds.feather")

trusts_geocoded <- read_rds("data/trusts-geocoded.rds")

trust_names <-
  trusts %>%
  filter(year == "2022-23") %>%
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
      year == "2022-23" ~ 1,
      year == "2021-22" ~ 1,
      TRUE ~ 0.9
    ),
    colour = case_when(
      year == "2022-23" ~ "red",
      year == "2021-22" ~ "black",
      TRUE ~ "grey"
    )
  )

trusts <-
  trusts %>%
  mutate(`Occupancy rate` = `G&A beds occ'd` / `G&A Beds Open`) %>%
  mutate(
    opacity = case_when(
      year == "2022-23" ~ 1,
      year == "2021-22" ~ 1,
      TRUE ~ 0.9
    ),
    colour = case_when(
      year == "2022-23" ~ "red",
      year == "2021-22" ~ "black",
      TRUE ~ "grey"
    )
  )

# ---- Data wrangling for summary indicators ----
# Does this winter's sitrep include data from after January?
contains_new_year_data <- FALSE

# Get most recent week number
this_week <-
  england_summary %>%
  filter(year == "2022-23") %>%
  {if (contains_new_year_data) filter(., year == "2022-23" & week < 20) else filter(., year == "2022-23") } %>%
  filter(week == max(week)) %>%
  distinct(week) %>%
  pull(week)

# this_week <- 9

# Recast summary for this week so rows are indicators and columns are years
this_week_summary <-
  england_summary %>%
  filter(week == this_week) %>%
  select(-week) %>%
  pivot_longer(cols = -year) %>%
  pivot_wider(names_from = year, values_from = value)

# Not every week is present in each year's dataset, so add them in if mising
if (!"2022-23" %in% names(this_week_summary)) this_week_summary$`2022-23` = NA
if (!"2021-22" %in% names(this_week_summary)) this_week_summary$`2021-22` = NA
if (!"2020-21" %in% names(this_week_summary)) this_week_summary$`2020-21` = NA
if (!"2019-20" %in% names(this_week_summary)) this_week_summary$`2019-20` = NA

# this_week_summary_trusts <-
#   trusts_summary %>%
#   filter(week == this_week) %>%
#   select(-week) %>%
#   pivot_longer(cols = -c(year, Name)) %>%
#   pivot_wider(names_from = year, values_from = value)
#
# this_week_summary <-
#   bind_rows(
#     this_week_summary %>% mutate(Name = "England"),
#     this_week_summary_trusts
#   )

# ---- UI ----
ui <- fluidPage(

    # CSS Styles
    tags$head(
      tags$style(
        HTML("
        #card {
            box-shadow: 0px 0px 3px grey;
            border-radius: 5px;
            padding: 10px 20px 10px 20px;
            margin: 0px 0px 20px 0px;
        }

        .change-box {
          padding-top: 5px;
          padding-right: 8px;
          padding-bottom: 4px;
          padding-left: 8px;
        }

        .change-box.good {
          background: #cce2d8;
          color: #005a30;
        }

        .change-box.bad {
          background: #f6d7d2;
          color: #942514;
        }
        ")
      )
    ),

    titlePanel("NHS England winter situation report explorer"),

    sidebarLayout(
        sidebarPanel(
          radioButtons(
            "summary_map_trends",
            "",
            choices = c("Summary", "Map", "Trends")
          ),

          conditionalPanel(
            condition = "input.summary_map_trends != 'Summary'",

            selectizeInput(
              "indicator",
              "Select an indicator",
              choices = c(
                "Critical care bed occupancy (rates)",
                "Critical care bed occupancy (counts)",
                "General & acute bed occupancy (rates)",
                "General & acute bed occupancy (counts)",
                "Beds occupied by long-stay patients (> 7 days)",
                "Beds occupied by long-stay patients (> 21 days)",
                "A&E diverts",
                "A&E closures",
                "Ambulance handover delays (30-60 mins)",
                "Ambulance handover delays (more than an hour)"
              )
            ),

            conditionalPanel(
              condition = "input.summary_map_trends == 'Trends'",

              radioButtons("eng_or_trusts", "Show data for England or for individual Trusts?", choices = c("England", "Trusts"))
            ),

            conditionalPanel(
              condition = "input.eng_or_trusts == 'Trusts'",

              selectizeInput("trust_name", "Select a Trust", trust_names, selected = trust_names[1], multiple = FALSE),

              radioButtons("trust_comparison", "Compare this Trust to...", choices = c("Itself historically", "Other Trusts this year", "England averages"))
            )
          )
        ),

        mainPanel(
          conditionalPanel(
            condition = "input.summary_map_trends == 'Summary'",

            # Show summary page
            fluidRow(
              htmlOutput("summary_title"),

              column(
                id = "beds_occ_box",
                width = 6,
                align = "center",
                tags$div(
                  id = "card",
                  summary_UI("summary_beds_occ")
                )
              ),

              column(
                id = "beds_open_box",
                width = 6,
                align = "center",
                tags$div(
                  id = "card",
                  summary_UI("summary_beds_open")
                )
              ),

              column(
                id = "cc_occ_box",
                width = 6,
                align = "center",
                tags$div(
                  id = "card",
                  summary_UI("summary_cc_occ")
                )
              ),

              column(
                id = "cc_open_box",
                width = 6,
                align = "center",
                tags$div(
                  id = "card",
                  summary_UI("summary_cc_open")
                )
              ),

              column(
                id = "beds_long_7_box",
                width = 6,
                align = "center",
                tags$div(
                  id = "card",
                  summary_UI("summary_long_7")
                )
              ),

              column(
                id = "beds_long_21_box",
                width = 6,
                align = "center",
                tags$div(
                  id = "card",
                  summary_UI("summary_long_21")
                )
              ),

              column(
                id = "diverts_box",
                width = 6,
                align = "center",
                tags$div(
                  id = "card",
                  summary_UI("summary_diverts")
                )
              ),

              column(
                id = "closures_box",
                width = 6,
                align = "center",
                tags$div(
                  id = "card",
                  summary_UI("summary_closures")
                )
              ),

              column(
                id = "delays_30_box",
                width = 6,
                align = "center",
                tags$div(
                  id = "card",
                  summary_UI("summary_delays_30")
                )
              ),

              column(
                id = "delays_60_box",
                width = 6,
                align = "center",
                tags$div(
                  id = "card",
                  summary_UI("summary_delays_60")
                )
              )
            ),
          ),

          # Show map
          conditionalPanel(
            condition = "input.summary_map_trends == 'Map'",

            shinycssloaders::withSpinner(leafletOutput("map", width = "100%", height = "600px"), color = "red")
          ),

          # Show trend graphs
          conditionalPanel(
            condition = "input.summary_map_trends == 'Trends'",

            shinycssloaders::withSpinner(plotOutput("plot"), color = "red")
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # ---- Map ----
  output$map <-
    renderLeaflet({
      # Get Trust-level data for current date
      # This will be implemented later, so for now just take most recent values
      curr_trust_data <-
        trusts_geocoded |>
        filter(Date == ymd("2022-03-08"))

      # Fetch data for selected indicator
      map_data <-
        curr_trust_data |>

        mutate(indicator = case_when(
          input$indicator == "Critical care bed occupancy (rates)" ~ `Critical care beds occupancy rate`,

          input$indicator == "Critical care bed occupancy (counts)" ~ `CC Adult Occ`,

          input$indicator == "General & acute bed occupancy (rates)" ~ `Occupancy rate`,

          input$indicator == "General & acute bed occupancy (counts)" ~ `G&A beds occ'd`,

          input$indicator == "Beds occupied by long-stay patients (> 7 days)" ~ `No. beds occupied by long-stay patients (> 7 days)`,

          input$indicator == "Beds occupied by long-stay patients (> 21 days)" ~ `No. beds occupied by long-stay patients (> 21 days)`,

          input$indicator == "A&E diverts" ~ Diverts,

          input$indicator == "A&E closures" ~ Closures,

          input$indicator == "Ambulance handover delays (30-60 mins)" ~ Delays30,

          input$indicator == "Ambulance handover delays (more than an hour)" ~ Delays60,

          # Default to critical care bed occupancy rate
          TRUE ~ `Critical care beds occupancy rate`
        )) |>

        mutate(indicator_formatted = ifelse(str_detect(input$indicator, "rates"), scales::percent(indicator), scales::comma(indicator))) |>

        select(nhs_trust22_code, nhs_trust22_name, indicator, indicator_formatted)

      # Create colour palette, depending on the selected indicator
      pal <- colorNumeric(
        palette = "Reds",
        domain = map_data$indicator
      )

      # Create friendly labels if the selected indicator is a percentage
      indicator_labels <- labelFormat(
        digits = 0,
        suffix = ifelse(str_detect(input$indicator, "rates"), "%", ""),
        transform = ifelse(str_detect(input$indicator, "rates"), function(x) x * 100, identity)
      )

      leaflet(
        data = map_data,
        options = leafletOptions(zoomControl = FALSE)
      ) |>
        setView(lat = 54, lng = -2.0, zoom = 7) |>
        addProviderTiles(
          providers$CartoDB.Positron,
          options = providerTileOptions(minZoom = 7)
        ) |>
        setMaxBounds(-12, 49, 3.0, 61) |>
        htmlwidgets::onRender(
          "function(el, x) {
            L.control.zoom({position:'bottomleft'}).addTo(this);
             }"
        ) |>
        addCircleMarkers(
          # data = map_data,
          fillColor = ~pal(indicator),
          radius = 6,
          fillOpacity = 1,
          stroke = T,
          weight = 2,
          color = "black",
          label = ~nhs_trust22_name
          # popup = ~paste(
          #   input$indicator, ": ", indicator_formatted
          # ),
          # popupOptions = popupOptions(maxWidth = 600, minWidth = 500)
        ) |>
        addLegend(
          "topright",
          pal = pal,
          na.label = "No value",
          values = ~indicator,
          labFormat = indicator_labels,
          title = str_wrap(input$indicator, 20),
          opacity = 1
        )
    })

  # ---- Trends ----
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

    } else if (input$eng_or_trusts == "England" & input$indicator == "A&E diverts") {

      england %>%
        plot_trends(Diverts, indicator_name = input$indicator, plotting_rates = FALSE)

    } else if (input$eng_or_trusts == "England" & input$indicator == "A&E closures") {

      england %>%
        plot_trends(Closures, indicator_name = input$indicator, plotting_rates = FALSE)

    } else if (input$eng_or_trusts == "England" & input$indicator == "Ambulance handover delays (30-60 mins)") {

      england %>%
        plot_trends(Delays30, indicator_name = input$indicator, plotting_rates = FALSE)

    } else if (input$eng_or_trusts == "England" & input$indicator == "Ambulance handover delays (more than an hour)") {

      england %>%
        plot_trends(Delays60, indicator_name = input$indicator, plotting_rates = FALSE)

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

    } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "Itself historically" & input$indicator == "A&E diverts") {

      trusts %>%
        filter(Name == input$trust_name) %>%
        plot_trends(Diverts, indicator_name = input$indicator, trust_name = input$trust_name, plotting_rates = FALSE)

    } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "Itself historically" & input$indicator == "A&E closures") {

      trusts %>%
        filter(Name == input$trust_name) %>%
        plot_trends(Closures, indicator_name = input$indicator, trust_name = input$trust_name, plotting_rates = FALSE)

    } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "Itself historically" & input$indicator == "Ambulance handover delays (30-60 mins)") {

      trusts %>%
        filter(Name == input$trust_name) %>%
        plot_trends(Delays30, indicator_name = input$indicator, trust_name = input$trust_name, plotting_rates = FALSE)

    } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "Itself historically" & input$indicator == "Ambulance handover delays (more than an hour)") {

      trusts %>%
        filter(Name == input$trust_name) %>%
        plot_trends(Delays60, indicator_name = input$indicator, trust_name = input$trust_name, plotting_rates = FALSE)

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

    } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "Other Trusts this year" & input$indicator == "A&E diverts") {

      trusts %>%
        plot_trust_comparison_trends(Diverts, indicator_name = input$indicator, trust_name = input$trust_name, plotting_rates = FALSE)

    } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "Other Trusts this year" & input$indicator == "A&E closures") {

      trusts %>%
        plot_trust_comparison_trends(Closures, indicator_name = input$indicator, trust_name = input$trust_name, plotting_rates = FALSE)

    } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "Other Trusts this year" & input$indicator == "Ambulance handover delays (30-60 mins)") {

      trusts %>%
        plot_trust_comparison_trends(Delays30, indicator_name = input$indicator, trust_name = input$trust_name, plotting_rates = FALSE)

    } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "Other Trusts this year" & input$indicator == "Ambulance handover delays (more than an hour)") {

      trusts %>%
        plot_trust_comparison_trends(Delays60, indicator_name = input$indicator, trust_name = input$trust_name, plotting_rates = FALSE)

    # - Comparison with England -
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

    } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "England averages" & input$indicator == "A&E diverts") {

      empty_graph

    } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "England averages" & input$indicator == "A&E closures") {

      empty_graph

    } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "England averages" & input$indicator == "Ambulance handover delays (30-60 mins)") {

      empty_graph

    } else if (input$eng_or_trusts == "Trusts" & input$trust_comparison == "England averages" & input$indicator == "Ambulance handover delays (more than an hour)") {

      empty_graph

    } # end if
  })

  # ---- Summaries ----
  output$summary_title <- renderText({
    # place <- ifelse(input$eng_or_trusts == "England", "England", input$trust_name)  # <-- one for the future
    place <- "England"

    paste0(
      "<h3>Summary for ", place, " in week ", this_week, "</h3>"
    )
  })

  summary_server(
    id = "summary_beds_occ",
    indicator_name = "General & Acute beds occupied",
    indicator_22 = this_week_summary %>% filter(name == "G&A beds occ'd") %>% pull(`2022-23`),
    indicator_21 = this_week_summary %>% filter(name == "G&A beds occ'd") %>% pull(`2021-22`),
    indicator_20 = this_week_summary %>% filter(name == "G&A beds occ'd") %>% pull(`2020-21`),
    indicator_19 = this_week_summary %>% filter(name == "G&A beds occ'd") %>% pull(`2019-20`),
  )

  summary_server(
    id = "summary_beds_open",
    indicator_name = "General & Acute beds open",
    indicator_22 = this_week_summary %>% filter(name == "G&A Beds Open") %>% pull(`2022-23`),
    indicator_21 = this_week_summary %>% filter(name == "G&A Beds Open") %>% pull(`2021-22`),
    indicator_20 = this_week_summary %>% filter(name == "G&A Beds Open") %>% pull(`2020-21`),
    indicator_19 = this_week_summary %>% filter(name == "G&A Beds Open") %>% pull(`2019-20`),
    bigger_is_better = TRUE
  )

  summary_server(
    id = "summary_cc_occ",
    indicator_name = "Adult critical care beds occupied",
    indicator_22 = this_week_summary %>% filter(name == "CC Adult Occ") %>% pull(`2022-23`),
    indicator_21 = this_week_summary %>% filter(name == "CC Adult Occ") %>% pull(`2021-22`),
    indicator_20 = this_week_summary %>% filter(name == "CC Adult Occ") %>% pull(`2020-21`),
    indicator_19 = this_week_summary %>% filter(name == "CC Adult Occ") %>% pull(`2019-20`),
  )

  summary_server(
    id = "summary_cc_open",
    indicator_name = "Adult critical care beds open",
    indicator_22 = this_week_summary %>% filter(name == "CC Adult Open") %>% pull(`2022-23`),
    indicator_21 = this_week_summary %>% filter(name == "CC Adult Open") %>% pull(`2021-22`),
    indicator_20 = this_week_summary %>% filter(name == "CC Adult Open") %>% pull(`2020-21`),
    indicator_19 = this_week_summary %>% filter(name == "CC Adult Open") %>% pull(`2019-20`),
    bigger_is_better = TRUE
  )

  summary_server(
    id = "summary_long_7",
    indicator_name = "Beds occupied by long-stay patients (> 7 days)",
    indicator_22 = this_week_summary %>% filter(name == "No. beds occupied by long-stay patients (> 7 days)") %>% pull(`2022-23`),
    indicator_21 = this_week_summary %>% filter(name == "No. beds occupied by long-stay patients (> 7 days)") %>% pull(`2021-22`),
    indicator_20 = this_week_summary %>% filter(name == "No. beds occupied by long-stay patients (> 7 days)") %>% pull(`2020-21`),
    indicator_19 = this_week_summary %>% filter(name == "No. beds occupied by long-stay patients (> 7 days)") %>% pull(`2019-20`)
  )

  summary_server(
    id = "summary_long_21",
    indicator_name = "Beds occupied by long-stay patients (> 21 days)",
    indicator_22 = this_week_summary %>% filter(name == "No. beds occupied by long-stay patients (> 21 days)") %>% pull(`2022-23`),
    indicator_21 = this_week_summary %>% filter(name == "No. beds occupied by long-stay patients (> 21 days)") %>% pull(`2021-22`),
    indicator_20 = this_week_summary %>% filter(name == "No. beds occupied by long-stay patients (> 21 days)") %>% pull(`2020-21`),
    indicator_19 = this_week_summary %>% filter(name == "No. beds occupied by long-stay patients (> 21 days)") %>% pull(`2019-20`)
  )

  summary_server(
    id = "summary_diverts",
    indicator_name = "A&E diverts",
    indicator_22 = this_week_summary %>% filter(name == "Diverts") %>% pull(`2022-23`),
    indicator_21 = this_week_summary %>% filter(name == "Diverts") %>% pull(`2021-22`),
    indicator_20 = this_week_summary %>% filter(name == "Diverts") %>% pull(`2020-21`),
    indicator_19 = this_week_summary %>% filter(name == "Diverts") %>% pull(`2019-20`)
  )

  summary_server(
    id = "summary_closures",
    indicator_name = "A&E closures",
    indicator_22 = this_week_summary %>% filter(name == "Closures") %>% pull(`2022-23`),
    indicator_21 = this_week_summary %>% filter(name == "Closures") %>% pull(`2021-22`),
    indicator_20 = this_week_summary %>% filter(name == "Closures") %>% pull(`2020-21`),
    indicator_19 = this_week_summary %>% filter(name == "Closures") %>% pull(`2019-20`)
  )

  summary_server(
    id = "summary_delays_30",
    indicator_name = "Ambulance handover delays (30-60 mins)",
    indicator_22 = this_week_summary %>% filter(name == "Delays30") %>% pull(`2022-23`),
    indicator_21 = this_week_summary %>% filter(name == "Delays30") %>% pull(`2021-22`),
    indicator_20 = this_week_summary %>% filter(name == "Delays30") %>% pull(`2020-21`),
    indicator_19 = this_week_summary %>% filter(name == "Delays30") %>% pull(`2019-20`)
  )

  summary_server(
    id = "summary_delays_60",
    indicator_name = "Ambulance handover delays (60+ mins)",
    indicator_22 = this_week_summary %>% filter(name == "Delays60") %>% pull(`2022-23`),
    indicator_21 = this_week_summary %>% filter(name == "Delays60") %>% pull(`2021-22`),
    indicator_20 = this_week_summary %>% filter(name == "Delays60") %>% pull(`2020-21`),
    indicator_19 = this_week_summary %>% filter(name == "Delays60") %>% pull(`2019-20`)
  )
}

# Run the application
shinyApp(ui = ui, server = server)
