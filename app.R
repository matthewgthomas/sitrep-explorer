library(shiny)
library(arrow)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(shinycssloaders)

source("plot-functions.R")

# ---- Load data ----
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
                "Beds occupied by long-stay patients (> 21 days)",
                "A&E diverts",
                "A&E closures",
                "Ambulance handover delays (30-60 mins)",
                "Ambulance handover delays (more than an hour)"
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
}

# Run the application
shinyApp(ui = ui, server = server)
