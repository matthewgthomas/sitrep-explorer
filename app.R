library(shiny)
library(arrow)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(shinycssloaders)

eng_2122 = read_feather("data/england-2021-22.feather")
eng_2021 = read_feather("data/england-2020-21.feather")
eng_hist_sum = read_feather("data/england-historical.feather")
trust_2122 = read_feather("data/trusts-2021-22.feather")
trust_2021 = read_feather("data/trusts-2020-21.feather")
trust_hist_sum = read_feather("data/trusts-historical.feather")

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

# ---- UI ----
ui <- fluidPage(

    titlePanel("NHS England winter situation report explorer"),

    sidebarLayout(
        sidebarPanel(
            selectizeInput(
              "indicator",
              "Select an indicator",
              choices = c(
                "Critical care bed occupancy",
                "General & acute bed occupancy (rates)",
                "General & acute bed occupancy (counts)",
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
        # Sort out selected indicator
        if (input$indicator == "Critical care bed occupancy") {
            eng_hist_sum <- eng_hist_sum %>%
                mutate(Indicator = `Median critical care beds occupancy rate`,
                       Indicator_max = `Max critical care beds occupancy rate`,
                       Indicator_min = `Min critical care beds occupancy rate`)

            eng_2122 <- eng_2122 %>%
              mutate(Indicator = `Critical care beds occupancy rate`)

            eng_2021 <- eng_2021 %>%
                mutate(Indicator = `Critical care beds occupancy rate`)

            trust_hist_sum <- trust_hist_sum %>%
                mutate(Indicator = `Median critical care beds occupancy rate`,
                       Indicator_max = `Max critical care beds occupancy rate`,
                       Indicator_min = `Min critical care beds occupancy rate`)

            trust_2122 <- trust_2122 %>%
              mutate(Indicator = `Critical care beds occupancy rate`)

            trust_2021 <- trust_2021 %>%
                mutate(Indicator = `Critical care beds occupancy rate`)

        } else if (input$indicator == "General & acute bed occupancy (rates)") {
            eng_hist_sum <- eng_hist_sum %>%
                mutate(Indicator = `Median occupancy rate`,
                       Indicator_max = `Max occupancy rate`,
                       Indicator_min = `Min occupancy rate`)

            eng_2122 <- eng_2122 %>%
              mutate(Indicator = `Occupancy rate`)

            eng_2021 <- eng_2021 %>%
                mutate(Indicator = `Occupancy rate`)

            trust_hist_sum <- trust_hist_sum %>%
                mutate(Indicator = `Median occupancy rate`,
                       Indicator_max = `Max occupancy rate`,
                       Indicator_min = `Min occupancy rate`)

            trust_2122 <- trust_2122 %>%
              mutate(Indicator = `Occupancy rate`)

            trust_2021 <- trust_2021 %>%
                mutate(Indicator = `Occupancy rate`)

        } else if (input$indicator == "General & acute bed occupancy (counts)") {
          # Nothing to do here - we pre-calculated this data at the top of this script

        } else if (input$indicator == "Beds occupied by long-stay patients (> 21 days)") {
          eng_hist_sum <- eng_hist_sum %>%
            mutate(Indicator = `Median no. beds occupied by long-stay patients (> 21 days)`,
                   Indicator_max = 0,
                   Indicator_min = 0)

          eng_2122 <- eng_2122 %>%
            mutate(Indicator = `No. beds occupied by long-stay patients (> 21 days)`)

          eng_2021 <- eng_2021 %>%
            mutate(Indicator = `No. beds occupied by long-stay patients (> 21 days)`)

          trust_hist_sum <- trust_hist_sum %>%
            mutate(Indicator = `Median no. beds occupied by long-stay patients (> 21 days)`,
                   Indicator_max = NA,
                   Indicator_min = NA)

          trust_2122 <- trust_2122 %>%
            mutate(Indicator = `No. beds occupied by long-stay patients (> 21 days)`)

          trust_2021 <- trust_2021 %>%
            mutate(Indicator = `No. beds occupied by long-stay patients (> 21 days)`)

        }

        # Draw plots
        if (input$eng_or_trusts == "England") {

          if (input$indicator != "General & acute bed occupancy (counts)" & input$indicator != "Beds occupied by long-stay patients (> 21 days)") {

            # Plot line graphs for England
            eng_hist_sum %>%
                ggplot(aes(x = day_of_year, y = Indicator, group = 1)) +
                geom_ribbon(aes(ymin = Indicator_min, ymax = Indicator_max), fill = "grey", alpha = 0.4) +
                geom_line(colour = "grey", lty = 2, size = 1.1) +

                geom_line(data = eng_2021, colour = "black", size = 1.1) +
                geom_line(data = eng_2122, colour = "red", size = 1.1) +

                scale_y_continuous(labels = scales::percent) +
                scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B") +

                labs(title = paste0(input$indicator, " in England"),
                     subtitle = "Red line shows rates in 2021-22; back lines are rates in 2020-21; grey lines show historical average, minimum and maximum rates",
                     x = NULL, y = paste0(input$indicator, " rate"),
                     caption = "Source: BRC/I&I analysis of NHSE data") +
                theme_classic()

          } else if (input$indicator == "Beds occupied by long-stay patients (> 21 days)") {

            eng_hist_sum %>%
              ggplot(aes(x = day_of_year, y = Indicator, group = 1)) +

              geom_line(colour = "grey", size = 1.1) +
              geom_line(data = eng_2021, colour = "black", size = 1.1) +
              geom_line(data = eng_2122, colour = "red", size = 1.1) +

              scale_y_continuous(labels = scales::comma) +
              scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B") +

              labs(
                title = paste0(input$indicator, " in England"),
                subtitle = "Red line shows data for 2021-22; black line is 2020-21; grey line is 2019-20",
                x = NULL,
                y = input$indicator,
                caption = "Source: BRC/I&I analysis of NHSE data"
              ) +
              theme_classic()

          } else if (input$indicator == "General & acute bed occupancy (counts)") {
            # Plot bar graphs for bed occupancy counts
            plt_beds_2122 <-
              beds_eng_2122 %>%
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
              beds_eng_2021 %>%
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
              beds_eng_hist %>%
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

        } else {
            # User wants to look at Trusts
            # What do they want to compare the selected Trust to?
            if (input$trust_comparison == "Itself historically") {

                plt <-
                    ggplot() +

                    scale_y_continuous(labels = scales::percent, limits = c(NA, 1)) +
                    scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B") +

                    labs(title = paste0(input$indicator, " in ", input$trust_name),
                         subtitle = "Red line shows rates in 2021-22; black lines are 2020-21; grey lines show historical average, minimum and maximum rates",
                         x = NULL, y = paste0(input$indicator, " rate"), caption = "Source: BRC/I&I analysis of NHSE data") +
                    theme_classic()

                this_trust_hist <- trust_hist_sum %>%
                    filter(Name == input$trust_name)
                    # filter(Name == trust_2021$Name[1])

                if (nrow(this_trust_hist) > 0) {
                    plt <-
                        plt +
                        geom_ribbon(data = this_trust_hist,
                                    aes(x = day_of_year, y = Indicator,
                                        ymin = Indicator_min, ymax = Indicator_max), fill = "grey", alpha = 0.4) +
                        geom_line(data = this_trust_hist,
                                  aes(x = day_of_year, y = Indicator, group = 1),
                                  colour = "grey", lty = 2, size = 1.1)
                }

                plt +
                    geom_line(data = trust_2021 %>%
                                  # filter(Name == trust_2021$Name[2]),
                                  filter(Name == input$trust_name),
                              aes(x = day_of_year, y = Indicator), colour = "black", size = 1.1) +
                    geom_line(data = trust_2122 %>%
                                  # filter(Name == trust_2021$Name[2]),
                                  filter(Name == input$trust_name),
                              aes(x = day_of_year, y = Indicator), colour = "red", size = 1.1)

            } else if (input$trust_comparison == "Other Trusts this year") {
                trust_2122 %>%
                    ggplot()+

                    geom_line(data = trust_2122 %>% filter(Name != input$trust_name),
                              aes(x = day_of_year, y = Indicator, group = Name),
                              colour = "grey", alpha = 0.7) +

                    geom_line(data = trust_2122 %>% filter(Name == input$trust_name),
                              aes(x = day_of_year, y = Indicator),
                              colour = "red", size = 1.1) +

                    scale_y_continuous(labels = scales::percent) +
                    scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B") +

                    labs(title = paste0(input$indicator, " in ", input$trust_name, " compared to other Trusts"),
                         subtitle = "Red line shows rates for this Trust; grey lines show other Trusts",
                         x = NULL, y = paste0(input$indicator, " rate"), caption = "Source: BRC/I&I analysis of NHSE data") +
                    theme_classic()

            } else if (input$trust_comparison == "England averages") {
                eng_2122 %>%
                    ggplot(aes(x = day_of_year, y = Indicator, group = 1)) +
                    # geom_ribbon(aes(ymin = Indicator_min, ymax = Indicator_max), fill = "grey", alpha = 0.4) +
                    geom_line(colour = "grey", lty = 2, size = 1.1) +

                    geom_line(data = trust_2122 %>% filter(Name == input$trust_name),
                              aes(y = Indicator), colour = "red", size = 1.1) +

                    scale_y_continuous(labels = scales::percent) +
                    scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B") +

                    labs(title = paste0(input$indicator, " in ", input$trust_name, " compared to England as a whole"),
                         subtitle = "Red line shows 2021-22 rates for this Trust; grey line show England rate",
                         x = NULL, y = paste0(input$indicator, " rate"), caption = "Source: BRC/I&I analysis of NHSE data") +
                    theme_classic()

            }

        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
