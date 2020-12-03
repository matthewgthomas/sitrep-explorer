library(shiny)
library(arrow)
library(tidyverse)

eng_2021 = read_feather("data/england-2020-21.feather")
eng_hist_sum = read_feather("data/england-historical.feather")
trust_2021 = read_feather("data/trusts-2020-21.feather")
trust_hist_sum = read_feather("data/trusts-historical.feather")

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("NHS England winter situation report explorer"),

    sidebarLayout(
        sidebarPanel(
            radioButtons("eng_or_trusts", "Show data for England or individual Trusts?", choices = c("England", "Trusts")),

            selectInput("trust_name", "Select a Trust", sort(trust_2021$Name), multiple = FALSE),

            radioButtons("trust_comparison", "Compare this Trust to...", choices = c("Itself historically", "Other Trusts this year", "England averages"))
        ),

        mainPanel(
           plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot <- renderPlot({
        if (input$eng_or_trusts == "England") {
            # Plot bed occupancy trends for England
            eng_hist_sum %>%
                ggplot(aes(x = day_of_year, y = `Median occupancy rate`, group = 1)) +
                geom_ribbon(aes(ymin = `Min occupancy rate`, ymax = `Max occupancy rate`), fill = "grey", alpha = 0.4) +
                geom_line(colour = "grey", lty = 2, size = 1.1) +

                geom_line(data = eng_2021, aes(y = `Occupancy rate`), colour = "red", size = 1.1) +

                scale_y_continuous(labels = scales::percent) +
                scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B") +

                labs(title = "Bed occupancy rates in England",
                     subtitle = "Red line shows rates in 2020-21; grey lines show historical average, minimum and maximum rates",
                     x = NULL, y = "Bed occupancy rate",
                     caption = "Source: BRC/I&I analysis of NHSE data") +
                theme_classic()

        } else {
            # User wants to look at Trusts
            # What do they want to compare the selected Trust to?
            if (input$trust_comparison == "Itself historically") {
                trust_hist_sum %>%
                    filter(Name == input$trust_name) %>%

                    ggplot(aes(x = day_of_year, y = `Median occupancy rate`, group = 1)) +
                    geom_ribbon(aes(ymin = `Min occupancy rate`, ymax = `Max occupancy rate`), fill = "grey", alpha = 0.4) +
                    geom_line(colour = "grey", lty = 2, size = 1.1) +

                    geom_line(data = trust_2021 %>% filter(Name == input$trust_name),
                              aes(y = `Occupancy rate`), colour = "red", size = 1.1) +

                    scale_y_continuous(labels = scales::percent) +
                    scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B") +

                    labs(title = paste0("Current and historical bed occupancy rates in ", input$trust_name),
                         subtitle = "Red line shows rates in 2020-21; grey lines show historical average, minimum and maximum rates",
                         x = NULL, y = "Bed occupancy rate", caption = "Source: BRC/I&I analysis of NHSE data") +
                    theme_classic()

            } else if (input$trust_comparison == "Other Trusts this year") {
                trust_2021 %>%
                    ggplot()+

                    geom_line(data = trust_2021 %>% filter(Name != input$trust_name),
                              aes(x = day_of_year, y = `Occupancy rate`, group = Name),
                              colour = "grey", alpha = 0.7) +

                    geom_line(data = trust_2021 %>% filter(Name == input$trust_name),
                              aes(x = day_of_year, y = `Occupancy rate`),
                              colour = "red", size = 1.1) +

                    scale_y_continuous(labels = scales::percent) +
                    scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B") +

                    labs(title = paste0("Bed occupancy rates in ", input$trust_name, " compared to other Trusts"),
                         subtitle = paste0("Red line shows rates in ", input$trust_name, "; grey lines show other Trusts"),
                         x = NULL, y = "Bed occupancy rate", caption = "Source: BRC/I&I analysis of NHSE data") +
                    theme_classic()

            } else if (input$trust_comparison == "England averages") {
                eng_hist_sum %>%
                    ggplot(aes(x = day_of_year, y = `Median occupancy rate`, group = 1)) +
                    geom_ribbon(aes(ymin = `Min occupancy rate`, ymax = `Max occupancy rate`), fill = "grey", alpha = 0.4) +
                    geom_line(colour = "grey", lty = 2, size = 1.1) +

                    geom_line(data = trust_2021 %>% filter(Name == input$trust_name),
                              aes(y = `Occupancy rate`), colour = "red", size = 1.1) +

                    scale_y_continuous(labels = scales::percent) +
                    scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B") +

                    labs(title = paste0("Bed occupancy rates in ", input$trust_name, " compared to England as a whole"),
                         subtitle = "Red line shows rates in 2020-21; grey lines show historical average, minimum and maximum rates",
                         x = NULL, y = "Bed occupancy rate", caption = "Source: BRC/I&I analysis of NHSE data") +
                    theme_classic()

            }

        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
