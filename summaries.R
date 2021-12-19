summary_UI <- function(id) {
  htmlOutput(NS(id, "nhs_summary"))
}

# indicator_21 = 90184 # beds occupied
# indicator_20 = 77502
# indicator_19 = 91733

summary_server <- function(id, indicator_name, indicator_21, indicator_20, indicator_19, bigger_is_better = FALSE) {
  moduleServer(id, function(input, output, session) {
    output$nhs_summary <- renderText({

      # Calculate differences between 2021-22 and previous years
      diff_21_20 <- indicator_21 - indicator_20
      diff_21_19 <- indicator_21 - indicator_19

      # Figure out colours
      # positive_colour <- c(background = "#cce2d8", text = "#005a30")
      # negative_colour <- c(background = "#f6d7d2", text = "#942514")

      colour_20 <- case_when(
        !bigger_is_better & diff_21_20 > 0 ~ "bad",  # e.g. more beds occupied, so make it a negative colour
        !bigger_is_better & diff_21_20 < 0 ~ "good", # e.g. fewer beds occupied, so make it a positive colour
         bigger_is_better & diff_21_20 > 0 ~ "good", # e.g. more beds open, so make it a positive colour
         bigger_is_better & diff_21_20 < 0 ~ "bad",  # e.g. fewer beds open, so make it a negative colour
      )
      # names(colour_20) <- c("background", "text")

      colour_19 <- case_when(
        !bigger_is_better & diff_21_19 > 0 ~ "bad",  # e.g. more beds occupied, so make it a negative colour
        !bigger_is_better & diff_21_19 < 0 ~ "good", # e.g. fewer beds occupied, so make it a positive colour
         bigger_is_better & diff_21_19 > 0 ~ "good", # e.g. more beds open, so make it a positive colour
         bigger_is_better & diff_21_19 < 0 ~ "bad",  # e.g. fewer beds open, so make it a negative colour
      )
      # names(colour_19) <- c("background", "text")

      # Convert to rates
      if (str_detect(indicator_name, "rate")) {
        # indicator_21_html <- paste0()
      } else {
        indicator_21_html <- scales::comma(indicator_21)
        diff_21_20_html   <- scales::comma(diff_21_20)
        diff_21_19_html   <- scales::comma(diff_21_19)
      }

      title <- paste0("<h4>", indicator_name, "</h4>")

      div_21 <- paste0(
        "<div>",
        "<span style='font-weight:bold; font-size=2rem'>", indicator_21_html, "</span>",
        "</div>"
      )

      div_20 <- paste0(
        "<div class='change-box ", colour_20, "'>",
        "<span>", diff_21_20_html, " ", ifelse(diff_21_20 > 0, "increase", "decrease"), " on same week in 2020-21.</span>",
        "</div>"
      )

      div_19 <- paste0(
        "<div class='change-box ", colour_19, "'>",
        "<span>", diff_21_19_html, " ", ifelse(diff_21_19 > 0, "increase", "decrease"), " on same week in 2019-20.</span>",
        "</div>"
      )

      # Output HTML
      paste0(title, div_21, div_20, div_19)

    })
  })
}
