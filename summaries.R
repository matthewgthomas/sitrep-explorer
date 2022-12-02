summary_UI <- function(id) {
  htmlOutput(NS(id, "nhs_summary"))
}

# indicator_21 = 90184 # beds occupied
# indicator_20 = 77502
# indicator_19 = 91733

summary_server <- function(id, indicator_name, indicator_22, indicator_21, indicator_20, indicator_19, bigger_is_better = FALSE) {
  moduleServer(id, function(input, output, session) {
    output$nhs_summary <- renderText({

      # Calculate differences between 2022-23 and previous years
      diff_22_21 <- indicator_22 - indicator_21
      diff_22_20 <- indicator_22 - indicator_20
      diff_22_19 <- indicator_22 - indicator_19

      # Figure out colours
      # positive_colour <- c(background = "#cce2d8", text = "#005a30")
      # negative_colour <- c(background = "#f6d7d2", text = "#942514")

      colour_21 <- case_when(
        !bigger_is_better & diff_22_21 > 0 ~ "bad",  # e.g. more beds occupied, so make it a negative colour
        !bigger_is_better & diff_22_21 < 0 ~ "good", # e.g. fewer beds occupied, so make it a positive colour
         bigger_is_better & diff_22_21 > 0 ~ "good", # e.g. more beds open, so make it a positive colour
         bigger_is_better & diff_22_21 < 0 ~ "bad",  # e.g. fewer beds open, so make it a negative colour
      )
      # names(colour_20) <- c("background", "text")

      colour_20 <- case_when(
        !bigger_is_better & diff_22_20 > 0 ~ "bad",  # e.g. more beds occupied, so make it a negative colour
        !bigger_is_better & diff_22_20 < 0 ~ "good", # e.g. fewer beds occupied, so make it a positive colour
         bigger_is_better & diff_22_20 > 0 ~ "good", # e.g. more beds open, so make it a positive colour
         bigger_is_better & diff_22_20 < 0 ~ "bad",  # e.g. fewer beds open, so make it a negative colour
      )
      # names(colour_20) <- c("background", "text")

      colour_19 <- case_when(
        !bigger_is_better & diff_22_19 > 0 ~ "bad",  # e.g. more beds occupied, so make it a negative colour
        !bigger_is_better & diff_22_19 < 0 ~ "good", # e.g. fewer beds occupied, so make it a positive colour
         bigger_is_better & diff_22_19 > 0 ~ "good", # e.g. more beds open, so make it a positive colour
         bigger_is_better & diff_22_19 < 0 ~ "bad",  # e.g. fewer beds open, so make it a negative colour
      )
      # names(colour_19) <- c("background", "text")

      # Convert to rates
      if (str_detect(indicator_name, "rate")) {
        # indicator_22_html <- paste0()
      } else {
        indicator_22_html <-
          ifelse(
            is.na(indicator_22) | is.nan(indicator_22),
            "No data for",
            scales::comma(indicator_22)
          )

        diff_22_21_html <-
          ifelse(
            is.na(diff_22_21) | is.nan(diff_22_21),
            "No data for",
            scales::comma(diff_22_21)
          )

        diff_22_20_html <-
          ifelse(
            is.na(diff_22_20) | is.nan(diff_22_20),
            "No data for",
            scales::comma(diff_22_20)
          )

        diff_22_19_html <-
          ifelse(
            is.na(diff_22_19) | is.nan(diff_22_19),
            "No data for",
            scales::comma(diff_22_19)
          )
      }

      title <- paste0("<h4>", indicator_name, "</h4>")

      div_22 <- paste0(
        "<div>",
        "<span style='font-weight:bold; font-size=2rem'>", indicator_22_html, "</span>",
        "</div>"
      )

      div_21 <- paste0(
        "<div class='change-box ", colour_21, "'>",
        "<span>", diff_22_21_html, " ", ifelse(!is.na(diff_22_21) & !is.nan(diff_22_21), ifelse(diff_22_21 > 0, "increase on ", "decrease on "), ""), "same week in 2021-22.</span>",
        "</div>"
      )

      div_20 <- paste0(
        "<div class='change-box ", colour_20, "'>",
        "<span>", diff_22_20_html, " ", ifelse(!is.na(diff_22_20) & !is.nan(diff_22_20), ifelse(diff_22_20 > 0, "increase on ", "decrease on "), ""), "same week in 2020-21.</span>",
        "</div>"
      )

      div_19 <- paste0(
        "<div class='change-box ", colour_19, "'>",
        "<span>", diff_22_19_html, " ", ifelse(!is.na(diff_22_19) & !is.nan(diff_22_19), ifelse(diff_22_19 > 0, "increase on ", "decrease on"), ""), "same week in 2019-20.</span>",
        "</div>"
      )

      # Output HTML
      paste0(title, div_22, div_21, div_20, div_19)

    })
  })
}
