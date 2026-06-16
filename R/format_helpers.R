#' Replace NA and Zero Values with Em Dash
#'
#' @description
#' Replaces specific values in summary tables with an em dash (—).
#' Useful for cleaning up gtsummary or other summary table outputs
#' where "0 (0%)", "NA", or "Inf" values should display as dashes.
#'
#' @param x A character vector to process
#'
#' @return A character vector with specified values replaced by em dashes
#'
#' @examples
#' # Replace NA and zero values
#' na_to_dash(c("5 (10%)", "0 (0%)", "NA", "10 (20%)"))
#'
#' @export
na_to_dash <- function(x) {
    dplyr::case_when(
        x == "0 (0%)" ~ "\U2014",
        stringr::str_detect(x, "NA") ~ "\U2014",
        stringr::str_detect(x, "Inf") ~ "\U2014",
        TRUE ~ x
    )
}

#' Format Percentage with Special Cases
#'
#' @description
#' Converts a numeric value (0-1) to a percentage string with special handling
#' for NA, 0, and 100% cases.
#'
#' @param x A numeric value to convert to percentage (typically 0 to 1)
#' @param accuracy Decimal places for label_percent (default: 0.1)
#'
#' @return A character string representing the percentage
#'
#' @examples
#' format_incidence_percent(0.5)
#' format_incidence_percent(0)
#' format_incidence_percent(1)
#' format_incidence_percent(NA_real_)
#'
#' @export
format_incidence_percent <- function(x, accuracy = 0.1) {
    data.table::fcase(
        is.na(x), "0%",
        x == 0, "0%",
        x == 1, "100%",
        default = scales::label_percent(accuracy = accuracy)(x)
    )
}

#' Convert Centimetres to Feet and Inches
#'
#' @description
#' Converts a numeric value (or vector) from centimetres to a formatted
#' feet-and-inches string (e.g. `170` -> `5' 6.9"`).  The function is
#' fully vectorised and handles `NA` values, making it safe to use directly
#' on columns in data frames and `data.table` objects.
#'
#' @param cm A numeric vector of heights in centimetres.
#'
#' @return A character vector of the same length as `cm` formatted as
#'   `<feet>' <inches>"`.  Elements corresponding to `NA` input values
#'   are returned as `NA_character_`.
#'
#' @examples
#' convert_cm_to_ft_in(170)
#' convert_cm_to_ft_in(c(150, 170, 190, NA))
#'
#' @export
convert_cm_to_ft_in <- function(cm) {
  # 1. Calculate total inches
  total_inches <- cm / 2.54

  # 2. Get whole feet
  feet <- total_inches %/% 12

  # 3. Get remaining inches (rounded to 1 decimal place)
  inches <- round(total_inches %% 12, 1)

  # 4. Handle cases where rounding inches up to 12 should increment feet
  inches_round_to_12 <- !is.na(inches) & inches == 12
  feet[inches_round_to_12] <- feet[inches_round_to_12] + 1
  inches[inches_round_to_12] <- 0

  # 5. Format as a character string; restore NAs from input
  result <- paste0(feet, "' ", inches, "\"")
  result[is.na(cm)] <- NA_character_

  return(result)
}