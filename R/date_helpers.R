#' Get Last Day of Month
#'
#' @description
#' Returns the last day of the month for a given date.
#'
#' @param date A Date or IDate object.
#'
#' @return An IDate representing the last day of the month.
#'
#' @examples
#' last_day(as.Date("2024-02-15"))
#' # Returns 2024-02-29 (leap year)
#'
#' last_day(as.Date("2023-02-15"))
#' # Returns 2023-02-28
#'
#' @export
last_day <- function(date) {
    lubridate::ceiling_date(date, "month") - lubridate::days(1)
}

#' Complete Partial Start Dates
#'
#' @description
#' Builds complete start dates from partial date components (day, month, year).
#' When components are unknown ("UNK" or "UN"), imputes the earliest possible
#' date: day defaults to "01", month defaults to "JAN".
#'
#' Useful for clinical trial data where partial dates are common and you need
#' to impute the earliest possible date for start dates.
#'
#' @param day Character or numeric day value. "UNK" or "UN" values are replaced with "01".
#' @param month Character or numeric month value. "UNK" or "UN" values are replaced with "JAN".
#'   Accepts numeric months ("01"-"12") or abbreviations ("JAN"-"DEC").
#' @param year Character or numeric year value. Non-numeric values result in NA.
#'
#' @return An IDate object, or NA if year is invalid.
#'
#' @examples
#' \dontrun{
#' # Complete date
#' complete_start_date("15", "03", "2024")
#'
#' # Unknown day - defaults to 1st

#' complete_start_date("UNK", "03", "2024")
#'
#' # Unknown month - defaults to January
#' complete_start_date("UNK", "UNK", "2024")
#' }
#'
#' @export
complete_start_date <- function(day, month, year) {
    # converts inputs to character
    day_c <- stringr::str_to_upper(as.character(day))
    month_c <- stringr::str_to_upper(as.character(month))
    year_c <- stringr::str_to_upper(as.character(year))

    # replaces "UNK" with "01" for day
    day_c <- data.table::fifelse(stringr::str_detect(day_c, "UN"), "01", day_c)

    # replaces "UNK" with "JAN" for day and month
    # uses fcase to ensure month is in %b format
    month_c <- data.table::fcase(
        stringr::str_detect(month_c, "UNK"), "JAN",
        month_c == "UN", "JAN",
        month_c == "01", "JAN",
        month_c == "02", "FEB",
        month_c == "03", "MAR",
        month_c == "04", "APR",
        month_c == "05", "MAY",
        month_c == "06", "JUN",
        month_c == "07", "JUL",
        month_c == "08", "AUG",
        month_c == "09", "SEP",
        month_c == "10", "OCT",
        month_c == "11", "NOV",
        month_c == "12", "DEC",
        !is.na(month_c), month_c
    )

    # year is free text so replacing non-numeric values with "NA" for year
    year_c <- data.table::fifelse(stringr::str_detect(year_c, "[^0-9]"), "NA", year_c)

    # date as a character string
    date_c <- paste0(as.character(year_c), "-", month_c, "-", day_c)

    # converts to date class
    data.table::fifelse(
        stringr::str_detect(date_c, "NA"),
        NA,
        data.table::as.IDate(date_c, format = "%Y-%b-%d")
    )
}

#' Complete Partial End Dates
#'
#' @description
#' Builds complete end dates from partial date components (day, month, year).
#' When components are unknown ("UNK" or "UN"), imputes the latest possible
#' date: day defaults to last day of month, month defaults to "DEC".
#'
#' Useful for clinical trial data where partial dates are common and you need
#' to impute the latest possible date for end dates.
#'
#' @param day Character or numeric day value. "UNK" or "UN" values are replaced
#'   with the last day of the month.
#' @param month Character or numeric month value. "UNK" or "UN" values are replaced with "DEC".
#'   Accepts numeric months ("01"-"12") or abbreviations ("JAN"-"DEC").
#' @param year Character or numeric year value. Non-numeric values result in NA.
#'
#' @return An IDate object, or NA if year is invalid.
#'
#' @examples
#' \dontrun{
#' # Complete date
#' complete_end_date("15", "03", "2024")
#'
#' # Unknown day - defaults to last day of month
#' complete_end_date("UNK", "02", "2024")
#' # Returns 2024-02-29
#'
#' # Unknown month - defaults to December 31st
#' complete_end_date("UNK", "UNK", "2024")
#' # Returns 2024-12-31
#' }
#'
#' @export
complete_end_date <- function(day, month, year) {
    # converts inputs to character
    day_c <- stringr::str_to_upper(as.character(day))
    month_c <- stringr::str_to_upper(as.character(month))
    year_c <- stringr::str_to_upper(as.character(year))

    # replaces "UNK" with "28" for day (placeholder, will be adjusted)
    day_c <- data.table::fifelse(stringr::str_detect(day_c, "UN"), "28", day_c)

    # replaces "UNK" with "DEC" for month
    month_c <- data.table::fcase(
        stringr::str_detect(month_c, "UNK"), "DEC",
        month_c == "UN", "DEC",
        month_c == "01", "JAN",
        month_c == "02", "FEB",
        month_c == "03", "MAR",
        month_c == "04", "APR",
        month_c == "05", "MAY",
        month_c == "06", "JUN",
        month_c == "07", "JUL",
        month_c == "08", "AUG",
        month_c == "09", "SEP",
        month_c == "10", "OCT",
        month_c == "11", "NOV",
        month_c == "12", "DEC",
        !is.na(month_c), month_c
    )

    # year is free text so replacing non-numeric values with "NA" for year
    year_c <- data.table::fifelse(stringr::str_detect(year_c, "[^0-9]"), "NA", year_c)

    # date as a character string
    date_c <- paste0(as.character(year_c), "-", month_c, "-", day_c)

    # converts to date class
    date_int <- data.table::fifelse(
        stringr::str_detect(date_c, "NA"),
        NA,
        base::as.Date(date_c, format = "%Y-%b-%d")
    )

    # converts from 28th to actual last day but only if day is unk
    data.table::as.IDate(
        data.table::fifelse(day == "UNK", last_day(date_int), date_int)
    )
}

#' Convert Days to Months
#'
#' @description
#' Converts a number of days to months using 30.4375 days per month
#' (average days per month accounting for leap years: 365.25/12).
#'
#' @param days Numeric vector of days to convert.
#'
#' @return Numeric vector of months, rounded to 2 decimal places.
#'
#' @examples
#' days_to_months(30)
#' # Returns 0.99
#'
#' days_to_months(365)
#' # Returns 11.99
#'
#' @export
days_to_months <- function(days) {
    round(days / 30.4375, 2)
}

#' Convert Days to Years
#'
#' @description
#' Converts a number of days to years using 365.25 days per year
#' (accounting for leap years).
#'
#' @param days Numeric vector of days to convert.
#'
#' @return Numeric vector of years, rounded to 2 decimal places.
#'
#' @examples
#' days_to_years(365)
#' # Returns 1.00
#'
#' days_to_years(730)
#' # Returns 2.00
#'
#' @export
days_to_years <- function(days) {
    round(days / 365.25, 2)
}

#' Convert HH:MM Duration to Minutes
#'
#' @description
#' Converts ITime durations in HH:MM format to total minutes as a numeric value.
#' Useful for calculating duration statistics from time objects.
#'
#' @param hhmm An ITime object representing a duration in HH:MM format.
#'
#' @return Numeric value representing total minutes.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' hhmm_to_min(as.ITime("01:30"))
#' # Returns 90
#'
#' hhmm_to_min(as.ITime("00:45"))
#' # Returns 45
#' }
#'
#' @export
hhmm_to_min <- function(hhmm) {
    lubridate::hour(hhmm) * 60 + lubridate::minute(hhmm)
}

#' Get Latest File-Modified Date in a Folder Tree
#'
#' @description
#' Scans all files recursively within a folder and returns the most recently
#' modified file's date as an `IDate`. Useful for inferring data pull dates
#' from source-file directories.
#'
#' @param folder_path Character string. Path to the folder to scan.
#'
#' @return An `IDate` representing the most recent file modification date,
#'   or `NA` (as `IDate`) if the folder contains no files.
#'
#' @examples
#' \dontrun{
#' get_latest_folder_date(here::here("source-files", "01_edc_data"))
#' }
#'
#' @export
get_latest_folder_date <- function(folder_path) {
    files <- list.files(folder_path, recursive = TRUE, full.names = TRUE)

    if (length(files) == 0L) {
        return(as.IDate(NA))
    }

    file.mtime(files) |>
        max(na.rm = TRUE) |>
        as.IDate()
}
