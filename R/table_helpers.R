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

#' Format Flextable with TG Styling
#'
#' @description
#' Applies TG Therapeutics formatting to an existing flextable.
#' This is a workaround for flextables converted from gtsummary,
#' which do not inherit default flextable settings.
#'
#' Features:
#' - Removes all borders, then adds horizontal lines in ice blue
#' - Sets Arial font at 12pt (8pt for footer)
#' - Centers vertical alignment
#' - Fixed column width layout
#'
#' @param x A flextable object to format
#' @param border_color Hex color for horizontal border lines.
#'   Defaults to TG ice blue ("#BBCBD3")
#' @param body_size Font size for table body. Defaults to 12
#' @param footer_size Font size for table footer. Defaults to 8
#' @param font_family Font family to use. Defaults to "Arial"
#' @param line_spacing Line spacing multiplier. Defaults to 1
#'
#' @return A formatted flextable object
#'
#' @examples
#' \dontrun{
#' library(flextable)
#' library(gtsummary)
#'
#' # Create a summary table and convert to flextable
#' tbl <- mtcars |>
#'   tbl_summary(include = c(mpg, hp, wt)) |>
#'   as_flex_table()
#'
#' # Apply TG formatting
#' tbl_formatted <- format_flextable(tbl)
#' }
#'
#' @export
format_flextable <- function(x,
                              border_color = "#BBCBD3",
                              body_size = 12,
                              footer_size = 8,
                              font_family = "Arial",
                              line_spacing = 1,
                              border_width = 0.25) {
    # Define border style
    std_border <- officer::fp_border(
        color = border_color,
        style = "solid",
        width = border_width
    )

    # Apply formatting
    x <- flextable::border_remove(x) |>
        flextable::hline(
            part = "all",
            border = std_border
        ) |>
        flextable::fontsize(
            size = body_size,
            part = "all"
        ) |>
        flextable::fontsize(
            size = footer_size,
            part = "footer"
        ) |>
        flextable::font(
            fontname = font_family,
            part = "all"
        ) |>
        flextable::valign(
            valign = "center",
            part = "body"
        ) |>
        flextable::line_spacing(
            space = line_spacing,
            part = "all"
        ) |>
        flextable::set_table_properties(layout = "fixed")

    return(x)
}

#' Export Flextable to Excel with Formatting
#'
#' @description
#' Exports a flextable to an Excel file while preserving background colors.
#' Creates a new workbook, transfers the data, and applies cell-by-cell
#' background coloring to match the flextable's styling.
#'
#' @param table A flextable object to export
#' @param file_name A string specifying the file name (without .xlsx extension).
#' @param overwrite Logical indicating whether to overwrite existing files.
#'   Defaults to TRUE
#'
#' @return Invisibly returns the path to the saved file
#'
#' @examples
#' \dontrun{
#' library(flextable)
#' library(gtsummary)
#'
#' # Create and format a summary table
#' tbl <- mtcars |>
#'   tbl_summary(include = c(mpg, hp, wt)) |>
#'   as_flex_table() |>
#'   format_flextable()
#'
#' # Export to Excel
#' export_flextable_to_excel(tbl, "summary_table")
#' }
#'
#' @export
export_flextable_to_excel <- function(table, file_name, overwrite = TRUE) {
    # Extract data from flextable
    data <- table$body$dataset

    # Extract background colors
    bgcolor <- as.data.frame(table$body$styles$cells$background.color$data)

    # Create workbook and add data
    wb <- openxlsx2::wb_workbook() |>
        openxlsx2::wb_add_worksheet("Sheet1") |>
        openxlsx2::wb_add_data("Sheet1", x = data)

    # Apply background colors cell by cell
    for (col_idx in seq_len(ncol(data))) {
        for (row_idx in seq_len(nrow(data))) {
            cell_bgcolor <- bgcolor[row_idx, col_idx]

            # Convert "transparent" to white
            cell_color <- openxlsx2::wb_color(
                hex = ifelse(
                    cell_bgcolor == "transparent",
                    "#FFFFFF",
                    cell_bgcolor
                )
            )

            # Get Excel cell reference (offset by 1 for header row)
            cell_dims <- openxlsx2::wb_dims(row_idx + 1, col_idx)

            # Apply fill color
            openxlsx2::wb_add_fill(
                wb,
                dims = cell_dims,
                color = cell_color
            )
        }
    }
    file_path <- here::here(output_folder, paste0(file_name, ".xlsx"))
    # Save workbook
    openxlsx2::wb_save(
        wb,
        file = file_path,
        overwrite = overwrite
    )

    invisible(file_path)
}
