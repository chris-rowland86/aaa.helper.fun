#' Convert ggplot to Editable RVG Graphic
#'
#' @description
#' Converts a ggplot object to an editable rvg (R Vector Graphics) object
#' that can be inserted into PowerPoint presentations. The resulting graphic
#' can be edited directly in PowerPoint.
#'
#' @param plot A ggplot object to convert
#' @param bg Background color. Defaults to "NA" (transparent)
#' @param fonts Named list mapping font families. Defaults to Arial for sans.
#' @param editable Logical indicating if the graphic should be editable.
#'   Defaults to TRUE.
#'
#' @return An rvg dml object
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' p <- ggplot(mtcars, aes(x = mpg, y = hp)) +
#'     geom_point()
#'
#' rvg_plot <- convert_to_rvg(p)
#' }
#'
#' @export
convert_to_rvg <- function(plot, bg = "NA", fonts = list(sans = "Arial"), editable = TRUE) {
    rvg::dml(
        ggobj = plot,
        bg = bg,
        fonts = fonts,
        editable = editable
    )
}

#' Add RVG Graphic to PowerPoint Slide
#'
#' @description
#' Adds an rvg graphic to a PowerPoint presentation slide. This function
#' adds a new slide with the specified layout and inserts the graphic
#' along with a formatted title.
#'
#' @param pptx An officer pptx object (the PowerPoint presentation)
#' @param rvg_graphic An rvg dml object created by `convert_to_rvg()`
#' @param title Character string for the slide title. Defaults to "Title"
#' @param title_lines Character indicating title layout: "one" for single line,
#'   "two" for two-line title layout. Defaults to "one"
#' @param top Numeric specifying the top position of the figure in inches.
#'   Defaults to 1.5
#' @param left Numeric specifying the left position of the figure in inches.
#'   Defaults to 0.3
#' @param fig_width Numeric specifying the figure width in inches.
#'   Defaults to 10
#' @param fig_height Numeric specifying the figure height in inches.
#'   Defaults to 5
#' @param layout_one Character string for single-line title layout name.
#'   Defaults to "Title and Content_No Graphic"
#' @param layout_two Character string for two-line title layout name.
#'   Defaults to "Title Two Line and Content_No Graphic"
#' @param master Character string for slide master name.
#'   Defaults to "Office Theme"
#' @param title_color Hex color code for title text.
#'   Defaults to TG royal blue ("#003494")
#' @param title_size Numeric font size for title. Defaults to 36
#' @param title_family Font family for title. Defaults to "Arial"
#'
#' @return The modified pptx object with the new slide added
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(officer)
#'
#' # Create presentation
#' pptx <- read_pptx("template.pptx")
#'
#' # Create plot and convert to rvg
#' p <- ggplot(mtcars, aes(x = mpg, y = hp)) +
#'     geom_point()
#' rvg_plot <- convert_to_rvg(p)
#'
#' # Add to presentation
#' pptx <- add_to_pptx(pptx, rvg_plot, title = "MPG vs Horsepower")
#'
#' # Save presentation
#' print(pptx, target = "output.pptx")
#' }
#'
#' @export
add_to_pptx <- function(pptx,
                        rvg_graphic,
                        title = "Title",
                        title_lines = "one",
                        top = 1.5,
                        left = 0.3,
                        fig_width = 10,
                        fig_height = 5,
                        layout_one = "Title and Content_No Graphic",
                        layout_two = "Title Two Line and Content_No Graphic",
                        master = "Office Theme",
                        title_color = "#003494",
                        title_size = 36,
                        title_family = "Arial") {
    # Determine slide layout based on title_lines argument
    if (title_lines == "one") {
        pptx <- officer::add_slide(pptx,
            layout = layout_one,
            master = master
        )
    } else {
        pptx <- officer::add_slide(pptx,
            layout = layout_two,
            master = master
        )
    }

    # Define properties for formatting title text
    fp_title <- officer::fp_text(
        font.size = title_size,
        font.family = title_family,
        color = title_color
    )

    # Add figure to slide
    pptx <- officer::ph_with(
        x = pptx,
        location = officer::ph_location_template(
            type = "body",
            left = left,
            top = top,
            width = fig_width,
            height = fig_height
        ),
        value = rvg_graphic
    )

    # Add title to slide
    pptx <- officer::ph_with(
        x = pptx,
        location = officer::ph_location_type(type = "title"),
        value = officer::fpar(title, fp_t = fp_title)
    )

    return(pptx)
}

#' Add Title Slide to PowerPoint Presentation
#'
#' @description
#' Adds a title slide to a PowerPoint presentation with a formatted title
#' and subtitle. Uses the "Title Slide" layout from the slide master.
#'
#' @param pptx An officer pptx object (the PowerPoint presentation)
#' @param title Character string for the slide title
#' @param cutoff_date_f Character string for the cutoff date to display in subtitle
#' @param master Character string for slide master name.
#'   Defaults to "Office Theme"
#' @param title_color Hex color code for title text.
#'   Defaults to TG royal blue ("#003494")
#' @param title_size Numeric font size for title. Defaults to 36
#' @param subtitle_size Numeric font size for subtitle. Defaults to 24
#' @param title_family Font family for title and subtitle. Defaults to "Arial"
#'
#' @return The modified pptx object with the title slide added
#'
#' @examples
#' \dontrun{
#' library(officer)
#'
#' # Create presentation
#' pptx <- init_pptx("template.pptx")
#'
#' # Add title slide
#' pptx <- add_title_slide(
#'     pptx,
#'     title = "My Presentation Title",
#'     cutoff_date_f = "01-Jan-2024"
#' )
#'
#' # Save presentation
#' print(pptx, target = "output.pptx")
#' }
#'
#' @export
add_title_slide <- function(pptx,
                            title,
                            cutoff_date_f,
                            master = "Office Theme",
                            title_color = "#003494",
                            title_size = 36,
                            subtitle_size = 24,
                            title_family = "Arial") {
    # Add slide with Title Slide layout
    pptx <- officer::add_slide(
        pptx,
        layout = "Title Slide",
        master = master
    )

    # Define properties for formatting title text
    fp_title <- officer::fp_text(
        font.size = title_size,
        font.family = title_family,
        color = title_color
    )

    # Define properties for formatting subtitle text
    fp_subtitle <- officer::fp_text(
        font.size = subtitle_size,
        font.family = title_family,
        color = title_color
    )

    # Add content to "title" content holder in Title Slide Layout
    pptx <- officer::ph_with(
        pptx,
        value = officer::fpar(title, fp_t = fp_title),
        location = officer::ph_location_type(type = "ctrTitle")
    )

    # Add content to "subtitle" content holder in Title Slide Layout
    pptx <- officer::ph_with(
        pptx,
        value = officer::fpar(paste("Cutoff Date:", cutoff_date_f), fp_t = fp_subtitle),
        location = officer::ph_location_type(type = "subTitle")
    )

    return(pptx)
}

#' Initialize Empty PowerPoint Presentation
#'
#' @description
#' Reads a PowerPoint template and removes all existing slides to create
#' a clean starting point for adding new slides programmatically. By default,
#' uses the package's built-in template, but a custom template path can be
#' provided.
#'
#' @param template_path Path to a custom PowerPoint template file. If NULL
#'   (default), uses the package's built-in template located in
#'   `inst/extdata/template.pptx`.
#'
#' @return An officer pptx object with all slides removed
#'
#' @examples
#' \dontrun{
#' # Use the built-in package template
#' pptx <- init_pptx()
#'
#' # Or use a custom template
#' pptx <- init_pptx("path/to/custom_template.pptx")
#' }
#'
#' @export
init_pptx <- function(template_path = NULL) {
    # Use built-in template if no path provided
    if (is.null(template_path)) {
        template_path <- system.file(
            "extdata", "tgtx_template.pptx",
            package = "aaa.helper.fun",
            mustWork = TRUE
        )
    }

    pptx <- officer::read_pptx(template_path)

    # Remove all slides from the template
    for (i in seq_along(pptx)) {
        pptx <- officer::remove_slide(pptx)
    }

    return(pptx)
}
