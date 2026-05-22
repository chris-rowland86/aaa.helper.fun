#' TG Therapeutics Custom ggplot2 Theme #
#'
#' @description
#' A custom ggplot2 theme with TG Therapeutics styling. Features Arial font,
#' centered titles, clean backgrounds, and professional axis formatting.
#'
#' @param base_size Base font size for the theme. Defaults to 18.
#' @param base_family Base font family. Defaults to "Arial".
#'
#' @return A ggplot2 theme object
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Apply TG theme to a plot
#' ggplot(mtcars, aes(x = mpg, y = hp)) +
#'   geom_point() +
#'   theme_tg()
#'
#' # Use with different base size
#' ggplot(mtcars, aes(x = mpg, y = hp)) +
#'   geom_point() +
#'   theme_tg(base_size = 14)
#'
#' # Apply theme globally to all plots
#' theme_set(theme_tg())
#' }
#'
#' @export
theme_tg <- function(base_size = 18, base_family = "Arial") {
    ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
        ggplot2::theme(
            # globally sets font for all elements
            text = ggplot2::element_text(
                family = base_family,
                size = base_size,
                color = "black"
            ),
            # Centers Plot Title
            plot.title = ggplot2::element_text(
                face = "bold",
                hjust = 0.5
            ),
            # Centers subtitle
            plot.subtitle = ggplot2::element_text(
                face = "bold",
                hjust = 0.5
            ),
            # Remove plot background
            plot.background = ggplot2::element_blank(),
            # Remove panel background
            panel.background = ggplot2::element_blank(),
            # Remove panel border
            panel.border = ggplot2::element_blank(),
            # Removes major gridlines
            panel.grid.major = ggplot2::element_blank(),
            # Removes minor gridlines
            panel.grid.minor = ggplot2::element_blank(),
            # Removes strip background
            strip.background = ggplot2::element_blank(),

            # axis attributes (both x & y)
            # title
            axis.title = ggplot2::element_text(
                color = "black",
                size = base_size,
                face = "bold",
                hjust = 0.5
            ),
            # labels
            axis.text = ggplot2::element_text(
                color = "black",
                size = base_size,
                angle = 0,
                hjust = 0.5,
                vjust = 0.5
            ),
            # axis line
            axis.line = ggplot2::element_line(color = "black"),
            # axis ticks
            axis.ticks = ggplot2::element_line(color = "black"),

            # x-axis specific attributes
            axis.title.x = ggplot2::element_text(
                margin = ggplot2::margin(
                    t = 15,
                    r = 0,
                    b = 0,
                    l = 0,
                    unit = "pt"
                )
            ),

            # y-axis attributes
            axis.title.y = ggplot2::element_text(
                margin = ggplot2::margin(
                    t = 0,
                    r = 15,
                    b = 0,
                    l = 0,
                    unit = "pt"
                )
            ),

            # Legend attributes
            legend.background = ggplot2::element_blank(),
            legend.key = ggplot2::element_blank(),
            legend.title = ggplot2::element_text(
                color = "black",
                size = base_size,
                face = "bold",
                hjust = 0.5
            ),
            legend.text = ggplot2::element_text(
                size = base_size,
                color = "black",
                hjust = 0.5
            ),
            legend.position = "right",
            # caption text
            plot.caption = ggplot2::element_text(
                size = 9
            )
        )
}
