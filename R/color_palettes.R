#' Extract TG Corporate Colors as Hex Codes
#'
#' @description
#' Function to extract TG colors as hex codes. If no argument is provided,
#' returns the full palette of TG colors.
#'
#' @param ... Character names of TG colors to extract. Valid options include:
#'   `ice_blue`, `navy_blue`, `green`, `sky_blue`, `royal_blue`,
#'   `warm_gray`, `yellow`, `red`
#'
#' @return A named character vector of hex color codes
#'
#' @examples
#' # Get all colors
#' tg_corp_color()
#'
#' # Get specific colors
#' tg_corp_color("green", "royal_blue")
#'
#' @export
tg_corp_color <- function(...) {
    # Color Key for TG color scheme
    tg_corp_colors <- c(
        `ice_blue` = "#BBCBD3",
        `navy_blue` = "#121B4D",
        `green` = "#99CC33",
        `sky_blue` = "#3EA7F3",
        `royal_blue` = "#003494",
        `warm_gray` = "#909FA7",
        `yellow` = "#FEE568",
        `red` = "#FF2600"
    )

    cols <- c(...)

    if (is.null(cols)) {
        return(tg_corp_colors)
    }

    tg_corp_colors[cols]
}

#' Generate Color Ramps from a Center Color
#'
#' @description
#' Function that ramps from white to black through a center color,
#' outputting 7 colors. The output includes 50% darker, 25% darker,
#' center color, 25% lighter, and 50% lighter variations.
#'
#' @param center_color A single color (hex code or color name) to act as
#'   the center color being darkened and lightened
#'
#' @return A character vector of 7 hex color codes ranging from dark to light
#'
#' @examples
#' ramp_hues("#99CC33")
#' ramp_hues("navy")
#'
#' @export
ramp_hues <- function(center_color = NULL, ...) {
    if (is.null(center_color)) {
        return("please specify a color")
    }

    grDevices::colorRampPalette(c(
        "black",
        center_color,
        "white"
    ))(9)[2:8]
}

#' Get TG Corporate Color Palettes
#'
#' @description
#' Function that arranges colors into specific palettes and stores as a list.
#' Hue palettes ramp from white to black through specific colors using 9 colors
#' including black and white. Selecting colors 3:7 keeps 50% darker,
#' 25% darker, center color, 25% lighter, 50% lighter.
#'
#' @param palette Character name of desired palette. Options include:
#'   * `"main"` - green, royal_blue, navy_blue
#'   * `"highlight"` - green, ice_blue
#'   * `"ae"` - sky_blue, warm_gray, yellow, red
#'   * `"all"` - returns list of all available palettes
#'   * Any individual color name (e.g., `"ice_blue"`, `"green"`) returns
#'     a ramped hue palette for that color
#'
#' @return A named character vector of hex color codes, or a list of all
#'   palettes if `palette = "all"`
#'
#' @examples
#' # Get main palette
#' tg_corp_palette("main")
#'
#' # Get AE severity palette
#' tg_corp_palette("ae")
#'
#' # View all palettes
#' tg_corp_palette("all")
#'
#' # Get hue variations of a specific color
#' tg_corp_palette("ice_blue")
#'
#' # View colors using scales package
#' # scales::show_col(tg_corp_palette("main"), cex_label = 2)
#'
#' # Select specific hues from hue palettes
#' tg_corp_palette("ice_blue")[4]
#'
#' @export
tg_corp_palette <- function(palette = "main", ...) {
    # Define custom palettes here
    tg_corp_palettes <- list(
        `main` = tg_corp_color("green", "royal_blue", "navy_blue"),
        `highlight` = tg_corp_color("green", "ice_blue"),
        `ae` = tg_corp_color("sky_blue", "warm_gray", "yellow", "red")
    )

    # Grabs all colors in tg_corp_color function
    # the way the tg_corp_color function is written, defaults to outputting
    # all the colors if no input is provided
    all_colors <- tg_corp_color()

    # Converts from character vector to LIST in order to purrr:map it
    all_colors <- as.list(all_colors)

    # applies ramp_hues function to all colors
    # NOTE: these ramped-hues-palettes will inherit the NAME of the color,
    # so the entire palette will be named the color name
    hue_palettes <- purrr::map(all_colors, ramp_hues)

    # Appends the hue_palettes to the end of the defined palettes
    tg_corp_palettes <- c(tg_corp_palettes, hue_palettes)

    if (palette == "all") {
        return(tg_corp_palettes)
    }

    tg_corp_palettes[[palette]]
}

#' Generate a Palette Function
#'
#' @description
#' This function outputs another function which accepts `n` as an argument.
#' The `n` refers to the number of colors needed for a particular plot.
#'
#' Creates a discrete palette that will use the first `n` colors from
#' the supplied color values when the palette has enough colors.
#' Otherwise, uses an interpolated color palette via `colorRampPalette()`.
#'
#' Can be used with `setNames()` to create named palettes so that specific
#' groups are always the same color.
#'
#' @param palette Character name of desired palette, defaults to `"main"`
#' @param direction Numeric controlling the direction of the palette.
#'   Use `< 0` to reverse the order
#'
#' @return A function that takes `n` (number of colors) and returns a
#'   character vector of hex color codes
#'
#' @examples
#' # Generate 3 colors from main palette
#' palette_gen()(3)
#'
#' # Generate 5 colors from AE palette
#' palette_gen("ae")(5)
#'
#' # Reverse palette direction
#' palette_gen("main", direction = -1)(3)
#'
#' # Create named palette for treatment arms
#' arms <- c("ubli", "teri")
#' arms_named_palette <- setNames(
#'   object = palette_gen()(length(unique(arms))),
#'   nm = unique(arms)
#' )
#'
#' @export
palette_gen <- function(palette = "main", direction = 1) {
    force(palette)
    function(n) {
        if (n <= length(tg_corp_palette(palette))) {
            all_colors <- tg_corp_palette(palette)

            all_colors <- unname(unlist(all_colors))

            all_colors <- if (direction >= 0) all_colors else rev(all_colors)

            all_colors[1:n]
        } else {
            pal <- tg_corp_palette(palette)

            pal <- if (direction >= 0) pal else rev(pal)

            grDevices::colorRampPalette(pal, alpha = FALSE)(n)
        }
    }
}

#' TG Color Scale for ggplot2
#'
#' @description
#' Color scale constructor for TG colors. Use this function to apply
#' TG corporate colors to the `color` aesthetic in ggplot2.
#'
#' @param palette Character name of palette in `tg_corp_palette()`.
#'   Defaults to `"main"`
#' @param discrete Logical indicating whether color aesthetic is discrete
#'   or continuous. Defaults to `TRUE`
#' @param direction Numeric indicating whether the palette should be reversed.
#'   Use `< 0` to reverse. Defaults to `1`
#' @param ... Additional arguments passed to `ggplot2::scale_colour_discrete()`
#'   or `ggplot2::scale_color_gradientn()`, used respectively when discrete
#'   is TRUE or FALSE
#'
#' @return A ggplot2 scale object
#'
#' @examples
#' library(ggplot2)
#'
#' # Discrete color scale
#' ggplot(mtcars, aes(x = mpg, y = hp, color = factor(cyl))) +
#'   geom_point(size = 3) +
#'   scale_color_tg()
#'
#' # Continuous color scale
#' ggplot(mtcars, aes(x = mpg, y = hp, color = wt)) +
#'   geom_point(size = 3) +
#'   scale_color_tg(discrete = FALSE)
#'
#' # Use AE palette
#' ggplot(mtcars, aes(x = mpg, y = hp, color = factor(gear))) +
#'   geom_point(size = 3) +
#'   scale_color_tg(palette = "ae")
#'
#' @export
scale_color_tg <- function(palette = "main", discrete = TRUE, direction = 1, ...) {
    pal <- palette_gen(palette = palette, direction = direction)

    if (discrete) {
        ggplot2::discrete_scale(
            aesthetics = "color",
            scale_name = paste0("tg_", palette),
            palette = pal,  # ggplot2 will call pal(n) automatically
            ...
        )
    } else {
        ggplot2::scale_color_gradientn(colors = pal(256), ...)
    }
}

#' TG Fill Scale for ggplot2
#'
#' @description
#' Fill scale constructor for TG colors. Use this function to apply
#' TG corporate colors to the `fill` aesthetic in ggplot2.
#'
#' @param palette Character name of palette in `tg_corp_palette()`.
#'   Defaults to `"main"`
#' @param discrete Logical indicating whether fill aesthetic is discrete
#'   or continuous. Defaults to `TRUE`
#' @param direction Numeric indicating whether the palette should be reversed.
#'   Use `< 0` to reverse. Defaults to `1`
#' @param ... Additional arguments passed to `ggplot2::scale_fill_discrete()`
#'   or `ggplot2::scale_fill_gradientn()`, used respectively when discrete
#'   is TRUE or FALSE
#'
#' @return A ggplot2 scale object
#'
#' @examples
#' library(ggplot2)
#'
#' # Discrete fill scale
#' ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
#'   geom_bar() +
#'   scale_fill_tg()
#'
#' # Continuous fill scale
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_tile() +
#'   scale_fill_tg(discrete = FALSE)
#'
#' # Use highlight palette
#' ggplot(mtcars, aes(x = factor(am), fill = factor(am))) +
#'   geom_bar() +
#'   scale_fill_tg(palette = "highlight")
#'
#' @export
scale_fill_tg <- function(palette = "main", discrete = TRUE, direction = 1, ...) {
    pal <- palette_gen(palette = palette, direction = direction)

    if (discrete) {
        ggplot2::discrete_scale(
            aesthetics = "fill",
            scale_name = paste0("tg_", palette),
            palette = pal,  # ggplot2 will call pal(n) automatically
            ...
        )
    } else {
        ggplot2::scale_fill_gradientn(colors = pal(256), ...)
    }
}