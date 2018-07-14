# UCSF brand colors
ucsf_colors <- c(
  `navy inter`  = "#052049",
  `navy`                   = "#052049",
  `navy3` = "#506380",
  `navy2` = "#9BA6B6",
  `navy1` = "#E6E9ED",
  `teal inter`       = "#058488",
  `teal` = "#18A3AC",
  `teal3`  = "#5DBFC5",
  `teal2` = "#A3DADE",
  `teal1` = "#E8F6F7",
  `green inter`                  = "#6EA400",
  `green` = "#90BD31",
  `green3` = "#B1D16F",
  `green2` = "#D3E4AD",
  `green1` = "#F4F8EA",
  `blue inter`                   = "#007CBE",
  `blue` = "#178CCB",
  `blue3` = "#5DAFDB",
  `blue2` = "#A2D1EA",
  `blue1` = "#E8F4FA",
  `orange inter`                 = "#F26D04",
  `orange` = "#F48024",
  `orange3` = "#F7A665",
  `orange2` = "#FBCCA7",
  `orange1` = "#FEF2E9",
  `purple inter`               = "#716FB2",
  `purple` = "#716FB2",
  `purple3` = "#9C9AC9",
  `purple2` = "#C6C5E0",
  `purple1` = "#F1F1F7",
  `red inter`                    = "#EB093C",
  `red` = "#EC1848",
  `red3` = "#F25D7F",
  `red2` = "#F7A3B6",
  `red1` = "#FDE8ED",
  `yellow inter`                 = "#FFDD00",
  `yellow` = "#FFDD00",
  `yellow3` = "#FFE74D",
  `yellow2` = "#FFF199",
  `yellow1` = "#FFFCE6"
  )

#' Function to extract official UCSF colors as hex codes.
#'
#' Executing this function without
#' any arguments provides a listing of color names and their associated hex codes.
#' These colors are based on the UCSF Identity Guidelines, available at
#' \url{https://identity.ucsf.edu/brand-guide/print-colors} and
#' \url{https://identity.ucsf.edu/brand-guide/digital-colors}.
#' The base colors are named navy, teal, green, blue, orange.
#' Accent colors are purple, red, yellow.
#' Each color has 3 lighter tints, a 70\%, 40\%, and 10\% tint of the main color.
#' These are names based on the main color, appended with a 3, 2, or 1, respectively.
#' For example, full strength is called "blue", the 70\% tint is "blue3", the 40\% tint
#' is "blue2", and the 10\% tint is "blue1"

#' @param ... Character names of UCSF color.
#'
#' @examples
#' ucsf_col("navy")
#' ucsf_col("teal")
#' ucsf_col("green")
#' ucsf_col("blue")
#' ucsf_col("orange")
#' ucsf_col("red")
#' ucsf_col("purple")
#' ucsf_col("yellow")
#' ucsf_col("red3")


ucsf_col <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (ucsf_colors)

  ucsf_colors[cols]
}

ucsf_palettes <- list(
  `main`  = ucsf_col("navy", "teal", "green", "blue", "orange"),

  `navies`  = ucsf_col("navy", "navy3", "navy2", "navy1"),

  `teals`  = ucsf_col("teal", "teal3", "teal2", "teal1"),

  `greens`  = ucsf_col("green", "green3", "green2", "green1"),

  `blues`  = ucsf_col("blue", "blue3", "blue2", "blue1"),

  `oranges`  = ucsf_col("orange", "orange3", "orange2", "orange1"),

  `purples`  = ucsf_col("purple", "purple3", "purple2", "purple1"),

  `reds`  = ucsf_col("red", "red3", "red2", "red1"),

  `yellows`  = ucsf_col("yellow", "yellow3", "yellow2", "yellow1")
)

#' Return function to interpolate a UCSF color palette
#' @export
#' @param palette Character name of palette in ucsf_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
ucsf_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- ucsf_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

#' Color scale constructor for UCSF colors
#' @export
#' @param palette Character name of palette in ucsf_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_ucsf <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- ucsf_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("ucsf_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


#' Fill scale constructor for ucsf colors
#' @export
#' @param palette Character name of palette in ucsf_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_ucsf <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- ucsf_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("ucsf_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
