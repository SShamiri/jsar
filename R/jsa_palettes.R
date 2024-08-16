#' JSA colour palettes
#'
#' @param name name of the colour palette we want to use
#' @param all_palettes list of colour palettes we want to extract our choice from
#' @param n how many colours from it we want to use
#' @param type discrete or continuous colour palette
#'
#' @return
#' @export
#'
#' @examples
jsa_palettes <- function(name, n,
                         all_palettes = jsa_colours,
                         type = c("discrete", "continuous")) {
  palette <- all_palettes[[name]]
  if (missing(n)) {
    n <- length(palette)
  }
  type <- match.arg(type)
  out <- switch(type,
    continuous = grDevices::colorRampPalette(palette)(n),
    discrete = palette[1:n]
  )
  structure(out, name = name, class = "palette")
}

# Define colours
jsa_colours <- list(
  primary = c(
    "#6929c4", "#009d9a", "#012749", "#ee538b", "#1192e8",
    "#9f1853", "#005d5d", "#fa4d56", "#570408", "#198038",
    "#002d9c", "#b28600", "#8a3800", "#a56eff"
  ),
  fav = c("#2f005f", "#4b0985", "#d5a3f9", "#d2de5a", "#0f2532", "#b91c1c")
)

## Update ggplot colour and fill
# Discrete
scale_colour_jsa_d <- function(name) {
  ggplot2::scale_colour_manual(values = jsa_palettes(name,
    type = "discrete"
  ))
}

scale_fill_jsa_d <- function(name) {
  ggplot2::scale_fill_manual(values = jsa_palettes(name,
    type = "discrete"
  ))
}
# Continuous
scale_colour_jsa_c <- function(name) {
  ggplot2::scale_colour_gradientn(colours = jsa_palettes(
    name = name,
    type = "continuous"
  ))
}

scale_fill_jsa_c <- function(name) {
  ggplot2::scale_fill_gradientn(colours = jsa_palettes(
    name = name,
    type = "continuous"
  ))
}
# Ensure sale_colour_*() function work with Britsh or American spelling
scale_color_jsa_d <- scale_colour_jsa_d
scale_color_jsa_c <- scale_colour_jsa_c
