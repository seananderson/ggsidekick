#' A sleek theme for ggplot2
#'
#' A simple theme for ggplot2 that loosely resembles nicely themed plots from
#' base graphics.
#'
#' @param base_size Base size
#' @param base_family Base family
#'
#' @importFrom ggplot2 element_text element_rect element_blank theme_light theme
#'   rel
#' @importFrom grid unit
#'
#' @examples
#' p <- ggplot2::ggplot(mtcars) +
#'   ggplot2::geom_point(ggplot2::aes(x = wt, y = mpg, colour = factor(gear))) +
#'   ggplot2::facet_wrap(~am)
#' p + theme_sleek()
#'
#' @export

theme_sleek <- function(base_size = 11, base_family = "") {
  half_line <- base_size/2
  theme_light(base_size = base_size, base_family = "") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "grey30"),
      strip.text.y = element_text(colour = "grey30"),
      axis.text = element_text(colour = "grey30"),
      axis.title = element_text(colour = "grey30"),
      legend.title = element_text(colour = "grey30", size = rel(0.9)),
      panel.border = element_rect(fill = NA, colour = "grey70", size = 1),
      legend.key.size = unit(0.9, "lines"),
      legend.text = element_text(size = rel(0.7), colour = "grey30"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA),
      plot.title = element_text(colour = "grey30", size = rel(1)),
      plot.subtitle = element_text(colour = "grey30", size = rel(.85))
    )
}
