#' Fourth root scale transformation for ggplot
#'
#' @export
#'
#' @references
#' <https://andrewgelman.com/2007/01/02/the_14power_tra/>
#'
#' @examples
#' ggplot2::ggplot(mtcars) +
#'   ggplot2::geom_point(ggplot2::aes(x = wt, colour = mpg, y = factor(gear))) +
#'   ggplot2::scale_colour_viridis_c(trans = "fourth_root_power")
fourth_root_power_trans <- function() {
  scales::trans_new(
    name = "fourth root power",
    transform = function(x) ifelse(x > 0, x^0.25, -(-x)^0.25),
    inverse = function(x) ifelse(x > 0, x^4, -(-x)^4),
    domain = c(Inf, Inf))
}
