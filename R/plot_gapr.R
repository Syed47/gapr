#' Plot gapr output
#'
#' Plot \code{"lifeExp"} over the years, of N countries or continents
#'
#' @param x An object of class \code{"gapr"} outputted from the \code{\link{load_gapr}} function.
#' @param var A data variable from the gapminder on which the data is summarized.
#' @param val A value for \code{country} or \code{continent} data variable which need to be summarized.
#'
#' @return A ggplot plot which plots \code{lifeExp} against \code{year}
#'
#' @importFrom dplyr "filter" "group_by" "summarize"
#' @importFrom ggplot2 "aes" "geom_line" "ggplot" "theme_classic"
#' @author Syed Baryalay - <\email{syed.baryalay.2020@@mumail.ie}>
#' @seealso \code{\link{load_gapr}}
#' @examples
#' dat <- load_gapr()
#' plot_gapr(dat, continent, c("Asia", "Europe", "Africa"))
#' plot_gapr(dat, country, c("China", "Ireland", "Nigeria"))
#' @export
plot_gapr <- function(x, var=continent, val=c("Asia", "Europe")) {
  x$data |>
    dplyr::filter({{ var }} %in% val) |>
    dplyr::group_by({{ var }}, year) |>
    dplyr::summarize(lifeExp = median(lifeExp)) |>
    ggplot2::ggplot(ggplot2::aes(x=year, y=lifeExp))+
      ggplot2::geom_line(ggplot2::aes(colour={{var}})) +
      ggplot2::theme_classic()
}
