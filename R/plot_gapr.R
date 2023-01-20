#' Plot gapr output
#'
#' Plot \code{"lifeExp"} over the years, of N countries or continents
#'
#' @param obj An object of class \code{"gapr"} outputted from the \code{\link{load_gapr}} function.
#'
#' @return A ggplot plot which plots \code{lifeExp} against \code{year}
#'
#' @importFrom dplyr "filter" "group_by" "summarise"
#' @importFrom ggplot2 "aes" "geom_line" "ggplot" "theme_classic"
#' @author Syed Baryalay - <\email{syed.baryalay.2020@@mumail.ie}>
#' @seealso \code{\link{load_gapr}}
#' @examples
#' dat <- load_gapr()
#' plot(dat, continent, c("Asia", "Europe", "Africa"))
#' plot(dat, country, c("China", "Ireland", "Nigeria"))
#' @export
plot.gapr <- function(obj, var=c("country", "continent"), val, ...) {
  obj$data |>
    dplyr::filter({{ var }} %in% val) |>
    dplyr::group_by({{ var }}, year) |>
    dplyr::summarise(lifeExp = median(lifeExp)) |>
    ggplot(aes(x=year, y=lifeExp))+
    geom_line(aes(colour={{var}})) +
    theme_classic()
}
