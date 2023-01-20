#' Plot gapr fit output
#'
#' Plot \code{"lifeExp"} over the years, worldwide, by country or by continent.
#'
#' @param x An object of class \code{"gapr_fit"} outputted from the \code{\link{fit}} function.
#' @param ... Optional arguments that can be passed to the plot method.
#'
#' @return A ggplot plot which plots \code{lifeExp} against \code{year}.
#'
#' @importFrom ggplot2 "aes" "geom_line" "geom_point" "ggplot" "ggtitle" "theme_classic" "theme"
#' @author Syed Baryalay - <\email{syed.baryalay.2020@@mumail.ie}>
#' @seealso \code{\link{load_gapr}} \code{\link{fit}}
#' @examples
#' dat <- load_gapr("country", "Ireland")
#' mod1 <- fit(dat)
#' mod2 <- fit(dat, effects = "mixed")
#' plot(mod1)
#' plot(mod2)
#' @export
plot <- function(x, ...) {
  UseMethod("plot")
}
#' @export
plot.gapr_fit <- function(x, ...) {
  ggplot2::ggplot(x$data, ggplot2::aes(x=year, y=lifeExp))+
    ggplot2::geom_point() +
    ggplot2::geom_line(y=fitted(x$model), color="red") +
    ggplot2::theme_classic() +
    ggplot2::ggtitle(paste("model:", x$effects))
}
