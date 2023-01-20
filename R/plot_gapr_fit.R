#' Plot gapr fit output
#'
#' Plot \code{"lifeExp"} over the years, worldwide, by country or by continent.
#'
#' @param model_fit An object of class \code{"gapr_fit"} outputted from the \code{link{fit}} function.
#'
#' @return A ggplot plot which plots \code{lifeExp} against \code{year}.
#'
#' @importFrom ggplot2 "aes" "geom_line" "geom_point" "ggplot" "ggtitle" "theme_classic" "theme"
#' @author Syed Baryalay - <\email{syed.baryalay.2020@@mumail.ie}>
#' @seealso \code{\link{fit}}
#' @examples
#' dat <- load_gapr("country", "Ireland")
#' mod1 <- fit(dat)
#' mod2 <- fit(dat, effects = "mixed")
#' plot(mod1)
#' plot(mod2)
#' @export
plot.gapr_fit <- function(model_fit) {
  ggplot(model_fit$data, aes(x=year, y=lifeExp))+
    geom_point() +
    geom_line(y=fitted(model_fit$model), color="red") +
    theme_classic() +
    ggtitle(paste("model:", model_fit$effects))
}
