#' Gapminder Data
#'
#' Loads in gapminder dataset from Github.
#'
#' @return A data.frame of 6 columns and 1704 observations on different countries
#' from year 1952 to 2007
#'
#' @note A dedicated \code{\link{fit}} function is provided for objects of class \code{"gapr"}.
#'
#' @export
#' @author Syed Baryalay - <\email{syed.baryalay.2020@@mumail.ie"}>
#'
#' @seealso \code{\link{fit}}, \code{\link{plot.gapr_fit}}
#' @examples
#' dat <- load_gapr()
#'
load_gapr <- function(summarize_over=c("overall" ,"country", "continent"),
                      value="overall") {

  summarize_over <- match.arg(summarize_over)
  dataset <- read.csv("https://raw.githubusercontent.com/Syed47/dataset/master/gapminder.csv")

  gm <- switch(summarize_over,
               overall = {
                 dataset |>
                   dplyr::group_by(year) |>
                   dplyr::summarise(lifeExp = mean(lifeExp), pop, gdpPercap)
                },
                country = {
                  dataset |>
                    dplyr::filter(country == value) |>
                    dplyr::group_by(country, year) |>
                    dplyr::summarise(lifeExp = mean(lifeExp), pop, gdpPercap)
                },
                continent = {
                  dataset |>
                    dplyr::filter(continent == value) |>
                    dplyr::group_by(continent, year) |>
                    dplyr::summarise(lifeExp = mean(lifeExp), pop, gdpPercap)
                })
  output <- list(data = gm)
  class(output) <- c("gapr", "listof")
  return(output)
}

