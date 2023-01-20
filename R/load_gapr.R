#' Gapminder Data
#'
#' Loads in gapminder dataset from Github.
#' @param summarize Variable on which the gapminder dataset will be summarized.
#' @param summarize Value(s) to look for when filtering the gapminder dataset.
#' @return A data.frame of 6 columns and 1704 observations on different countries
#' from year 1952 to 2007
#'
#' @importFrom dplyr "filter" "group_by" "summarise"
#'
#' @note A dedicated \code{\link{fit}} and \code{\link{plot}} functions are
#' provided for objects of class \code{"gapr"}.
#'
#' @export
#' @author Syed Baryalay - <\email{syed.baryalay.2020@@mumail.ie"}>
#'
#' @seealso \code{\link{fit}}, \code{\link{plot.gapr}}, \code{\link{plot.gapr_fit}}
#' @examples
#' dat <- load_gapr("country", "Ireland")
#' dat <- load_gapr("continent", "Europe")
#' dat <- load_gapr("year")
#'
#' @export
load_gapr <- function(summarize=c("none", "year", "country", "continent"),
                      value=c("none")) {

  summarize <- match.arg(summarize)
  dataset <- read.csv("https://raw.githubusercontent.com/Syed47/dataset/master/gapminder.csv")

  gm <- switch(summarize,
               none = { dataset |> select(-X) },
               year = {
                 dataset |>
                   dplyr::group_by(year) |>
                   dplyr::summarise(lifeExp = median(lifeExp),
                                    pop = median(pop),
                                    gdpPercap = median(gdpPercap))
                },
                country = {
                  dataset |>
                    dplyr::filter(country %in% value) |>
                    dplyr::group_by(country, year) |>
                    dplyr::summarise(lifeExp = median(lifeExp),
                                     pop = median(pop),
                                     gdpPercap = median(gdpPercap))
                },
                continent = {
                  dataset |>
                    dplyr::filter(continent %in% value) |>
                    dplyr::group_by(continent, year) |>
                    dplyr::summarise(lifeExp = median(lifeExp),
                                     pop = median(pop),
                                     gdpPercap = median(gdpPercap))
                })
  output <- list(data = gm, summarize=summarize, value=summarize)
  class(output) <- c("gapr", "listof")
  return(output)
}

