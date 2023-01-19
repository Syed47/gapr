#' summarize
#'
#' \code{summarize} function filters data from the \code{gapminder} dataset based on the condition,
#' groups it on some variable and returns the summary of all numeric variables.
#'
#' @param dat A \code{data.frame} or \code{tibble}
#' @param filter_var A data variable from \code{dat} on which the data should be filtered
#' @param filter_value A value that can be used for comparison when filtering data
#' @param group_var A data variable from \code{dat} on which we want to group the
#'  data
#' @param filter_func An optional operator written as a function giving the operation on which to filter,
#' given by \code{(`==`)} by default, such that \code{gapminder} dataset is filtered according
#' to whether \code{filter_var == filter_value}.
#' @param summary_func An optional function that we can pass to
#'  \code{summarize} to produce a summary of all columns of
#'  the \code{dat}
#' @return A \code{data.frame} which contains the summary of the filtered and grouped data.
#' @importFrom dplyr "filter" "group_by" "select" "summarize_all"
#' @importFrom gapminder "gapminder"
#' @export
#'
#' @examples
#' summarize(gdpPercap, 1000, country, filter_func = (`<`));
#' summarize(pop, 10000000, year, filter_func = (`>=`));

summarize <- function(filter_var, filter_value, group_var,
                          filter_func = (`==`),
                          summary_func = list(mean=mean)) {
  output <- list(data = gapminder::gapminder |>
                   dplyr::filter({{ filter_var }} |> filter_func(filter_value)) |>
                   dplyr::group_by({{ group_var }}) |>
                   dplyr::select(where(is.numeric) & -year) |>
                   dplyr::summarise_all(.tbl=_, summary_func))
  attr(output, "source") <- attr(gapminder::gapminder, "source")
  class(output) <- c("climr_summarize", "listof")
  return(output)
}


s <- summarize(pop, 10000000, year, filter_func = (`>=`)); s

summarize(gdpPercap, 1000, country, filter_func = (`<`))




# devtools::load_all()
#
# devtools::document()

# devtools::build()

# devtools::load_all()
#
# devtools::document()
