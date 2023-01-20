#' Summary Function
#'
#' summary function that summarizes the data based on conditions
#'
#' \code{summary} function filters data from the \code{gapminder} dataset based on the condition,
#' groups it on some variable and returns the summary of all numeric variables.
#'
#' @param obj A object of class \code{}
#' @param filter_var A data variable from \code{obj} on which the data should be filtered
#' @param filter_value A value that can be used for comparison when filtering data
#' @param group_var A data variable from \code{obj} on which we want to group the
#'  data
#' @param filter_func An optional operator written as a function giving the operation on which to filter,
#' given by \code{(`==`)} by default, such that \code{gapminder} dataset is filtered according
#' to whether \code{filter_var == filter_value}.
#' @param summary_func An optional function that we can pass to
#'  \code{summary} to produce a summary of all columns of the \code{obj}.
#' @return A \code{data.frame} which contains the summary of the filtered and grouped data.
#' @importFrom dplyr "filter" "group_by" "select" "summarise_all"
#' @examples
#' dat <- load_gapr()
#' summary(dat, lifeExp, 50, country, filter_func = (`<`))
#'
#' dat <- load_gapr("continent", "Asia")
#' summary(dat, year, 1980, continent, filter_func = (`>=`));
#' #' @export
summary.gapr <- function(obj, filter_var, filter_value, group_var,
                          filter_func = (`==`),
                          summary_func = list(mean=mean)) {
  output <- list(data = obj$data |> as.data.frame() |>
                   dplyr::filter({{ filter_var }} |> filter_func(filter_value)) |>
                   dplyr::group_by({{ group_var }}) |>
                   dplyr::select(where(is.numeric) & -year) |>
                   dplyr::summarise_all(.tbl=_, summary_func))
  class(output) <- c("gapr", "listof")
  return(output)
}
