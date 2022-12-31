library(tidyverse)
library(dplyr)
library(tidyr)
library(gapminder)

# summarize (...)
# plot (...)
# fit ()
# predict()

summarize <- function(x, filter_var, filter_value, group_var,
                      filter_func = (`==`),
                      summary_func = list(min=min, max=max)) {
    x |>
        dplyr::filter({{ filter_var }} |> filter_func(filter_value)) |>
        dplyr::group_by({{ group_var }}) |>
        dplyr::select(where(is.numeric) & -year) |>
        dplyr::summarise_all(.tbl=_, summary_func) 
}

s <- summarize(gapminder,
          pop, 10000000,
          year, 
          filter_func = (`>=`)); s



