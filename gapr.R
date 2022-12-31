library(tidyverse)
library(dplyr)
library(tidyr)
library(gapminder)


# summarize (...)
# plot (...)
# fit ()
# predict()

summarize <- function(x,
                      filter_var,
                      filter_value,
                      group_var,
                      filter_func = (`==`),
                      summary_func = median) {
    x |>
        dplyr::filter({{ filter_var }} |> filter_func(filter_value)) |>
        dplyr::group_by({{ group_var }}) |>
        dplyr::select(year:gdpPercap) |>
        dplyr::summarize_all(.tbl=_, summary_func) 
}

s <- summarize(gapminder,
          continent, "Europe",
          year,
          filter_func = (`==`),
          summary_func = mean); s
