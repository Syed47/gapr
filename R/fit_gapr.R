library(tidyverse)

fit <- function(obj, fit_type=c("lm", "loess", "random")) {
  UseMethod("fit")
}

#' @export
fit.gapr <- function(obj, fit_type=c("lm", "loess", "random")) {

  fit_type <- match.arg(fit_type)
  gm <- obj$data
  mod <- switch(fit_type,
                lm = {
                  gm |> stats::lm(lifeExp ~ pop + year + gdpPercap, data=_)
                },
                loess = { # not done yet
                  gm |> stats::loess(lifeExp ~ pop + year + gdpPercap, data=_)
                },
                random = { # not done yet
                  gm |> stats::lm(lifeExp ~ pop + year + (1|gdpPercap), data=_)
                })
  output <- list(model = mod,
                 data = gm,
                 fit_type = fit_type)
  attr(output, "source") <- attr(obj, "source")
  class(output) <- c("gapr_fit", "listof")
  invisible(output)
}



