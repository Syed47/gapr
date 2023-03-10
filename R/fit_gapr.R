#' Fit statistical models to gapminder data
#'
#' Fits linear regression models with fixed and random effects to the gapminder data.
#' @param obj An object of class \code{"gapr"} generated by \code{\link{load_gapr}}.
#' @param effects Specify what kind of effects to be used in the model.
#' @return An object of class \code{"gapr_fit"} which includes the model details
#' as well as the data set and the kind of \code{effects} used.
#'
#' @note A dedicated \code{fit.gapr} method is provided
#' for objects of class \code{"gapr_fit"}
#' .
#' @seealso \code{\link{load_gapr}}
#'
#' @importFrom stats "lm" "glm"
#' @author Michael Doherty - <\email{michael.doherty.2019@@mumail.ie}>
#' @examples
#' dat <- load_gapr("country", "Ireland")
#' mod1 <- fit(dat)
#' mod2 <- fit(dat, effects = "mixed")
#' plot(mod1)
#' plot(mod2)
#' @export
fit <- function(obj, effects=c("fixed", "mixed")) {
  UseMethod("fit")
}

#' @export
fit.gapr <- function(obj, effects=c("fixed", "mixed")) {

  effects <- match.arg(effects)
  gm <- obj$data
  mod <- switch(effects,
                fixed = {
                  gm |> stats::lm(lifeExp ~ year + pop + gdpPercap, data=_)
                },
                mixed = {
                  gm |> stats::glm(lifeExp ~ year + (1|pop) + (1|gdpPercap), data=_)
                  # gm |> lme4::lmer(lifeExp ~ year + (1|country), data = _)
                })
  output <- list(model = mod,
                 data = gm,
                 effects = effects)
  attr(output, "source") <- attr(obj, "source")
  class(output) <- c("gapr_fit", "listof")
  invisible(output)
}


