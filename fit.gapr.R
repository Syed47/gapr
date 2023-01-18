
dat <- gapminder::gapminder

fit <- function(x, type = c("lm", "glm", "random")) {
  UseMethod("fit")
}


fit_gapr <- function(x, fit_type = c("lm", "glm", "random")) {

  fit_type <- match.arg(fit_type)

  mod <- switch(fit_type,
                lm = {lm(lifeExp ~ pop + year + gdpPercap, data=gap)},
                glm = {glm(lifeExp ~ pop + year +  gdpPercap , data=gap)},
                random = {lm(lifeExp ~ pop +  year + (1|gdpPercap),
                                     data = gap)})
  print(mod)
  output <- list(model = mod, data = dat, fit_type = fit_type)
  attr(output, "source") <- attr(x, "source")
  class(output) <- c("fit.gapr", "listof")
  invisible(output)
}

lmmod <- fit_gapr(s)

glmmod <- fit_gapr(s, "glm")

randmod <- fit_gapr(s, "random")

