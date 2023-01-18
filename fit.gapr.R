dat <- gapminder::gapminder

fit <- function(x, type = c("lm", "glm", "random")) {
  UseMethod("fit")
}


fit.gapr <- function(x, fit_type = c("lm", "glm", "random")) {

  fit_type <- match.arg(fit_type)

  mod <- switch(fit_type,
                lm = {lm(lifeExp ~ pop + gdpPercap, data=gap)},
                glm = {glm(lifeExp ~ pop + year +  gdpPercap , data=gap)},
                random = {lm(lifeExp ~ pop + (1|gdpPercap),
                                     data = gap)})
  print(mod)
  output <- list(model = mod, data = dat, fit_type = fit_type)
  attr(output, "source") <- attr(x, "source")
  class(output) <- c("fit.gapr", "listof")
  invisible(output)
}

model1 <- fit.gapr(dat)

model2 <- fit.gapr(dat, "glm")

model3 <- fit.gapr(dat, "random")

