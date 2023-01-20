
# These are not unit tests
dat <- load_gapr()
summary(dat, lifeExp, 50, country, filter_func = (`<`))

plot(dat, continent, c("Asia", "Europe", "Africa"))
plot(dat, country, c("China", "Ireland", "Nigeria"))

dat <- load_gapr("country", "Ireland")
mod1 <- fit(dat)
mod2 <- fit(dat, effects = "mixed")
plot(mod1)
plot(mod2)

