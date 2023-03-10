# These are not unit tests
dat <- load_gapr()
summarize_gapr(dat, lifeExp, 50, country, filter_func = (`<`))

plot_gapr(dat, var=continent, val=c("Asia", "Europe", "Africa"))
plot_gapr(dat, var=country, val=c("China", "Ireland", "Nigeria"))

dat <- load_gapr("country", "Ireland")
mod1 <- fit(dat)
mod2 <- fit(dat, effects = "mixed")
plot(mod1)
plot(mod2)

