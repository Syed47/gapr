
# These are not unit tests
dat <- load_gapr(summarize_over = "country", value = "Ireland")
mod <- fit(dat, fit_type = "lm")
plot(mod)
