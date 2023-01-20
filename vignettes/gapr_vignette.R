## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages('devtools')
#  devtools::install_github('Syed47/gapr')

## -----------------------------------------------------------------------------
library(gapr)

## -----------------------------------------------------------------------------
Ireland <- load_gapr("country", "Ireland")
Ireland$data

## -----------------------------------------------------------------------------
dat <- load_gapr()
# !!!
# this function somehow works when only this block is executed but shows error 
# "not exported by gapr", when knitting, even though in the NAMESPACE file 
# it can be seen that it is exported.
# It' weird that it works sometime but not always
summarize_gapr(dat, lifeExp, 50, country, filter_func = (`<`))

## -----------------------------------------------------------------------------
lmmod <- fit(Ireland)

## -----------------------------------------------------------------------------
randmod <- fit(Ireland, effects="mixed")

## -----------------------------------------------------------------------------
plot_gapr(load_gapr(), country, c("China", "Ireland", "Germany"))

## -----------------------------------------------------------------------------
plot(randmod)

