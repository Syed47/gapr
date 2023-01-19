# added to account for non-standard evaluation employed by tidyverse code
# this removes the "fun_name: no visible binding for global variable var_name" notes from devtools::check()
utils::globalVariables(c("overall" ,"country", "continent", "lm", "loess", "random"))
