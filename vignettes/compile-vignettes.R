# based on advice from rOpenSci https://ropensci.org/technotes/2019/12/08/precompute-vignettes/

library(knitr)
knit("vignettes/injection-taxa.Rmd.orig", "vignettes/injection-taxa.Rmd")

knit("vignettes/telecharge-sites.Rmd.orig", "vignettes/telecharge-sites.Rmd")
