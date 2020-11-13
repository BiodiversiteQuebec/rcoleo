# based on advice from rOpenSci https://ropensci.org/technotes/2019/12/08/precompute-vignettes/

library(knitr)

# Téléchargement
knit("vignettes/telecharge-sites.Rmd.orig", "vignettes/telecharge-sites.Rmd")

# Injections TABLES
knit("vignettes/injection-taxa.Rmd.orig", "vignettes/injection-taxa.Rmd")
knit("vignettes/injection-cellules.Rmd.orig", "vignettes/injection-cellules.Rmd")
knit("vignettes/injection-sites.Rmd.orig", "vignettes/injection-sites.Rmd")

# Injections CAMPAGNES
knit("vignettes/tutoriel-injections.Rmd.orig", "vignettes/tutoriel-injections.Rmd`")
knit("vignettes/CAMPAGNE_zooplancton_injection.Rmd.orig", "vignettes/CAMPAGNE_zooplancton_injection.Rmd")

