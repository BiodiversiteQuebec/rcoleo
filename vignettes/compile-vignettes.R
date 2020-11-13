# based on advice from rOpenSci https://ropensci.org/technotes/2019/12/08/precompute-vignettes/

library(knitr)

# Téléchargement
knit("vignettes/telecharge-sites.Rmd.orig", "vignettes/telecharge-sites.Rmd") # needs httr-oauth


# Injections TABLES
knit("vignettes/injection-taxa.Rmd.orig", "vignettes/injection-taxa.Rmd") # needs httr-oauth


# Injections CAMPAGNES

knit("vignettes/CAMPAGNE_zooplancton_injection.Rmd.orig", "vignettes/CAMPAGNE_zooplancton_injection.Rmd")

# keeping Tutorial-injections.Rmd.orig as .orig, not because it needs
# authorization or something similare, but because I'm not sure if we are
# including it right now
