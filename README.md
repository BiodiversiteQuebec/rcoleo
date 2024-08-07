# Documentation du paquet `rcoleo` <img src="man/figures/logo.svg" width="130" height="150" align="right"/>

<!-- badges: start -->
[![R build status](https://github.com/ReseauBiodiversiteQuebec/rcoleo/workflows/R-CMD-check/badge.svg)](https://github.com/ReseauBiodiversiteQuebec/rcoleo/actions)
<!-- badges: end -->

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

`rcoleo` un paquet R permettant le retrait et l'analyse des données collectées dans le cadre du programme de suivi de la biodiversité (BdQc).

Ce paquet R expose les services `postgREST` de l'API de Coléo. Coléo est un système d'information sur la biodiversité du Québec développé par le laboratoire d'Écologie Intégrative de l'Université de Sherbrooke.


## Installer le paquet `rcoleo`

```r
devtools::install_github("BiodiversiteQuebec/rcoleo")
```

## S'authentifier auprès de l'API

Il est **fortement recommandé** d'enregistrer le jeton d'accès directement en tant que _Environment variable_ afin de s'assurer qu'il ne soit pas visible ou transmis avec votre code à un autre utilisateur. Ce jeton d'accès est unique et révocable. 

```r
file.edit("~/.Renviron")
```

cette linge va ouvrir un ficher text dans votre Rstudio. Ajoutez à ce ficher la ligne suivante:


```r
RCOLEO_TOKEN=7f8df438e1be96a18436e9dab5d97d68ed0e0441d9b68f59e0ce631b2919f3aa
```

(utilizez votre propre token ici)

*Le jeton d'accès est un exemple ici et n'est aucunement valide.*


## Injecter de données dans Coléo

[Accédez au tutoriel d'injection.](docs/injection-donnees.md)


## Télécharger des données de Coléo

[Accédez au tutoriel de téléchargement.](docs/telecharge-donnees.md)
