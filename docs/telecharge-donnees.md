# Télécharger des données de Coleo

Il est possible de télécharger les données de coleo directement depuis R. Pour cela, il faut utiliser les fonctions `coleo_request_data` et `coleo_request_general` de la librairie `rcoleo`.

Il est attendu que l'utilisateur aie déjà installé la librairie `rcoleo` et sauvé son jeton d'accès (token) en tant que variable d'environnement. Au besoin, se référer à la [procédure](https://github.com/ReseauBiodiversiteQuebec/rcoleo/blob/master/README.md).


## Télécharger les données d'inventaires terrain

La voie principale de téléchargement des données d'inventaires est assurée par la commande R `coleo_request_data`. Cette fonction R donne accès aux données affichées sur les tableaux du portail web de [Biodiversité Québec](https://biodiversite-quebec.ca/fr/inventaires) et spécifier le type d'inventaire. 

La fonction prend deux arguments :

- `survey_type` Type d'inventaire à télécharger. Les types d'inventaires supportés sont 'vegetation', 'acoustique_anoures', 'acoustique_chiropteres', 'acoustique_oiseaux', 'acoustique_orthopteres' et 'adne_corrige', 'benthos', 'decomposition_sol', 'insectes_sol', 'mammiferes', 'odonates', 'papilionides', 'physicochimie_terrain', 'zooplancton'.
- `view` Type de vue à télécharger. Les options sont : `short` et `long`. `short` retourne une table correspondant à la vue 'SIMPLIFIÉ' des données disponibles sur les inventaires terrain du portail web de Biodiversité Québec alors que la vue `long` retourne une table correspondant à la vue 'COMPLET'.

```r
# Requête pour les inventaires de type 'végétation'
response <- coleo_request_data(survey_type = 'vegetation', view = 'short')
```


## Télécharger les données depuis un endpoint particulier

***L'accès à certains endpoints est restraint aux utilisateurs détenant des privilèges spécifiques***

La fonction `coleo_request_general` permet le téléchargement de données de tables, de view et de fonctions depuis l'api de COLEO. Elle prend trois arguments pour retourner des données : 

- `endpoint` Nom du endpoint de l'API de coleo sur lequel la requête doit être effectuée. Si la requête est faite sur une fonction, il est nécessaire d'ajouter 'rpc/' devant le nom de la fonction.
- `response_as_df = TRUE` FALSE par défaut. Retroune un data.frame si TRUE.
- `schema` 'api' par défaut. Schéma qui contient la fonction ou table passée à l'argument endpoint.

Il est aussi possible d'ajouter des paramètres à la requête pour préciser les critères de recherche. 

Par exemple, pour télécharger les données de la table `cells`, il faut utiliser l'endpoint `cells`. Si aucun paramètre suplémentaire n'est passé à la commande, toutes les entrées de la table sont retournées. **ATTENTION !!!** Cela peut représenter beaucoup de données.

Voici un exemple de téléchargement de l'ensemble des données de la table `cells` :

```r
cells <- coleo_request_general(endpoint = "cells", response_as_df = TRUE, schema = "public")
```

On peut rafiner notre recherche en spécifiant des paramètres pour préciser les données à télécharger.

Voici un exemple de téléchargement des données de la table `cells` avec le paramètre `cell_code` qui permet de spécifier le code de la cellule à télécharger :

```r
cell_105_101 <- coleo_request_general("cells", response_as_df = TRUE, schema = "public", cell_code = "eq.105_101")
```

Notez que les paramètres passés doivent respecter la nommenclature des api `postgREST`. Ainsi, pour une équivalence comme `cell_code = "eq.105_101"` il faut ajouter `eq.` avant la valeur.


## Télécharger des données d'une fonction de Coléo

Les requêtent sur des fonctions requièrent un format différent de celles sur des tables. Les arguments demeurent les mêmes, mais `rpc/` doit être ajouté avant le nom de la fonction. 

Voici un exemple de téléchargement des colonnes de la table `cells` en utilisant la fonction `table_columns` :

```r
cells_columns <- coleo_request_general('rpc/table_columns', response_as_df = TRUE, 'table_name' = 'cells')
```