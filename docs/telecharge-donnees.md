# Télécharger des données de Coleo

Il est possible de télécharger les données de coleo directement depuis R. Pour cela, il faut utiliser la fonction `coleo_request_general`.

Pour télécharger les données, il faut utiliser la commande `coleo_request_general` et spécifier la table à télécharger ou la fonction à appeler. 


## Télécharger des données d'une table de Coléo

La commande `coleo_request_general` prend trois arguments pour télécharger desdonnées d'uen table (ou d'une View) de Coléo : d'abord un endpoint (le nom de la table), `response_as_df = TRUE` pour que la commande retourne la requête dans une table et le nom du schéma à l'argument `schéma`. Il est aussi possible d'ajouter des paramètres à la requête pour préciser les critères de recherche.

Par exemple, pour télécharger les données de la table `cells`, il faut utiliser l'endpoint `cells`. Si aucun paramètre suuplémentaire n'est passé à la commande, toutes les entrées de la table sont retournées. **ATTENTION !!!** Cela peut représenter beaucoup de données.

Voici un exemple de téléchargement de l'ensemble des données de la table `cells` :

```r
cells <- coleo_request_general("cells", response_as_df = TRUE, schema = "public")
```

On peut rafiner notre recherche en spécifiant des paramètres pour préciser les données à télécharger.

Voici un exemple de téléchargement des données de la table `cells` avec le paramètre `cell_code` qui permet de spécifier le code de la cellule à télécharger :

```r
cell_105_101 <- coleo_request_general("cells", response_as_df = TRUE, schema = "public", cell_code = "eq.105_101")
```

Notez que les paramètres passés doivent respecter la nomenclature des api `postgREST`. Ainsi, pour une équivalence comme `cell_code = "eq.105_101"` il faut ajouter `eq.` avant la valeur.


## Télécharger des données d'une fonction de Coléo

Les requêtent sur des fonctions requièrent un format différent celles sur des tables. Les arguments demeurent les mêmes, mais `rpc/` doit être ajouté avant le nom de la fonction. 

Voici un exemple de téléchargement des colonnes de la table `cells` en utilisant la fonction `table_columns` :

```r
cells_columns <- coleo_request_general('rpc/table_columns', response_as_df = TRUE, 'table_name' = 'cells')
```