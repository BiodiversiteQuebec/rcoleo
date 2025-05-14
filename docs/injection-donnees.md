# Injecter de données dans Coleo

Ce tutoriel présente le processus pour injecter un jeu de données complet dans la base de données Coléo. L'exemple présenté est le même pour tout type d'inventaire ainsi que les cellules et sites.

Le processus d'injection requiert des données saisies dans un gabarit d'injection et comprends trois étapes : (1) le chargement des données, (2) la validation des données et (3) l'injection des données. Ces étapes sont présentées dans les sections suivantes.


## 0. Formater le jeu de données

Les données doivent d'abord être saisies dans un **gabarit**. Les gabarits sont spécifiques à chaque inventaire. 
Le gabarit d'injection de données est un fichier Excel qui contient des colonnes pour chaque champ requis par la base de données. Il est important de suivre le format du gabarit pour que les données soient correctement injectées.

Complétez la [liste de vérification](checklist.md) pour vous assurer que les données sont prêtes à être injectées.

## 1. Charger le jeu de données

La commande `coleo_read` est utilisée pour charger les données du gabarit excel. Elle charge et formate les données du gabarit en un dataframe qui est prêt à être injecté dans la base de données.

Notez que l'injection de cellules requiert un shapefile plutôt que le gabarit Excel standard. Pour structurer et formater le fichier, veillez vous référer au [gabarit d'injection de cellules](gabarits/COLEO_cellules_template.xlsx).

```r
data <- coleo_read(fileName)
```

La commande `coleo_read` prend un argument `fileName` qui spécifie le chemin d'accès local vers le gabarit contenant les données.

Une fois les données chargées, il faut les valider avant de lancer la procédure d'injection.


## 2. Valider le jeu de données

On utilise la commande `coleo_validate` pour valider les données. Cette commande vérifie que les données sont dans le bon format et qu'elles sont complètes. Elle retourne un message d'erreur si les données contiennent une erreur.

Certains inventaires sont injéectés avec des fichier médias. Dans ces cas, le chemin d'accès vers dossier contenant les médias à injecter doit être passé avec l'argument `media_path`.

```r
data_validated <- coleo_validate(data, media_path = NULL)
```

Il est à noter que tous les jeux de données requierent que les *cellules* et les *sites* aient déjà été injectés. Le processus d'injection des cellules et sites suivent le même processus d'injection.


## 3. Injecter les données (téléversement)

On utilise la commande `coleo_inject` pour exécuter la procédure d'injectection des données dans la base de données Coléo. Cette commande retourne un message indiquant le nombre de lignes s'ayant injecté avec succès et erreur ainsi que le jeu de données initial avec les colonnes `_id` et `_error` pour chaque table injectée.

L'argument `media_path` est requis lorsqu'il y a des fichiers médias à injecter. Il doit être le chemin local vers les fichiers médias à injecter.

```r
data_injected <- coelo_inject(data_validated, media_path = NULL)
```

Des messages sont produits pour indiquer les données qui ont été injectées avec succès et celles qui ont échouées. Les données qui ont échoué peuvent être retrouvées dans l'objet `data_injected` puisqu'elles ont une valeur `NULL` dans la colonne `*_id` qui leur est associée.

On peut consulter les message d'erreur puisqu'ils sont sauvés dans la colonne `*_error` du dataframe `data_injected`.

En cas d'erreur, se référer à la personne ressource.

**C'EST TOUT !**
