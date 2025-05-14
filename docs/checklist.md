# Checklist pré-injection COLEO

## Dates et heures

- [ ] Vérifier l'étendue des dates (min/max).
- [ ] Confirmer le format des dates (YYYY-MM-DD).
- [ ] Confirmer le format des heures (HH:MM:SS).
- [ ] Vérifier que tous les champs de dates et heures sont remplis.
- [ ] Valider que les dates d'observation sont dans les limites de la campagne.
- [ ] Confirmer que les années d'échantillonnage des sites correspondent au fichier de référence.

## Coordonnées

- [ ] Vérifier que toutes les coordonnées sont remplies.
- [ ] Confirmer que les coordonnées sont dans les limites du Québec.
- [ ] Confirmer que les coordonnées sont à proximité de leur cellule d'appartenance.

## Taxonomie

- [ ] Vérifier l'absence de caractères invalides dans les noms taxonomiques.
- [ ] Confirmer l'absence de taxons vides ou inconnus.
- [ ] Inspecter visuellement les noms taxonomiques pour les erreurs de saisie.

## Sites et cellules

- [ ] Confirmer que tous les sites existent dans COLEO.
- [ ] Confirmer l'année de référence pour l'échantillonnage du site.
- [ ] Vérifier la validité des cellules avec le fichier de référence.

## Format

- [ ] Vérifier que tous les noms de colonnes sont valides.
- [ ] Identifier et corriger les espaces insécables.
- [ ] Identifier et valider les champs vides.
- [ ] Confirmer que tous les champs requis sont remplis.

## Valeurs

- [ ] Vérifier les plages de valeurs (min/max).
- [ ] Identifier les valeurs aberrantes.
- [ ] Visualiser les séries temporelles pour confirmer les tendances.
- [ ] Vérifier les transitions brusques avec une fenêtre mobile.

## Médias

- [ ] Confirmer que tous les fichiers médias saisis dans le gabarit sont présents dans le dossier.

## Inventaires particuliers

- [ ] Vérifier le nombre d'inventaires par type d'effort (végétation). 
- [ ] Identifier les doublons de taxons pour une position le long d'un transect (végétation).
- [ ] Vérifier la variation intra-journalière (thermographes).
- [ ] Comparer les variations dans et hors de l'eau (thermographes).

## Validation finale

- [ ] Exécuter `coleo_validate` pour valider les données.

