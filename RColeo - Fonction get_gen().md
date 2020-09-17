# RColeo - Test de la fonction get_gen() et de la validité des endpoints

Rappel
*Champs communs à la plupart des tables dans COLEO* (champs en gras sont obligatoires)

Champs | Type | Description | Options
------------ | ------------- | ------------- | -------------
**id** | nombre entier | Identifiant unique | |
created_at | date-heure | Date et heure de création | |
updated_at | date-heure | Date et heure de mise à jour | |

## endpoints = "/cells"
Point d'accès: /api/v1/cells
Connexion à la base de données: Valide
Similaire à la table cells dans COLEO: Non
Sortie de la requête: 
     - $body[[1:7]] = 768 entrées
     - 8 champs (en italique = champs générée par la BD; en gras = champs en plus que ceux trouvés dans la table de la BD)
Champs | Type | Description | Options
------------ | ------------- | ------------- | -------------
name | texte | Nom de la cellule | |
cell_code | texte | Code de la cellule | |
geom | geometry | Localisation de la cellule | |
*id* | | nombre entier | Identifiant unique | |
*created_at* | date-heure | Date et heure de création | |
*updated_at*| date-heure | Date et heure de mise à jour | |
**sites**| | | |
**geom.type**| | | |
**geom.coordinates**| | | |

***

## endpoints = "/sites"
Point d'accès: /api/v1/sites
Connexion à la base de données: Valide
Similaire à la table cells dans COLEO: Non
Sortie de la requête: 
     - $body[[1]] = 68 entrées
     - 19 champs (en italique = champs générée par la BD; en gras = champs en plus que ceux trouvés dans la table de la BD)
Champs | Type | Description | Options
------------ | ------------- | ------------- | -------------
cell_id | nombre entier | Identifiant de la cellule | |
off_station_code_id | texte |  | |
site_code | texte | Identifiant unique du site | |
type | choix | Type d'inventaire réalisé sur le site | 'lac', 'rivière', 'forestier', 'marais', 'marais côtier', 'toundrique', 'tourbière' |
opened_at | date | Date de l'ouverture du site | |
--geom-- | geometry | Localisation du site | |
notes | texte | Commentaires | |
*id* | | nombre entier | Identifiant unique | |
*created_at* | date-heure | Date et heure de création | |
*updated_at*| date-heure | Date et heure de mise à jour | |
**campaigns** | | | |
**geom.type** | | | |
**geom.coordinates** | | | |
**cell.id** | | | |
**cell.name** | | | |
**cell.cell_code** | | | |
**cell.created_at** | | | |
**cell.updated_at** | | | |
**cell.geom.type** | | | |
**cell.geom.coordinates** | | | |

***

## endpoints = "/campaigns"
Point d'accès: /api/v1/campaigns
Connexion à la base de données: Valide
Similaire à la table cells dans COLEO: Non
Sortie de la requête: 
     - $body[[1]] = 80 entrées
     - 34 champs (en italique = champs générée par la BD; en gras = champs en plus que ceux trouvés dans la table de la BD)
Champs | Type | Description | Options
------------ | ------------- | ------------- | -------------
site_id | texte | Identifiant unique du site attaché à la campagne d'échantillonnage | |
type | choix | Le type campagne réalisé | 'végétation', 'végétation_transect', 'sol', 'acoustique', 'phénologie', 'mammifères', 'papilionidés', 'odonates', 'insectes_sol', 'ADNe','zooplancton', 'température_eau', 'température_sol', 'marais_profondeur_température' |
technicians | ARRAY(texte) | Noms des technicien(ne)s | |
opened_at | date | Date d'ouverture de la campagne d'échantillonnage | |
closed_at | date | Date de fermeture de la campagne d'échantillonnage | |
notes | texte | Commentaires | |
*id* | | nombre entier | Identifiant unique | |
*created_at* | date-heure | Date et heure de création | |
*updated_at*| date-heure | Date et heure de mise à jour | |
**efforts** | | | |
**lures** | | | |
**landmarks** | | | |
**traps** | | | |
**environment.id** | | | |
**environment.campaign_id** | | | |
**environment.wind** | | | |
**environment.sky** | | | |
**environment.temp_c** | | | |
**environment.notes** | | | |
**environment.created_at** | | | |
**environment.updated_at** | | | |
**device.id** | | | |
**device.campaign_id** | | | |
**device.sd_card_codes** | | | |
**device.cam_code** | | | |
**device.cam_h_cm **| | | |
**device.mic_logger_code** | | | |
**device.mic_acc_code** | | | |
**device.mic_h_cm_acc** | | | |
**device.mic_ultra_code** | | | |
**device.mic_h_cm_ultra** | | | |
**device.mic_orientation** | | | |
**device.created_at** | | | |
**device.updated_at** | | | |

***

## endpoints = "/efforts"
Point d'accès: /api/v1/efforts
Connexion à la base de données: Valide
Similaire à la table cells dans COLEO: Oui
Sortie de la requête: 
     - $body[[1:2]] = 110 entrées
     - 10 champs (en italique = champs générée par la BD; en gras = champs en plus que ceux trouvés dans la table de la BD)
Champs | Type | Description | Options
------------ | ------------- | ------------- | -------------
campaing_id | nombre entier | Numéro d'identification de la campagne | |
stratum | choix | Strate de végétation concernée par l'effort d'échantillonage | 'arbres', 'arbustes/herbacées', 'bryophytes' |
time_start | date et heure | Date et heure de début de l'inventaire | |
time_finish | date et heure | Date et heure de fin de l'inventaire | |
samp_surf | nombre décimal| Taille de la surface d'échantillonage | |
samp_surf_unit | choix | Unité de mesure utilisé pour la surface d'échantillonnage | 'cm2', 'm2', 'km2' |
notes | texte | Commentaires | |
*id* | | nombre entier | Identifiant unique | |
*created_at* | date-heure | Date et heure de création | |
*updated_at*| date-heure | Date et heure de mise à jour | |

***

## endpoints = "/environment"
Point d'accès: /api/v1/environment
Connexion à la base de données: Valide
Similaire à la table cells dans COLEO: Non
Sortie de la requête: 
     - $body[[1]] = 29 entrées
     - 8 champs (en italique = champs générée par la BD; en gras = champs en plus que ceux trouvés dans la table de la BD)
Champs | Type | Description | Options
------------ | ------------- | ------------- | -------------
campaing_id | nombre entier | Numéro d'identification de la campagne | |
wind | choix | Vent en km/h | 'calme (moins de 1 km/h)', 'très légère brise (1 à 5 km/h)', 'légère brise (6 à 11 km/h)', 'petite brise (12 à 19 km/h)', 'jolie brise (20 à 28 km/h)' |
sky | choix | Allure du ciel | 'dégagé (0 à 10 %)', 'nuageux (50 à 90 %)', 'orageux', 'partiellement nuageux (10 à 50 %)', 'pluvieux' |
temp_c | nombre décimal | Date et heure de fin de l'inventaire | |
--samp_surf-- | nombre décimal| Température en celsius | |
--samp_surf_unit-- | choix | Unité de mesure utilisé pour la surface d'échantillonnage | 'cm2', 'm2', 'km2' |
notes | texte | Commentaires | |
*id* | | nombre entier | Identifiant unique | |
*created_at* | date-heure | Date et heure de création | |
*updated_at*| date-heure | Date et heure de mise à jour | |

***

## endpoints = "/devices"
Point d'accès: /api/v1/devices
Connexion à la base de données: Valide
Similaire à la table cells dans COLEO: Oui
Sortie de la requête: 
     - $body[[1]] = 12 entrées
     - 13 champs (en italique = champs générée par la BD; en gras = champs en plus que ceux trouvés dans la table de la BD)
Champs | Type | Description | Options
------------ | ------------- | ------------- | -------------
campaing_id | nombre entier | Numéro d'identification de la campagne | |
sd_card_codes | ARRAY(texte) | Numéro d'identification des cartes SD utilisées |  |
cam_code | ARRAY(texte) | Numéro d'identification de la caméra utilisée |  |
cam_h_cm | nombre décimal | Hauteur de la camera en centimètres | |
mic_logger_code | texte| Numéro d'identification du enregistreur utilisé | |
mic_acc_code | texte | Numéro d'identification du microphone accoustique utilisé | |
mic_h_cm_acc | nombre décimal | Hauteur du microphone ultrason utilisé en centimètres | |
mic_ultra_code | texte | Hauteur du microphone ultrason utilisé en centimètres | |
mic_orientation | choix | Orientation du dispositif | 'n', 's', 'e', 'o', 'ne', 'no', 'se', 'so' |
*id* | | nombre entier | Identifiant unique | |
*created_at* | date-heure | Date et heure de création | |
*updated_at*| date-heure | Date et heure de mise à jour | |