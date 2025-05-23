% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coleo_validate.R
\name{coleo_validate_diagnostics}
\alias{coleo_validate_diagnostics}
\title{Validation de la présence de valeurs NA dans les colonnes de date}
\usage{
coleo_validate_diagnostics(data, cols_date, no_obs = 0)
}
\arguments{
\item{data}{Le dataframe contenant les données à injecter.}

\item{cols_date}{Les noms des colonnes contenant des dates.}

\item{no_obs}{Le nombre d'observations sans taxon.}
}
\value{
Le message de validation.
}
\description{
Validation de la présence de valeurs NA dans les colonnes de date
}
