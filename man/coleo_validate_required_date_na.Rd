% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coleo_validate.R
\name{coleo_validate_required_date_na}
\alias{coleo_validate_required_date_na}
\title{Validation de la présence de valeurs NA dans les colonnes de date}
\usage{
coleo_validate_required_date_na(data, cols_date, tbl)
}
\arguments{
\item{data}{Le dataframe contenant les données à injecter.}

\item{cols_date}{Les noms des colonnes contenant des date.}

\item{tbl}{La table de la base de données à laquelle les données seront injectées.}
}
\value{
Le message de validation.
}
\description{
Validation de la présence de valeurs NA dans les colonnes de date
}
