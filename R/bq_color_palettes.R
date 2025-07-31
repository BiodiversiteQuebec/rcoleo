
#' Retourne un vecteur de valeurs hex pour les couleurs Biodiversité Québec
#'
#' @return Un vecteur de valeurs hexadécimales pour les couleurs Biodiversité Québec.
#' @export
#'
bq_colors <- function() {
  list(
  vert_base='#2e483e',
  jaune_base='#e0b658',
  bleu_base='#7bb5b1',
  orange_base='#a6612d',
  beige_base='#fcf8ee',
  gris_base='#7a7a7a',
  vert_secondaire='#4a6b5c',
  jaune_secondaire='#f0d28c',
  bleu_secondaire='#a0c4c3',
  orange_secondaire='#c88f6b',
  beige_secondaire='#e0c97b',
  gris_secondaire='#a0a0a0',
  vert_fonce='#061f19',
  jaune_fonce='#d88219',
  bleu_fonce='#3e8986',
  orange_fonce='#8d4719',
  beige_fonce='#f1dca9',
  gris_fonce='#5a5a5a',
  vert_clair='#c9d2d0',
  jaune_clair='#fbedd5',
  bleu_clair='#d5edec',
  orange_clair='#f4d7c6',
  beige_clair='#fefdfb',
  gris_clair='#c4c4c4')
}

#' Retourne un vecteur de valeurs hex pour des couleurs Biodiversité Québec qualitative
#'
#' @param size nombre de couleurs à retourner, par défaut 5
#' @return vecteur de couleurs hexadécimales
#' @examples library(scales); show_col(bq_pal_qual(8))
#' @export
#'
bq_pal_qual <- function(size=5) {
  unlist(unname(bq_colors()[seq(1,size)]))
}

#' Retourne un vecteur de valeurs hex pour des couleurs Biodiversité Québec quantitative
#' @param color couleur de base, par défaut "orange"
#' @param size nombre de couleurs à retourner, par défaut 10
#' @return vecteur de couleurs hexadécimales
#' @examples library(scales); show_col(bq_pal_quant_mono("orange", 10))
#' @export
bq_pal_quant_mono <- function(color="orange",size=10){
  colorRampPalette(c(bq_colors()[[paste0(color, "_clair")]], 
                     bq_colors()[[paste0(color, "_base")]],
                     bq_colors()[[paste0(color, "_fonce")]]))(size)
}

#' Retourne un vecteur de valeurs hex pour des couleurs Biodiversité Québec quantitative
#' @param color1 couleur de base 1. "vert", "bleu", "orange", "gris", "beige" par défaut "vert"
#' @param color2 couleur de base 2, par défaut "bleu"
#' @param ton ton de couleur. Base, fonce ou clair. Par défaut "base"
#' @param size nombre de couleurs à retourner, par défaut 10
#' @return vecteur de couleurs hexadécimales
#' @examples library(scales); show_col(bq_pal_quant_bi("vert", "bleu", "base", 10))
#' @examples library(scales); show_col(bq_pal_quant_bi("orange", "bleu", "fonce", 10))
#' @export
bq_pal_quant_bi <- function(color1="vert", color2="bleu", ton='base', size=10){
  colorRampPalette(c(bq_colors()[[paste0(color1, "_", ton)]], 
                     bq_colors()[[paste0(color2, "_", ton)]]))(size)
}

#' Retourne un vecteur de valeurs hex pour des couleurs Biodiversité Québec quantitative
#' @param color1 couleur de base 1. "orange", "bleu", "vert" par défaut "orange"
#' @param color2 couleur de base 2, par défaut "bleu"
#' @param color3 couleur de base 3, par défaut "vert"
#' @param ton ton de couleur. Base, fonce ou clair. Par défaut "base"
#' @param size nombre de couleurs à retourner, par défaut 10
#' @return vecteur de couleurs hexadécimales
#' @examples library(scales); show_col(bq_pal_quant_tri("orange", "bleu", "vert", "base", 10))
#' @examples library(scales); show_col(bq_pal_quant_tri("orange", "bleu", "vert", "fonce", 10))
#' @export
bq_pal_quant_tri <- function(color1="orange", color2="bleu", color3="vert", ton='base', size=10){
  colorRampPalette(c(bq_colors()[[paste0(color1, "_", ton)]], 
                     bq_colors()[[paste0(color2, "_", ton)]],
                     bq_colors()[[paste0(color3, "_", ton)]]))(size)
}