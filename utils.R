#' Sanitiza nombres de variables
#'
#' Esta función toma una cadena de texto y la procesa para generar un nombre
#' limpio y estandarizado que puede ser utilizado, por ejemplo, como nombre
#' de columna en un `data.frame` o `tibble`.
#'
#' El proceso de sanitización incluye:
#' - Extraer la segunda parte de la cadena dividiendo por `"Variável - "`.
#' - Reemplazar todos los espacios por guiones bajos (`_`).
#' - Eliminar paréntesis de apertura y cierre.
#' - Normalizar caracteres latinos con acentos a su forma ASCII (ej. `á` → `a`).
#' - Convertir el resultado a minúsculas.
#'
#' @param string `character(1)`  
#'   Cadena de texto a sanitizar.
#'
#' @return Un `character(1)` con la cadena sanitizada.
#'
#' @examples
#' sanitizar("Nombre (Ejemplo)")
#' # [1] "nombre_ejemplo"
#'
#' sanitizar("Código Único (ID)")
#' # [1] "codigo_unico_id"
#'
#' sanitizar("Indicador")
#' # [1] indicador
#' 
#' @export
sanitizar <- function(string) {
  library(stringr)
  return(
    string |>
      str_replace_all(" ", "_") |>
      str_replace("\\(", "") |>
      str_replace("\\)", "") |>
      stringi::stri_trans_general(id="Latin-ASCII") |>
      tolower()
  )
}
