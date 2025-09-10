library(dplyr)
source("utils.R")
box::use(here[here])
box::use(magrittr[`%>%`])

main <- function() {
  variables_filepaths <- list.files(here("data", "raw", "uy", "pymes"), full.names = T)
  variables_df <- lapply(variables_filepaths, \(filepath) {
    readxl::read_xlsx(filepath) %>%
      rename((!!unique(.$Indicador)) := Valor, anio = Año, tramo = Tamaño) %>%
      rename_with(sanitizar) %>%
      select(-indicador, -sector)
  }) %>%
    Reduce(left_join, .) %>%
    mutate(across(everything(), ~ if_else(as.character(.x) == "SIN DATOS", NA, as.character(.x)))) %>%
    mutate(across(everything(), .fns= readr::parse_guess))
  
  out_dir <- here::here("data", "processed", "variables", "uy")
  out_file <- here::here(out_dir, glue::glue("pymes.csv"))
  dir.create(out_dir, showWarnings = F, recursive = T)
  variables_df |> readr::write_csv(out_file)
}

main()