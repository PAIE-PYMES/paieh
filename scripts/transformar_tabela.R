library(tidyverse)
library(readxl)
library(optparse)
source("utils.R")
box::use(glue[glue])

main <- function(filename, n_sheets) {
  for(i in seq_len(n_sheets)) {
    tabela_df <- read_xlsx(here::here(glue("data/raw/br/pymes/{filename}")), glue("Tabela {i}"), .name_repair = "unique_quiet")
    variable_i <- sanitizar(tabela_df |> slice(1) |> pull(1) |> str_split_i("Variável - ", 2))
    
    variable_df <- 
      # 1. Generamos un DF que contenga la apertura de anio y sector
      tabela_df |> 
      select(-1, -2) |>
      slice(3:4) |>
      janitor::row_to_names(1) |>
      suppressWarnings() |>
      janitor::clean_names() |>
      pivot_longer(everything(), names_to="anio", values_to="sector") |>
      mutate(anio = if_else(startsWith(anio, "x"), str_replace(anio, "x", ""), NA_character_)) |>
      fill(anio, sector, .direction="down") |>
      unique() |>
      # 2. Le asignamos un group id que nos permitira unir la información de la variable por región con ésta apertura generada
      mutate(group = row_number()) %>%
      # 3. Luego, hacemos coincidir el dataframe aperturado por anio y sector contra el generado a partir de la región, variable y su valor
      inner_join(
        tabela_df |>
        slice(6:n()-1) |>
        select(-2) |>
        mutate(across(1, ~ replace_na(.x, "region"))) |>
        janitor::row_to_names(1) |>
        suppressWarnings() |>
        pivot_longer(
          !region,
          names_to="tramo",
          values_to=variable_i
        ) |>
        mutate(
          {{variable_i}} := suppressWarnings(as.integer(!!as.name(variable_i))),
          # Cada 336 filas (#anios * #sectores) reiniciamos el ciclo 1..336 para la asignación de un grupo
          # Además, un id de grupo corresponde a un bloque de 9 filas (es decir: cada id correspondera a 9 filas consecutivas correspondientes al #tramos de empresa)
          group = (( (row_number() - 1) %/% 9 ) %% 336) + 1
        ),
        by = "group"
      ) |>
      select(-group)
    
    out_dir <- here::here("data", "interim", "variables", "pymes", "br")
    out_file <- here::here(out_dir, glue::glue("{variable_i}.csv"))
    dir.create(out_dir, showWarnings = F, recursive = T)
    variable_df |> readr::write_csv(out_file)
  }
}

option_list <- list(
  make_option("--filename", type="character", dest="filename"),
  make_option("--n-tabs", type="integer", dest="n_tabs")
)
opt <- parse_args(OptionParser(option_list=option_list))
list2env(opt, envir = .GlobalEnv)
main(filename, n_tabs)
