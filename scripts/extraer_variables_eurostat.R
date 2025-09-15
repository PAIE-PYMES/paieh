library(eurostat)
library(dplyr)
source("utils.R")
box::use(magrittr[`%>%`])
box::use(tidyr[pivot_wider])
box::use(openxlsx[read.xlsx])

main <- function() {
  # DataFrame auxiliar para mappear codigo de indicador con acronimo utilizado en bd_size_r
  vars_mapper_df <- read.xlsx("https://ec.europa.eu/eurostat/cache/metadata/Annexes/bd_sims_an_BD-correspondence-table.xlsx", sheet=1) |>
    filter(Old.Eurobase.Table == "bd_size_r3") |>
    select(Eurobase.code.in.historical.BD.tables, Eurobase.code.in.new.tables) |>
    union(
      # Variables derivadas (creadas a partir de las variables principales (e.g: supervivencia))
      read.xlsx("https://ec.europa.eu/eurostat/cache/metadata/Annexes/bd_sims_an_BD-correspondence-table.xlsx", sheet=2, fillMergedCells = T) |>
        # La primera fila tiene una celda mergeada para los nombres de las bdd's, con esto reshapeamos
        janitor::row_to_names(row_number = 1) |> 
        filter(bd_size_r == "yes") |>
        rename_with(~ stringr::str_replace_all(.x, " ", ".")) |>
        rename_with(~ stringr::str_replace_all(.x, "\n", "")) |>
        select(Eurobase.code.in.historical.BD.tables, "Eurobase.code.in.new.tables" = "Eurobase.code.-.from.reference.year.2021.onwards")
    ) |>
    unique()
    
  # 1. Obtenemos el DF con indicadores de las empresas europeas a nivel regional y por tramo de empresa.
  business_df <- get_eurostat(id="bd_size_r3", stringsAsFactors = T) |>
    # 2. Obtenemos el country code como los dos primeros caracteres del geo
    mutate(country_code = stringr::str_sub(geo, 1, 2)) |>
    # 3. Agregamos el nombre del país y, de paso, filtramos para quedarnos únicamente con Italia y España
    inner_join(
      eu_countries |> filter(tolower(label) %in% c("italy", "spain")) |> mutate(pais = name) |> select(code, pais),
      by = join_by(country_code == code)
    ) |>
    # 4. Agregamos el nombre de la región
    inner_join(
      eurostat_geodata_60_2016 |> select(id, region = NUTS_NAME) |> tibble(),
      by = join_by(geo == id)
    ) |>
    # 5. Pasa que para una misma región existen dos códigos distintos (e.g ES13 == ES130) entonces necesitamos distinguirlos
    distinct(pais, region, TIME_PERIOD, indic_sb, sizeclas, values) |>
    # 6. Actualizamos (cuando se pueda) el nombre del indicador de codigo a acronimo (utilizado en bd_size_r)
    left_join(
      vars_mapper_df,
      by=join_by(indic_sb == Eurobase.code.in.historical.BD.tables)
    ) |>
    mutate(variables = coalesce(Eurobase.code.in.new.tables, indic_sb)) |>
    # 7. Transformamos a una hoja ordenada
    pivot_wider(
      id_cols=c(pais, region, sizeclas, TIME_PERIOD),
      names_from = variables,
      values_from = values,
    ) |>
    # 8. Sanitizamos
    rename_with(sanitizar) |>
    rename(tramo = sizeclas) |>
    mutate(tramo = str_replace_all(str_replace_all(tramo, "-", " a "), "GE", "+")) |>
    # 9. La frequencia es 'A' (Anual) por lo que podemos quedarnos solo con el año
    mutate(anio = format(time_period, "%Y")) |>
    relocate(anio, .before=time_period) |>
    select(-time_period)
  
  # 10. Por cada pais escribimos un csv
  business_df %>%
    split(f = as.factor(.$pais)) |>
    purrr::iwalk(\(df, pais) {
      pais <- tolower(pais)
      out_dir <- here::here("data", "processed", "variables", pais)
      dir.create(out_dir, showWarnings = F, recursive = T)
      readr::write_csv(df, file.path("data", "processed", "variables", pais, "pymes.csv"))
    })
}

main()