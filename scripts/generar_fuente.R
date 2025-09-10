library(dplyr)
library(data.table)
library(optparse)
box::use(magrittr[`%>%`])

main <- function(pais, tipo_variable) {
  variable_df <- list.files(here::here("data", "interim", "variables", tipo_variable, pais), full.names = T) %>%
    lapply(fread) %>%
    Reduce(cbind, .) %>%
    select(which(!duplicated(names(.))))
  
    out_dir <- here::here("data", "processed", "variables", pais)
    out_file <- here::here(out_dir, glue::glue("{tipo_variable}.csv"))
    dir.create(out_dir, showWarnings = F, recursive = T)
    variable_df |> readr::write_csv(out_file)
}

option_list <- list(
  make_option("--pais", type="character", dest="pais"),
  make_option("--tipo-variable", type="character", dest="tipo_variable")
)
opt <- parse_args(OptionParser(option_list=option_list))
list2env(opt, envir = .GlobalEnv)
main(pais, tipo_variable)
