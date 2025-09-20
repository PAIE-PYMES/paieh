# libraries
library(tidyverse)
library(openxlsx2)
library(janitor)

#data

temp <- tempfile(fileext = ".xlsb")
download.file("https://www.sii.cl/sobre_el_sii/empresas/PUB_TRAM5_REG.xlsb",
              destfile = temp, mode = "wb")

wb <- wb_load(temp)

raw <- wb_read(wb, sheet = "Datos")

raw <- raw %>% clean_names()

#transformaciones


df1 <- raw %>% filter(region_del_domicilio_o_casa_matriz != "Sin Información")

df1 <- df1 %>% mutate(ventas_anuales_en_uf = as.numeric(ventas_anuales_en_uf),
                      renta_neta_informada_en_uf_trabajadores_de_genero_femenino = as.numeric(renta_neta_informada_en_uf_trabajadores_de_genero_femenino),
                      renta_neta_informada_en_uf_trabajadores_de_genero_masculino = as.numeric(renta_neta_informada_en_uf_trabajadores_de_genero_masculino),
                      honorarios_pagados_informados_en_uf = as.numeric(honorarios_pagados_informados_en_uf))

df2 <- df1 %>% 
  group_by(ano_comercial, region_del_domicilio_o_casa_matriz) %>%
  mutate(contribucion_po = numero_de_empresas / sum(numero_de_empresas),
            contribucion_empleo = (numero_de_trabajadores_dependientes_informados + numero_de_trabajadores_a_honorarios_informados)/
            sum(numero_de_trabajadores_dependientes_informados + numero_de_trabajadores_a_honorarios_informados),
            contribucion_vtas = ventas_anuales_en_uf / sum(ventas_anuales_en_uf),
            contribucion_sal_neto = renta_neta_informada_en_uf_trabajadores_de_genero_femenino+
                                    renta_neta_informada_en_uf_trabajadores_de_genero_masculino+
                                    honorarios_pagados_informados_en_uf/
                                    sum(renta_neta_informada_en_uf_trabajadores_de_genero_femenino+
                                        renta_neta_informada_en_uf_trabajadores_de_genero_masculino+
                                        honorarios_pagados_informados_en_uf),
            sal_neto =renta_neta_informada_en_uf_trabajadores_de_genero_femenino+
                      renta_neta_informada_en_uf_trabajadores_de_genero_masculino+
                      honorarios_pagados_informados_en_uf,
            trabajadores = numero_de_trabajadores_dependientes_informados + numero_de_trabajadores_a_honorarios_informados) %>% 
  ungroup() %>% 
  select(ano_comercial,
         region_del_domicilio_o_casa_matriz,
         tramo_segun_ventas_5_tramos,
         contribucion_po,
         contribucion_empleo,
         contribucion_vtas,
         contribucion_sal_neto,
         numero_de_empresas,
         trabajadores,
         ventas_anuales_en_uf,
         sal_neto)

write_csv(df2,"../data/interim/variables/pymes/chl/raw-chl-v1.csv")

