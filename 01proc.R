rm(list = ls())

# Procesamiento tasas de inasistencia año 2016 -2021

# Carga paquetes---------------------------------------------------------------
pacman::p_load(tidyverse,
               data.table,
               car,
               haven,
               sjmisc)

# Carga base de datos ---------------------------------------------------------

rend_16 <- read_dta("input/rendimiento 2016.dta")
rend_21<-fread("input/20220302_Rendimiento_2021_20220131_PRIV.csv")

# Procesar datos --------------------------------------------------------------
proc = rend_16 %>% 
  select(dependencia = cod_depe2,
         situacion_final = sit_fin_r,
         genero = gen_alu,
         ruralidad = rural_rbd,
         etnia = cod_etnia_alu,
         nacionalidad = cod_nac_alu,
         nivel = cod_ense2,
         region = cod_reg_rbd,
         run_alu, fec_ing_alu, estado_estab, cod_grado,cod_ense) %>% 
  filter(genero > 0,
         situacion_final == "P" | situacion_final == "R" | situacion_final == "Y",
         estado_estab == 1) %>% 
  mutate(dependencia = factor(case_when(dependencia == 1 | dependencia == 5 ~ "Municipal",
                                 dependencia == 2 ~ "Particular Subvencionado",
                                 dependencia == 3 ~ "Particular Pagado",
                                 dependencia == 4 ~ "CAD",
                                 TRUE ~ NA_character_),
                              levels = c("Municipal",
                                         "Particular Subvencionado",
                                         "Particular Pagado",
                                         "CAD")),
         situacion_final = car::recode(.$situacion_final,
                                     "'P' = 'Promovido';
                                      'R' = 'Reprobado';
                                      'Y' = 'Retirado'", as.factor = T, 
                                     levels = c("Promovido",
                                                "Reprobado",
                                                "Retirado")),
         educ_joven = factor(ifelse(cod_ense %in% c(110,310,410,510,610,710,810,910),"Educ nn","Adultos")),
         genero = car::recode(.$genero,
                               "1 = 'Hombre';
                                2 = 'Mujer'", as.factor = T,
                               levels = c("Hombre",
                                          "Mujer")),
        cod_grado = car::recode(.$cod_grado,
                                "1 = '1 básico';
                                 2 = '2 básico';
                                 3 = '3 básico';
                                 4 = '4 básico';
                                 5 = '5 básico';
                                 6 = '6 básico';
                                 7 = '7 básico';
                                 8 = '8 básico'",
                                levels = c("1 básico",
                                           "2 básico",
                                           "3 básico",
                                           "4 básico",
                                           "5 básico",
                                           "6 básico",
                                           "7 básico",
                                           "8 básico")),
        curso = factor(case_when(cod_ense == 110 ~ cod_grado,
                           cod_ense %in% c(310,410,510,610,810,910) & cod_grado == "1 básico" ~ "1 medio", # Primero 
                          cod_ense %in% c(310,410,510,610,810,910) & cod_grado == "2 básico" ~ "2 medio", # Segundo
                          cod_ense == 310 & cod_grado == "3 básico" ~ "3 medio HC", # Tercero HC
                          cod_ense %in% c(310,410,510,610,710,810,910) & cod_grado == "3 básico" ~ "3 medio TP", # Tercero TP
                          cod_ense == 310 & cod_grado == "4 básico" ~ "4 medio HC", # Cuarto HC
                          cod_ense %in% c(310,410,510,610,710,810,910) & cod_grado == "4 básico" ~ "4 medio TP",
                          TRUE ~ NA_character_),
                       levels = c("1 básico",
                                  "2 básico",
                                  "3 básico",
                                  "4 básico",
                                  "5 básico",
                                  "6 básico",
                                  "7 básico",
                                  "8 básico",
                                  "1 medio",
                                  "2 medio",
                                  "3 medio HC",
                                  "3 medio TP",
                                  "4 medio HC",
                                  "4 medio TP")),
        ruralidad = factor(ifelse(ruralidad == 0, "Urbano", "Rural"),
                           levels = c("Urbano",
                                      "Rural")),
        etnia = factor(ifelse(etnia %in% c(1:9), "Pertenece a etnia", "No pertenece a etnia"),
                       levels = c("No pertenece a etnia",
                                  "Pertenece a etnia")),
        nacionalidad = factor(ifelse(nacionalidad %in% c("E","N"), "Migrante", "No migrante"),
                              levels = c("Migrante",
                                         "No migrante")), 
       nivel = car::recode(.$nivel,
                            "2 = 'Enseñanza básica';
                             5 = 'Enseñanza media HC';
                             7 = 'Enseñanza media TP'", as.factor = T,
                            levels = c("Enseñanza básica",
                                       "Enseñanza media HC",
                                       "Enseñanza media TP")),
       region = car::recode(.$region, 
                            "1 = 'I. Tarapacá';
                              2 = 'II. Antofagasta';
                              3 = 'III. Atacama';
                              4 = 'IV. Coquimbo';
                              5 = 'V. Valparaíso';
                              6 = 'VI. OHiggins';
                              7 = 'VII. Maule';
                              8 = 'VIII. Bío-bío';
                              9 = 'IX. La Araucanía';
                              10 = 'X. Los Lagos';
                              11 = 'XI. Aysén';
                              12 = 'XII. Magallanes';
                              13 = 'XIII. Metropolitana';
                              14 = 'XIV. Los Ríos';
                              15 = 'XV. Arica y Parinacota';
                              16 = 'XVI. Ñuble'", as.factor = T,
                            levels = c('XV. Arica y Parinacota',
                                       'I. Tarapacá',
                                       'II. Antofagasta',
                                       'III. Atacama',
                                       'IV. Coquimbo',
                                       'V. Valparaíso',
                                       'XIII. Metropolitana',
                                       'VI. OHiggins',
                                       'VII. Maule',
                                       'XVI. Ñuble',
                                       'VIII. Bío-bío',
                                       'IX. La Araucanía',
                                       'XIV. Los Ríos',
                                       'X. Los Lagos',
                                       'XI. Aysén',
                                       'XII. Magallanes'))) %>% 
  arrange(run_alu,desc(fec_ing_alu)) %>% # Eliminar duplicados
  distinct(run_alu, .keep_all = T) %>% 
  filter(educ_joven == "Educ nn") %>% # Seleccionar variables
  select(dependencia,
       situacion_final,
       educ_joven, curso, genero, ruralidad,
       etnia, nacionalidad,nivel,region)


# Exportar datos --------------------------------------------------------------
saveRDS(proc,"output/data/bd_proc.rds")
