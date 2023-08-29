rm(list = ls())

# Análisis tasa nacionales ----------------------------------------------------

# Cargar paquetes
pacman::p_load(tidyverse,
               haven,
               sjmisc,
               janitor,
               writexl,
               openxlsx)

# Carga datos -----------------------------------------------------------------

data = readRDS("output/data/bd_proc.rds")


# Creación tablas -------------------------------------------------------------

# tasa_nacional = merge(data %>% group_by(genero,situacion_final) %>% summarise(situacion = n()),
#                       data %>% group_by(genero) %>% summarise(tot = n()),
#                       "genero") %>% 
#   mutate(tasa = round(situacion/tot*100,2))


# tasa_curso = merge(data %>% group_by(genero,situacion_final,curso) %>% summarise(situacion = n()),
#                    data %>% group_by(curso,genero) %>% summarise(tot = n()),
#                    c("curso","genero")) %>% 
#  mutate(tasa = round(situacion/tot*100,2))

tabla_nacional = tabyl(data,situacion_final,genero) %>% 
  adorn_totals("row") %>% 
  adorn_percentages("col") %>% 
  adorn_pct_formatting(digits = 0) %>% 
  adorn_ns(position = "front")
  

# Esta funcion construye una lista: una tabla para cada 3ra variable
# Hombre mujer y su sitfin en municipal/ Homb mujer y su sitfin en particular pagado

curso = tabyl(data,situacion_final,genero,curso) %>% 
  adorn_totals("row") %>% 
  adorn_percentages("col") %>% 
  adorn_pct_formatting(digits = 0) %>% 
  adorn_ns(position = "front")

nivel = tabyl(data,situacion_final,genero,nivel) %>% 
  adorn_totals("row") %>% 
  adorn_percentages("col") %>% 
  adorn_pct_formatting(digits = 0) %>% 
  adorn_ns(position = "front")

dependencia = tabyl(data,situacion_final,genero,nivel) %>% 
  adorn_totals("row") %>% 
  adorn_percentages("col") %>% 
  adorn_pct_formatting(digits = 0) %>% 
  adorn_ns(position = "front")

region = tabyl(data,situacion_final,genero,nivel) %>% 
  adorn_totals("row") %>% 
  adorn_percentages("col") %>% 
  adorn_pct_formatting(digits = 0) %>% 
  adorn_ns(position = "front")

migrante = tabyl(data,situacion_final,genero,nivel) %>% 
  adorn_totals("row") %>% 
  adorn_percentages("col") %>% 
  adorn_pct_formatting(digits = 0) %>% 
  adorn_ns(position = "front")

rural = tabyl(data,situacion_final,genero,nivel) %>% 
  adorn_totals("row") %>% 
  adorn_percentages("col") %>% 
  adorn_pct_formatting(digits = 0) %>% 
  adorn_ns(position = "front")

pueblo = tabyl(data,situacion_final,genero,nivel) %>% 
  adorn_totals("row") %>% 
  adorn_percentages("col") %>% 
  adorn_pct_formatting(digits = 0) %>% 
  adorn_ns(position = "front")


# Exportar tablas

# Tabla nacional
writexl::write_xlsx(tabla_nacional,"output/img/tabla_nacional.xlsx")

# Tabla por curso
writexl::write_xlsx(curso,"output/img/curso.xlsx")

# Tabla por nivel
writexl::write_xlsx(nivel,"output/img/nivel.xlsx")

# Tabla por dependendencia
writexl::write_xlsx(dependencia,"output/img/dependencia.xlsx")

# Tabla por region
writexl::write_xlsx(region,"output/img/region.xlsx")

# Tabla por migrante/no migrante
writexl::write_xlsx(migrante,"output/img/migrante.xlsx")

# Tabla por rural/urbano
writexl::write_xlsx(rural,"output/img/ruralidad.xlsx")

# Tabla por pueblo originario (binario)
writexl::write_xlsx(pueblo,"output/img/pueblo_origen.xlsx")






# archivo_excel = "dataframesjuntos.xlsx"
# prueba = createWorkbook()
# addWorksheet(prueba, sheetName = "primera")
# writeData(prueba, sheet = "primera", dependencia, startCol = 1, startRow = 4)
# writeData(prueba, sheet = "primera", tabla_nacional, startCol = 1, startRow = 1)
# saveWorkbook(prueba,archivo_excel, overwrite = TRUE)



# Esta función agrupa sitfin y genero y hace el cruce con todas las demas variables.
# Genera pequeñas tablas de hombres/aprob hombres/reprob homb/trasl
# Hay que juga con el select y el groupby y filtro


data %>%
  filter(genero == "Hombre") %>%
  group_by(situacion_final,genero) %>% #Seleccionamos las variables de interés
  frq(show.na = F,
      out = "viewer",
      encoding = "UTF-8",
      sort.frq = "asc")


# sjt.xtab(data$situacion_final, data$genero,
#         show.col.prc=TRUE,
#         show.summary=F, 
#         title = "Tabla de contingencia:
# sexo y satisfacción con equilibrio trabajo-familia",
#          encoding = "UTF-8")

# Opcion 3: aprender como agrupar dos variables con la funcion frq
#
# Opcion 4: tabyl
#
# Opcion 5: prop.table
# Exportar manualmente desde el Viewer
# Export > Save as Image...
# Definir Directory la carpeta output/fig
# Dar un nombre al archivo (por ej., kable) 