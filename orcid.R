# Pre-requisitos----
# Instalación y carga de paquetes
install.packages("rorcid")
install.packages("httpuv")
install.packages("tidyverse")
install.packages("janitor")
install.packages("usethis")
install.packages("anytime")

library(rorcid)
library(tidyverse)
library(janitor)
library(usethis)
library(anytime)

# Para hacer uso de la API de debe haber registrado una cuenta previamente
orcid_auth()

#---Búsqueda Simple----

# Realizamos una búsqueda por apellido
rorcid::orcid_search(family_name = 'Valenzuela')

# Por defecto la función orcid_search entrega sólo 10 resultados
# Podemos obtener hasta 200 resultados con el parámetro rows
rorcid::orcid_search(family_name = 'Valenzuela', rows = 50)

# Se pueden combinar otros parámetros de búsqueda para refinar las búsquedas
rorcid::orcid_search(family_name = 'Valenzuela',
                     current_inst = 'Universidad de Concepción')


#---Búsqueda Avanzada----
# Para realizar búsquedas más complejas se utiliza la función orcid
# dentro de la cual se puede construir un query de búsqueda que
# incluye varios parámetros y operaciones lógicas AND y OR

rorcid::orcid(
  query = 'ringgold-org-id:28067 OR
                       affiliation-org-name:"Universidad de Tarapacá" OR
                       affiliation-org-name:"Universidad de Tarapaca"'
)

# Por defecto sólo se entregan los primeros 100 resultados
# Con las siguientes funciones se puede obtener el número total
# de resultados de la búsqueda

univ_count <-
  base::attr(
    rorcid::orcid(
      query = 'ringgold-org-id:14655 OR
               email:*@uchile.cl OR
               affiliation-org-name:"Universidad De Chile"'
    ),
    "found"
  )
univ_count

# Si obtenemos menos de 200 resultados podemos guardarlos directamente
univ_orcids <- rorcid::orcid(
  query = 'ringgold-org-id:28067 OR
                       affiliation-org-name:"Universidad De Chile"'
)

# Si obtenemos más de 200 resultados
# Se crea una secuencia a intervalos de 200 resultados
# la cual es usada como marca para obtener resultados sucesivos
paginas <- seq(from = 0, to = univ_count, by = 200)

# Utilizando la secuencia, se pasa como parámetro a la función de búsqueda
# y se obtienen los resultados sucesivos, hasta completar la búsqueda completa.
univ_orcids <- purrr::map(paginas,
                          function(page) {
                            print(page)
                            orcids <- rorcid::orcid(
                              query = 'ringgold-org-id:14655 OR
                                       email:*@uchile.cl OR
                                       affiliation-org-name:"Universidad De Chile"',
                              rows = 200,
                              start = page
                            )
                            return(orcids)
                          })

# Los resultados devueltos son re-formateados para trabajar con ellos
univ_orcids_data <- univ_orcids %>%
  dplyr::bind_rows() %>%
  janitor::clean_names()

# Podemos extraer todos los identificadores orcid de los resultados
univ_orcids_vector <- univ_orcids_data$orcid_identifier_path

# Luego esos identificadores pueden ser pasados a una función que obtiene
# el detalle del apartado employment de los orcid obtenidos
# este paso puede demorar varios minutos dependiendo de la cantidad de resultados
univ_employment <- rorcid::orcid_employments(univ_orcids_vector)

# la data de employment es reformateada para su posterior utilización
univ_employment_data <- univ_employment %>%
  purrr::map(., purrr::pluck, "affiliation-group", "summaries") %>%
  purrr::flatten_dfr() %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    employment_summary_end_date = anytime::anydate(employment_summary_end_date /
                                                     1000),
    employment_summary_created_date_value = anytime::anydate(employment_summary_created_date_value /
                                                               1000),
    employment_summary_last_modified_date_value = anytime::anydate(employment_summary_last_modified_date_value /
                                                                     1000)
  )

names(univ_employment_data) <- names(univ_employment_data) %>%
  stringr::str_replace(., "employment_summary_", "") %>%
  stringr::str_replace(., "source_source_", "") %>%
  stringr::str_replace(., "organization_disambiguated_", "")

# Luego de tenerla en una forma trabajable podemos filtrar los resultados
# y dejar sólo aquellos donde aparezca la organización de interés listada
# dentro del historial de empleo

# Lista las diferentes organizaciones dentro del set de datos
unique(univ_employment_data$organization_name)

# Corrige formas o errores de escritura, reemplaza lo primero por lo segundo

univ_employment_data$organization_name <-
  univ_employment_data$organization_name %>%
  stringr::str_replace_all(
    .,
    "Universidad De Chile",
    "Universidad de Chile"
  )

# Filtra el set de datos
univ_employment_data_filtered <- univ_employment_data %>%
  filter(organization_name == "Universidad de Chile")

univ_employment_data_filtered <- univ_employment_data %>%
  filter(grepl(
    "Universidad de Chile",
    univ_employment_data$organization_name
  ))

unique(univ_employment_data_filtered$organization_name)

# Para dejar sólo aquellos resultados donde la organización de interés
# es la que aparece como actualmente en empleo podemos dejar sólo los resultados
# que no tengan una fecha de término, marcado como NA en la columna correspondiente
univ_employment_data_filtered_current <-
  univ_employment_data_filtered %>%
  filter(is.na(end_date_year_value))

# Podemos eliminar algunos registros duplicados, por ejemplo aquellos investigadores
# que cumplen varias funciones en la organización
univ_current <-
  univ_employment_data_filtered_current[!duplicated(univ_employment_data_filtered_current["orcid_path"]), ]

uchile <- univ_current

# Para analizar las publicaciones de 1 o varios orcid
univ_works <- rorcid::orcid_works(ucn$orcid_path)

univ_works_data <- univ_works %>%
  purrr::map_dfr(pluck, "works") %>%
  janitor::clean_names() %>%
  dplyr::mutate(created_date_value = anytime::anydate(created_date_value/1000))


write_csv2(udec, "udec.csv")
