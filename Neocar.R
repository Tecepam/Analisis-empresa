
# Importar tablas ---------------------------------------------------------

clientes <- read_csv('../4. Tidyr, forcats y stringr/Automatriz_dirty/clientes_dirty.csv')
comisiones <- read_csv('../4. Tidyr, forcats y stringr/Automatriz_dirty/comisiones_dirty.csv')
modelos <- read_csv('../4. Tidyr, forcats y stringr/Automatriz_dirty/modelos_dirty.csv')
ventas <- read_csv('../4. Tidyr, forcats y stringr/Automatriz_dirty/ventas_dirty.csv')

# ---------------------- LIMPIEZA ------------------------------------------
# En tabla clientes, cambiar la columna provincia para que no hayan 
# provincias iguales escritas de forma diferente (ej, con y sin tilde, mayúsculas, 
# caracteres especiales, etc)

clientes <- clientes %>% 
  mutate(
    ProvinciaResidencia = str_to_lower(ProvinciaResidencia),
    ProvinciaResidencia = str_replace_all(ProvinciaResidencia, c(
      "á" = "a", 
      "é" = "e", 
      "í" = "i", 
      "ó" = "o", 
      "ú" = "u"
    )),
    ProvinciaResidencia = str_to_title(ProvinciaResidencia)
  ) 

# En tabla clientes, cambiar la columna Nacionalidad para que no hayan 
# nacionalidades iguales escritas de forma diferente.

clientes <- clientes %>% 
  mutate(
    Nacionalidad = str_to_lower(Nacionalidad),
    Nacionalidad = str_replace_all(Nacionalidad, c(
      "á" = "a", 
      "é" = "e", 
      "í" = "i", 
      "ó" = "o", 
      "ú" = "u"
    )),
    Nacionalidad = str_replace_all(Nacionalidad, "[:punct:]", ""),
    Nacionalidad = str_to_upper(Nacionalidad)
  )

# En tabla clientes, cambiar la columna Genero para que no hayan generos 
# iguales escritos de forma diferente.

clientes <- clientes %>% 
  mutate(
    Genero = str_to_upper(Genero)
  )


# En tabla Ventas, cambiar la columna Estado para que no hayan estados iguales 
# escritos de forma diferente.

ventas <- ventas %>% 
  mutate(
    Estado = str_replace_all(Estado, "[:punct:]+", " "),
    Estado = str_to_title(Estado)
  ) 

# En tabla clientes, reemplazar todos los valores faltantes en columna 
# nacionalidad para que sean de nacionalidad española (ES)

clientes <- clientes %>% 
  mutate(
    Nacionalidad = str_replace_na(Nacionalidad, "ES")
  )

# En tabla clientes, convertir columna Estudios en un factor para que el orden 
# sea el siguiente: Universitario, Preuniversitario, Secundario, Primario

clientes <- clientes %>% 
  mutate(
    Estudios = factor(
      Estudios,
      levels = c("Universitario", "PreUniversitario", "Secundario", "Primario")
    )
  )

# En tabla clientes, convertir columna TipoCliente en un factor para que el orden 
# sea el siguiente: VIP, Nuevo, Comun. Verificar que no hayan errores de carga

clientes <- clientes %>% 
  mutate(
    TipoCliente = str_replace_all(TipoCliente, "[:punct:]",""),
    TipoCliente = str_replace_all(TipoCliente, " ",""),
    TipoCliente = str_to_lower(TipoCliente),
    TipoCliente = str_replace_all(TipoCliente, "^nuebo$","nuevo"),
    TipoCliente = str_replace_all(TipoCliente, "ú","u"),
    TipoCliente = str_to_title(TipoCliente),
    TipoCliente = str_replace_all(TipoCliente, "Vip","VIP")
  )


# Transformar tabla Comisiones para que quede con el siguiente formato de columnas:
# a.	Marca b.	Modelo c.	Year (año de fabricación) d.	ComisionVariable e.	ComisionFija

comisiones <- comisiones %>% 
  separate_wider_delim(
    cols  = Coche,
    names = c("Marca", "Modelo"),
    delim = " - "
  ) %>% 
  pivot_longer(
    cols = c(3:27),
    names_to = "Year",
    values_to = "Comision"
  ) %>% 
  separate_wider_delim(
    cols = Comision,
    names = c("ComisionVariable", "ComisionFija"),
    delim = "/"
  ) 
comisiones <- comisiones %>% 
  mutate(
    across(c("Year", "ComisionFija", "ComisionVariable"), as.double)
  )


# 10.	En tabla clientes, reemplazar todos los valores faltantes en columna genero 
# para que sean “Otro”

clientes <- clientes %>% 
  mutate(
    Genero = str_replace_na(Genero,"Otro")
  )


# ------------------------- ANÁLISIS --------------------------------------
# Agregar una columna a la tabla Ventas que sea la comisión calculada como 
# “ComisionFija + Precio * ComisionVariable”

ventas %>% 
  left_join(modelos, by = "IdModelo") %>% 
  left_join(comisiones, by = c("Modelo", "Marca", "Year")) %>%
  mutate(
    ComisionTotal = ComisionFija + Precio * ComisionVariable
  ) 
 
# Calcular la comisión generada por Clientes cuyo nombre comience y 
# termine con la letra “s”

ventas %>% 
  left_join(clientes) %>% 
  filter(str_sub(Nombre, 1, 1) %in% c("s","S") & str_sub(Nombre, -1, -1) %in% c("s","S")) %>% 
  left_join(modelos, by = "IdModelo") %>% 
  left_join(comisiones, by = c("Modelo", "Marca", "Year")) %>%
  mutate(
    ComisionTotal = ComisionFija + Precio * ComisionVariable
  ) %>% 
  summarise(
    Total_comision = sum(ComisionTotal)
  )


# Armar una tabla con las comisiones que tenga en filas la marca de los 
# vehículos y en columnas la comisión generada por Modelo

ventas %>% 
  left_join(modelos, by = "IdModelo") %>% 
  left_join(comisiones, by = c("Modelo", "Marca", "Year")) %>%
  mutate(
    ComisionTotal = ComisionFija + Precio * ComisionVariable
  ) %>% 
  group_by(Marca, Modelo) %>% 
  summarise(ComisionTotal = sum(ComisionTotal)) %>% 
  pivot_wider(
    names_from = Modelo,
    values_from = ComisionTotal
  )


# Armar una tabla que muestre la cantidad de vehículos vendidos. 
# En filas tendrá las marcas y en columnas el género

ventas %>% 
  left_join(clientes, by = "IdCliente") %>% 
  left_join(modelos, by = "IdModelo") %>% 
  group_by(Marca, Genero) %>% 
  summarise(Cantidad = n()) %>% 
  pivot_wider(
    names_from = Genero,
    values_from = Cantidad
  )

# Armar un cuadro que muestre en filas la Marca y el Modelo y en columnas el 
# nivel de Estudios alcanzado que muestre el total facturado

ventas %>% 
  left_join(clientes, by = "IdCliente") %>% 
  left_join(modelos, by = "IdModelo") %>% 
  group_by(Marca, Modelo, Genero) %>% 
  summarise(Facturado = sum(Precio)) %>% 
  pivot_wider(
    names_from = Genero,
    values_from = Facturado
  )

# Generar una columna que sea Nombre Completo que muestre primero el/los nombres, 
# seguido de un espacio, y después el/los apellidos.

clientes %>% 
  separate_wider_delim(
    cols = Nombre,
    names = c("Apellido", "Nombre"),
    delim = ", "
  ) %>% 
  unite(
    "NombreCompleto",
    Nombre,
    Apellido,
    sep = ", "
  )

# Agrupar los clientes en función de si su nombre empieza con vocal o no y 
# contar cuantos coches compró cada grupo.

ventas %>% 
  left_join(clientes) %>% 
  mutate(
    primera_letra = case_when(
      str_to_lower(str_sub(Nombre, 1, 1)) %in% c("a", "e", "i", "o", "u") ~ "Vocal",
      TRUE ~ "Consonante"
    ) 
  ) %>%
  group_by(primera_letra) %>% 
  summarise(
    Cantidad = n()
  )

# ----------------- EXTRAS -------------------------------------

clientes <- clientes %>%
  separate_wider_delim(
    cols  = Nombre, 
    names = c("Nombre", "Apellido"), 
    delim = ", "
  )

clientes %>% 
  filter(str_detect(Nombre,pattern = "^[Aa]"))

ventas %>% 
  left_join(clientes) %>% 
  filter(str_detect(Nombre,pattern = "^[Ee]")) %>% 
  summarise(Cantidad = n())


clientes %>% 
  filter(str_detect(Apellido,pattern = "[:blank:]")) %>%  # si tiene un espacio en blanco es porque tiene 2 o más apellidos
  summarise(Cantidad = n())


clientes %>% 
  filter(str_detect(Nombre,pattern = "[:blank:]") & str_detect(Apellido,pattern = "[:blank:]")) %>% 
  summarise(Cantidad = n())

clientes %>% 
  mutate(
    Nombre = str_replace(Nombre, "[:blank:]", ""), # elimino espacios en blanco del nombre
    Apellido = str_replace(Apellido, "[:blank:]", ""), # elimino espacios en blanco del apellido
  ) %>% 
  filter(str_length(Nombre) == str_length(Apellido))


clientes %>% 
  filter(str_detect(Nombre,pattern = "[:punct:]") & str_detect(Apellido,pattern = "[:punct:]")) %>% 
  summarise(Cantidad = n())


clientes %>% 
  mutate(
    Vocales = str_count(tolower(Nombre), "[aeiou]") + str_count(tolower(Apellido), "[aeiou]")
  )


clientes %>% 
  filter(
    !str_sub(str_to_lower(ProvinciaResidencia), 1, 1) %in% c("a", "e", "i", "o", "u") &
      !str_sub(str_to_lower(ProvinciaResidencia), -1, -1) %in% c("a", "e", "i", "o", "u")
  ) %>% 
  distinct(ProvinciaResidencia)


clientes %>% 
  distinct(ProvinciaResidencia) %>% 
  mutate(
    ProvinciaResidencia_sin_vocales = str_replace(ProvinciaResidencia, "[:blank:]", ""), # elimino espacios en blanco
    Vocales = str_count(tolower(ProvinciaResidencia), "[aeiou]"), # cuento vocales
    Consonantes = str_length(ProvinciaResidencia_sin_vocales) - Vocales # todo lo que no sean vocales, son consonantes
  ) %>% 
  select(ProvinciaResidencia, Vocales, Consonantes) %>% 
  filter(Vocales > Consonantes)



