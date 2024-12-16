setwd("C:/r_unir/s4 lab 2")

#función para leer el archivo de números
leer_numeros <- function(numeros.txt) {
  #verificar si el archivo existe
  if (!file.exists(numeros.txt)) {
    stop("El archivo no existe. Por favor, verifica el nombre o la ubicación.")
  }
  #leer los números y convertirlos a un vector de enteros
  numeros <- as.integer(readLines(numeros.txt, warn = FALSE))
  return(numeros)
}

archivo_entrada <- "C:/r_unir/s4 lab 2/numeros.txt"
archivo_salida <- "C:/r_unir/s4 lab 2/resultados.txt"

numeros <- leer_numeros(archivo_entrada)

#calcular los estadísticos
media_numeros <- mean(numeros)
mediana_numeros <- median(numeros)
desviacion_numeros <- sd(numeros)

#manejar valores atípicos-->verificar si hay alta variabilidad
if (desviacion_numeros > 10) {
  mensaje_variabilidad <- "Alta variabilidad, la desviación estándar es mayor a 10."
  cat(mensaje_variabilidad, "\n")
} else {
  mensaje_variabilidad <- "Variabilidad dentro del rango esperado."
}

#aplicar funcion con la familiar  apply
cuadrados <- sapply(numeros, function(x) x^2)

#sobrescribir el archivo de resultados
fileConn <- file(archivo_salida, "w")
writeLines(c(
  "Resultados del análisis de numeros.txt:",
  "--------------------------------------",
  paste("Media:", media_numeros),
  paste("Mediana:", mediana_numeros),
  paste("Desviación estándar:", desviacion_numeros),
  mensaje_variabilidad,
  "\nCuadrados de los números:",
  paste(cuadrados, collapse = ", ")
), fileConn)

#cerrar conexión al archivo
close(fileConn)

#mensaje indicando que el script finalizó correctamente
cat("El análisis ha finalizado. Los resultados se guardaron en resultados.txt\n")
