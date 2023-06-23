#'Diseño factorial con dos factores.
#'
#'Crea un diseño factorial de dos factores.
#'
#'@param respuesta (Columna de datos) Nuestra variable dependiente o variable respuesta.
#'@param factorA (Columna de datos) El primer factor de nuestro diseño factorial.
#'@param factorB (Columna de datos) El segundo factor de nuestro diseño factorial:
#'@returns Devuelve una tabla ANOVA con la fuente de variacion, la suma de cuadrados, los grados de libertad, el cuadrado medio y el F valor.
#'@export
#'
#'@examples
#'\dontrun{
#'library(readr)
#'Libro1 <- read_csv("C:/Users/donpo/OneDrive/Escritorio/Libro1.csv")
#'
#'library(DisFact)
#'
#'data <- data.frame(Libro1)
#'
#'TablaAnova(respuesta = "hr", factorA = "temp", factorB = "mat", data = data)
#'}
TablaAnova <- function(respuesta, factorA, factorB, data){

  # Definimos parámetros

  y <- data[ , respuesta]

trat <- factor(data[ ,factorB])

  bloque <- factor(data[ ,factorA])

  a <- nlevels(trat)

  b <- nlevels(bloque)

  N <- length(y)

  n <- N/(a*b)


  #Correccion para la media

  suma_total <- sum(y)

  cuad <- suma_total^2/(a*b*n)

  #Suma de cuadrados total

  sct <- sum(y^2)

  sc_total <- sct - cuad

  gl_total <- a*b*n-1

  # Suma de cuadrados por Tratamiento B

  tapply(y, trat, sum)

  scb <-  tapply(y, trat, sum)

  scb <- sum(scb^2)

  bn <- b*n

  SSMat <- ((1/bn)*scb)-cuad

  # Calculo los grados de libertad

  gl_tratB <- a-1

  # Calculo el cuadrado medio

  cm_tratB <- SSMat/gl_tratB


  # Suma de cuadrados por tratameinto A

  tapply(y, bloque, sum)

  sca <-  tapply(y, bloque, sum)

  sca <- sum(sca^2)

  ba <- a*n

  SSTemp <- ((1/ba)*sca)-cuad

  # Calculo los grados de libertad

  gl_tratA <- a-1

  # Calculo el cuadrado medio

  cm_tratA <- SSTemp/gl_tratA


  # Suma de cuadrados por interrelación


  f2 <- factor(paste0(trat, "-", bloque))


  tapply(y, f2, sum)

  sci <- tapply(y, f2, sum)

  sci <- sum(sci^2)

  SSInter <- ((1/n)*sci)-cuad-SSMat-SSTemp

  # Calculo los grados de libertad

  gl_SSI <- (a-1)*(b-1)

    # Calculo el cuadrado medio de Interelacion

  cm_SSI <- (SSInter)/gl_SSI

  # Calculamos la suma de cuadrados del error

  SSError <- sc_total-SSInter-SSMat-SSTemp

  # Calculo los grados de libertad del error

  gl_Error <- a*b*(n-1)

  # Calculo el cuadrado medio del Error

  cm_Error <- SSError/gl_Error

  # Ahora saco los valores de F

  F_a <- cm_tratA/cm_Error

  F_b <- cm_tratB/cm_Error

  F_i <- cm_SSI/cm_Error

SSModelo <- SSMat+SSTemp+SSInter

R_2 <- SSModelo/sc_total

  # Creamos el data frame

  tabla <- data.frame(
    Fuente_de_variación = c("FactorA","FactorB","Interrelación","Error","Total"),
    Suma_de_cuadrados = c(SSTemp, SSMat, SSInter,SSError,sc_total),
    Grados_de_libertad = c(gl_tratA, gl_tratB, gl_SSI, gl_Error, gl_total),
    Cuadrado_medio = c(cm_tratA, cm_tratB, cm_SSI, cm_Error, NA),
    F_valor = c(F_a, F_b, F_i, NA, NA)
  )


  anava <- format(tabla)
  anava[is.na(tabla)] <- ""

  return(anava)
}














