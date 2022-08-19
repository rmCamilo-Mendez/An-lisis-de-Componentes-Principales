#----------------------------------------#
#   ANÁLISIS DE COMPONENTES PRINCIPALES  # 
#    Estudiante Camilo Mendez Hilario    #
#      20191412@lamolina.edu.pe          #
#----------------------------------------#

# Para limpiar el workspace, por si hubiera algún dataset 
# o información cargada
rm(list = ls())
graphics.off()

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Otras opciones
options(scipen = 999)    # Eliminar la notación científica
options(digits = 4)      # Número de decimales

# ------------ #
# INTRODUCCIÓN #
# ------------ #
# Redes Sociales en una universidad pública 

# Se realizó un estudio para conocer cómo
# evalúan las redes sociales los estudiantes de
# pregrado de la UNALM de acuerdo a ciertas
# variables.
# Se realizó una encuesta a 95 alumnos de
# pregrado matriculados en el semestre 2013-II.
# Se diseñó una encuesta con 10 preguntas
# donde se le pide a cada uno de los 95
# encuestados que valoren de 1 a 5 si están de
# acuerdo a las siguientes afirmaciones con
# relación a las redes sociales. Los
# encuestados deberán contestar con 5 si
# están muy de acuerdo con la afirmación, 4 si
# están de acuerdo, 3 si están indiferentes, 2
# si están en desacuerdo y 1 si están muy en
# desacuerdo. Las 10 afirmaciones con relación
# a las redes sociales a valorar (X1 a X10) son
# las siguientes:

# X1: Contribuyen al bullying
# X2: Pueden ser adictivas y devorar gran cantidad de
#     nuestro tiempo, pues son ideales para el ocio.
# X3: Presentan problemas como la invasión de la privacidad.
# X4: Son un medio que permiten hacer trabajos, tareas, investigación.
# X5: Tiene información actualizada acerca de temas de interés, además
#     permiten acudir a eventos,participar en actos y conferencias.
# X6: Facilitan las relaciones entre las personas, evitando todo tipo 
#     de barreras tanto culturales como físicas.
# X7: Permiten el establecimiento de lazos y relaciones con personas
#     que comparten los mismos intereses, preocupaciones y necesidades.
# X8: Alejan de la posibilidad de conocer a gente de tu mismo entorno, 
#     lo que te aísla de la familia, amigos de la universidad, etc.
# X9: Permiten que las relaciones interpersonales se vuelvan menos afianzadas.
# X10: Se hacen amenazas, se discrimina, se estafa y se incita a la violencia a, 
#      ya que cualquiera puede crear una identidad falsa que será muy difícil descubrir.


# ----------------- #
# LECTURA DE DATOS  #
# ----------------- #

library(readr)
data <- read_csv("BD-Redes Sociales.csv")


# -------------------- #
# ANÁLISIS DESCRIPTIVO #
# -------------------- #

library(psych)
psych::describe(data)
# Se visualiza que los datos aparentemente presentan rangos similares, prueba de ello
# son los valores de la media de cada variable, son similares. Para este caso se 
# utilizaría la matriz var-cov pero tambíen se puede utilizar la matriz de correlación.

cor(data)  # Matriz de correlación
library(GGally)
ggpairs(data)  # Grafico de correlación

# Se visualiza que las varibale x4 y x5 tienen una correlación fuerte de 0.72, las
# variables x5 y x2 una correlación moderada de 0.41, x6 y x5 correlación moderada de 0.441,
# x7 y x4 correlación moderada de 0.485, x7 y x5 correlación moderada de 0.498, x9 y x8
# correlación moderada de 0.49 y x10 y x4 correlación moderada de 0.473. Por lo tanto hay
# evidencia estadística que las variables tienen relación una con otras. Se procederá a aplicar
# el análisis de componentes principales.



# ------------------------------------------------------- #
# ANÁLISIS DE COMPONENTES PRINCIPALES CON EL PAQUETE ade4 #
# ------------------------------------------------------- #
library(ade4)
acp <- dudi.pca(data,
                scannf=FALSE, 
                scale=TRUE,center = T, #Matriz de correlación 
                nf=ncol(data))

acp$eig  # autovalores/ valores propios
# 3.2851494 1.6444385 1.1272057 0.9370378 0.8337603 0.5625358 0.5089936 0.4498452 0.4194962 0.2315376

acp$c1   # autovectores
#         CS1          CS2         CS3          CS4         CS5          
# x1  -0.21691684 -0.242422299  0.09514009  0.664205382  0.56134662 
# x2  -0.30387482 -0.207393110  0.51984911 -0.008813135 -0.11091635  
# x3  -0.26426881  0.003723119  0.65322893 -0.218160593 -0.05553836 
# x4  -0.44173063 -0.138251435 -0.19669583  0.010048149 -0.31603078
# x5  -0.45138963 -0.186904880 -0.13541771 -0.156843744 -0.13732272
# x6  -0.32428757  0.042120004 -0.08613462 -0.456006225  0.58487835  
# x7  -0.35743691  0.027183139 -0.44911595 -0.158106091  0.07717151 
# x8  -0.07252091  0.644672997  0.12128391 -0.017549536  0.32483300  
# x9  -0.17728274  0.609220527  0.05988743  0.083085801 -0.23808735 
# x10 -0.34968356  0.234292192 -0.09772112  0.496294387 -0.20132030  
#     CS6         CS7         CS8         CS9                CS10
# -0.217900957 -0.01806831 -0.27031765  0.06802728      0.05780508
# 0.621990720 -0.40855681 -0.08919355 -0.02368959       0.14041785
# -0.597680932  0.04265569  0.27647043  0.10643710      -0.07716725
# -0.153416754  0.25984330 -0.06480485 -0.35212986      0.65409829
# -0.006915677  0.15561433 -0.39787638 -0.23627756     -0.67905977
# 0.281156070  0.38820707  0.12863707  0.27175835       0.12436394
# -0.229371909 -0.72536557  0.16000732  0.18472705      0.01867355
# 0.011770631 -0.15187651 -0.01380733 -0.65958628       -0.01238810
# -0.018204323  0.05297432 -0.51312647  0.50272852       0.10523033
# 0.229585582  0.19096676  0.61187617  0.08884253       -0.23447363

# Componente = Combinación lineal de todas las variables
 
# Y1 = -0.21691684*z1 - 0.30387482*z2 ...- 0.34968356*z10
#  .
#  .
#  .
# Y10 = 0.05780508*z1 + 0.14041785*z2 ...- 0.23447363*z10


# ---------------------------------------------- #
# CRITERIO PARA ESCOGER EL NÚMERO DE COMPONENTES #
# ---------------------------------------------- #

# 1. Media Aritmetica
# valor = (suma de la traza de la matriz correlación o matriz var-cov)/(número de variables) 
sum(diag(cor(data)))/ncol(data)
# 1
acp$eig
# 3.2851494 1.6444385 1.1272057 0.9370378 0.8337603 0.5625358 0.5089936 0.4498452 0.4194962 0.2315376

# Según este criterio de la media aritmetica, el valor obtenido sera un tipo un "filtro", es decir, los
# autovalores igual o mayor a dicho valores se quedaran.
# Nos quedamos con 3 autovalores: 3.2851494; 1.6444385 y 1.1272057
# Por lo tanto se trabajará con 3 componentes.

# 2. Grafico de Sedimentación
# Se busca el punto de inflexión, a partir de ese punto nos quedaremos con las componentes que esten por 
# encima del punto. 
library(ggplot2)
ggplot(as.data.frame(acp$eig)) + 
  aes(x = seq(1:10), y = acp$eig) + 
  geom_point(color = "red") + geom_line() + theme_bw() +
  scale_x_continuous(breaks = seq(1:10)) +
  labs(title = "Scree-Plot",
       x = "Componentes",
       y = "Autovalor")

fviz_eig(acp, addlabels=TRUE, hjust = -0.3)
# Apartir del tercer autovalor la disminución es poco significativa en comparación con las primeras. 
# Se considerará optar por trabajar con 3 componentes.

Variabilidad = (3.2851494/10)*100 + (1.6444385/10)*100 + (1.1272057/10)*100 ;  Variabilidad
# Se esta trabajando con el 60.56% de variabilidad del 100 de la data.

# -------------------------------------------------------- #
# CORRELACIÓN DE LAS VARIABLES ASOCIADAS A CADA COMPONENTE #
# -------------------------------------------------------- #

# correlación(variable(z), componente(n)) = (autovector de el componenete)*raiz(autovalor de el componente)
# correlación(variable(z1), componente(1)) = -0.21691684*sqrt(3.2851494) =  -0.3931614

acp$co[,1:3]  # correlación de todas las variables con los 3 componentes.

#       Comp1        Comp2       Comp3
#x1  -0.3931614 -0.310871840  0.10101017
#x2  -0.5507725 -0.265951929  0.55192351
#x3  -0.4789867  0.004774366  0.69353279
#x4  -0.8006359 -0.177287643 -0.20883185
#x5  -0.8181429 -0.239678711 -0.14377291
#x6  -0.5877706  0.054012865 -0.09144908
#x7  -0.6478537  0.034858479 -0.47682615
#x8  -0.1314440  0.826700687  0.12876706
#x9  -0.3213246  0.781237977  0.06358245
#x10 -0.6338008  0.300446144 -0.10375046

# Interpretación:
# La correlacíon de la variable x1 y el componente 1 es relativamente baja de -0.39.
# La correlación de la variable x8 y el componente 2 es alta de 0.82
# La correlación de la variable x2 y el componente 3 es moderada de 0.55

# Componente 1: x1, x4, x5, x6, x7, x10
# Componente 2: x8, x9
# Componente 3: x2, x3

fviz_pca_var(acp, col.var="steelblue")+theme_minimal()  # Grafico con 2 componentes


# ---------------------- #
# CONTRIBUCIÓN RELATIVA  #
# ---------------------- #

contri_relativa <- acp$co[,1:3]*acp$co[,1:3]   ; contri_relativa

#      Comp1        Comp2       Comp3
#x1  0.15457587 9.664130e-02 0.010203055
#x2  0.30335040 7.073043e-02 0.304619556
#x3  0.22942828 2.279457e-05 0.480987726
#x4  0.64101790 3.143091e-02 0.043610743
#x5  0.66935774 5.744588e-02 0.020670650
#x6  0.34547428 2.917390e-03 0.008362934
#x7  0.41971446 1.215114e-03 0.227363178
#x8  0.01727753 6.834340e-01 0.016580955
#x9  0.10324953 6.103328e-01 0.004042727
#x10 0.40170346 9.026789e-02 0.010764158
# Interpretación:
# El 15.45% de la variabilidad de la variable x1 esta siendo explicada por el componente 1
# El 9.66% de la variabilidad de la variable x1 esta siendo explicada por el componente 2
# El 41.97% de la variabilidad de la variable x7 esta siendo explicada por el componente 1
# El 22.737% de la variabilidad de la variable x7 esta siendo explicada por el componente 3

contrib <- as.matrix(contri_relativa)  # Covirtiendo a matriz
library(corrplot)
corrplot(contrib,is.corr=FALSE)        # Graficando la contribución relativa


# ---------------------- #
# Scores o Puntuaciones  #
#---------------------- -#

# Obtener los datos originales, estandarizados y los scores o puntuaciones de los 2 componentes
# datos estandarizados(solo porque se trabajo con la matriz correlación)
datos.estan <- as.data.frame(scale(data))

acp$li[,1:3] # scores

# correlación de los componetes y sus respectivos scores o puntuaciones
cor(acp$li)  # la correlación debe ser nula

# Juntar las los datos originales, estandarizados y los scores o puntuaciones de los 3 componentes
data_total <- cbind(data, datos.estan, acp$li[,1:3]); data_total # score o puntuación: es el resumen de todos los 
                                                                 # datos juntos de cada  fila correspondiente.

# Grafico de los scores con 2 dimensiones
library(factoextra)
fviz_pca_ind(acp)

# Grafico de los scores con sus componetes
fviz_pca_biplot(acp, repel = FALSE,
                col.var = "steelblue",
                col.ind = "black",
                select.ind = list(contrib = 95)) # especificar la cantidad de scores

# ------------ #
# CONCLUSIONES #
# ------------ #

# Componente 1: Relación intrapersonales de individuos que comparten los mismos interes 
# Componente 2: Nivel de debilidad de las relaciones
# Componente 3: Tiempo y privacidad