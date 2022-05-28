#esqueleto de grid search
#se espera que los alumnos completen lo que falta para recorrer TODOS cuatro los hiperparametros 

rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("rpart")
require("parallel")

ksemillas  <- c(200443, 200461, 200467, 200569, 200587) #reemplazar por las propias semillas

#------------------------------------------------------------------------------
#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30 

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
          by= agrupa ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia  <- function( semilla, param_basicos )
{
  #particiono estratificadamente el dataset
  particionar( dataset, division=c(70,30), agrupa="clase_ternaria", seed= semilla )  #Cambiar por la primer semilla de cada uno !

  #genero el modelo
  modelo  <- rpart("clase_ternaria ~ .",     #quiero predecir clase_ternaria a partir del resto
                   data= dataset[ fold==1],  #fold==1  es training,  el 70% de los datos
                   xval= 0,
                   control= param_basicos )  #aqui van los parametros del arbol

  #aplico el modelo a los datos de testing
  prediccion  <- predict( modelo,   #el modelo que genere recien
                          dataset[ fold==2],  #fold==2  es testing, el 30% de los datos
                          type= "prob") #type= "prob"  es que devuelva la probabilidad

  #prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  #cada columna es el vector de probabilidades 


  #calculo la ganancia en testing  qu es fold==2
  ganancia_test  <- dataset[ fold==2, 
                             sum( ifelse( prediccion[, "BAJA+2"]  >  1/60,
                                         ifelse( clase_ternaria=="BAJA+2", 59000, -1000 ),
                                         0 ) )]

  #escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada  <-  ganancia_test / 0.3

  return( ganancia_test_normalizada )
}
#------------------------------------------------------------------------------

ArbolesMontecarlo  <- function( semillas, param_basicos )
{
  #la funcion mcmapply  llama a la funcion ArbolEstimarGanancia  tantas veces como valores tenga el vector  ksemillas
  ganancias  <- mcmapply( ArbolEstimarGanancia, 
                          semillas,   #paso el vector de semillas, que debe ser el primer parametro de la funcion ArbolEstimarGanancia
                          MoreArgs= list( param_basicos),  #aqui paso el segundo parametro
                          SIMPLIFY= FALSE,
                          mc.cores= 1 )  #se puede subir a 5 si posee Linux o Mac OS

  ganancia_promedio  <- mean( unlist(ganancias) )

  return( ganancia_promedio )
}
#------------------------------------------------------------------------------

#Aqui se debe poner la carpeta de la computadora local
setwd("C:\\Users\\Martin\\Desktop\\MineriaDeDatos\\")   #Establezco el Working Directory

#cargo los datos
dataset  <- fread("./datasets/paquete_premium_202011.csv")


#genero el archivo para Kaggle
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./labo/exp/",  showWarnings = FALSE ) 
dir.create( "./labo/exp/HT2020/", showWarnings = FALSE )
archivo_salida  <- "./labo/exp/HT2020/gridsearch2.txt"

#Escribo los titulos al archivo donde van a quedar los resultados
#atencion que si ya existe el archivo, esta instruccion LO SOBREESCRIBE, y lo que estaba antes se pierde
#la forma que no suceda lo anterior es con append=TRUE
cat( file=archivo_salida,
     sep= "",
     "max_depth", "\t",
     "min_split", "\t",
     "vmin_bucket", "\t",
     "cpValue", "\t",
     "ganancia_promedio", "\n")


#itero por los loops anidados para cada hiperparametro

myCount=1

#for(cpValue in c(-10,-3,-1,-0.7,-0.3,-0.1,-0.01,0, 0.01, 0.05, 0.1, 0.3, 0.7, 1, 3, 10))
for(cpValue in c(0.01))
{
for( vmax_depth  in  c( 20)  )
#for( vmax_depth  in  c( 4, 6, 8, 10, 12, 14, 16 )  )
{
#for( vmin_split  in  c(1000, 800, 600, 400, 200, 100, 50, 20, 10 )  )
for( vmin_split  in  c(20))
{
#for(vmin_bucket in (c(1000, 800, 600, 400, 200, 100, 50, 20, 10 )/2)  )
for(vmin_bucket in c(6))
{

  if(vmin_bucket*2>vmin_split)
    next
  
  print(myCount)
  myCount=myCount+1
  
  #notar como se agrega
  param_basicos  <- list( "cp"=        cpValue,     #complejidad minima
                          "minsplit"=  vmin_split,  #minima cantidad de registros en un nodo para hacer el split
                          "minbucket"= vmin_bucket, #minima cantidad de registros en una hoja
                          "maxdepth"=  vmax_depth ) #profundidad mÃ¡xima del arbol

  #Un solo llamado, con la semilla 17
  ganancia_promedio  <- ArbolesMontecarlo( ksemillas,  param_basicos )

  #escribo los resultados al archivo de salida
  cat(  file=archivo_salida,
        append= TRUE,
        sep= "",
        vmax_depth, "\t",
        vmin_split, "\t",
        vmin_bucket, "\t",
        cpValue, "\t",
        ganancia_promedio, "\n"  )

}
}
}
}


###NOtes###
#
# 1) Max depth: si max depth es 16
#
# Hay 160k datos
#
# Si todos los nodos tienen la misma cantida de observaciones cada nodo tiene: 
#
# 160000/(2^16)
# 2.441406
#
# No tiene sentido que sea mas de 16
#
# 2) Min split
#
# En el set de prueba hay un total de 648 baja +2 -> el 30% de eso que uso para entrenar es 195
# 
# Solo me interezan las hojas que tiene al menos 1 entre 60 baja +2
#
# El maximo que tiene sentido analizar es 195*60 = 11.7k 
#
# Pero esto se dividide entre varias hojas => uso 1k como maximo
#
#
# 3) Min bucket
#
#  Min bucket tiene que ser al menos menor que la mitad de min Split => itero con los valores de Min split dividido 2
#
# 4) cp
#
# The default value of the cp is .01. To tell you how to calculate cp is beyond the scope of our discussion here. 
# Just think of cp as the "minimum benefit" that a split must add to the tree. 
# If the split doesn't yield at least that much benefit (the value of cp), rpart() doesn't add it.
#
# What happens if you set cp to .00? 
# You get no restrictions on what a split must add. 
# Hence, you wind up with the most complex tree possible. 
# 
# Como el valor por defecto es 0.01 voy a centarme en valores cercanos a esos y moverme cada vez en pasos mas largos -> quiero 15 valores solo
#
#    c(-10,-3,-1,-0.7,-0.3,-0.1,-0.01,0, 0.01, 0.05, 0.1, 0.3, 0.7, 1, 3, 10)


#cargo los datos
Resultados = fread(archivo_salida)
#Resultados=fread("./labo/exp/HT2020/gridsearchOriginal.txt")

max(Resultados$ganancia_promedio)
which.max(Resultados$ganancia_promedio)
Resultados[which.max(Resultados$ganancia_promedio)]

### Grafico un histograma de los datos ####

hist(Resultados$ganancia_promedio, probability=TRUE,breaks=30)
lines(density(Resultados$ganancia_promedio), col="violet", lwd = 2) #normal

boxplot(Resultados$ganancia_promedio)
ThirdQ=summary(Resultados$ganancia_promedio)[5] #Calculo el tercer cuadrante


##Elimino todo lo que esta abajo del 3Q
ResultadosFiltrados=Resultados[Resultados$ganancia_promedio>ThirdQ]
hist(ResultadosFiltrados$ganancia_promedio, probability=TRUE,breaks=30)
lines(density(ResultadosFiltrados$ganancia_promedio), col="violet", lwd = 2) #normal
boxplot(ResultadosFiltrados$ganancia_promedio)


##Vuelvo a eliminar todo lo que esta en el segundo cuadrante

primerFiltroThirdQ=summary(ResultadosFiltrados$ganancia_promedio)[5]
ResultadosFiltrados2=ResultadosFiltrados[ResultadosFiltrados$ganancia_promedio>primerFiltroThirdQ]
hist(ResultadosFiltrados2$ganancia_promedio, probability=TRUE,breaks=30)
lines(density(ResultadosFiltrados2$ganancia_promedio), col="violet", lwd = 2) #normal

boxplot(ResultadosFiltrados2$ganancia_promedio)

boxplot(ResultadosFiltrados2$cpValue)
hist(ResultadosFiltrados2$cpValue, probability=TRUE,breaks=30)
plot(ResultadosFiltrados2$cpValue,ResultadosFiltrados2$ganancia_promedio)

boxplot(ResultadosFiltrados2$max_depth)
hist(ResultadosFiltrados2$max_depth, probability=TRUE,breaks=30)
plot(ResultadosFiltrados2$max_depth,ResultadosFiltrados2$ganancia_promedio)

boxplot(ResultadosFiltrados2$min_split)
hist(ResultadosFiltrados2$min_split, probability=TRUE,breaks=30)
plot(ResultadosFiltrados2$min_split,ResultadosFiltrados2$ganancia_promedio)

boxplot(ResultadosFiltrados2$vmin_bucket)
hist(ResultadosFiltrados2$vmin_bucket, probability=TRUE,breaks=30)
plot(ResultadosFiltrados2$vmin_bucket,ResultadosFiltrados2$ganancia_promedio)

Mcor=cor(ResultadosFiltrados2)
library(ggcorrplot)
ggcorrplot(Mcor) + ggtitle('Matriz de Correlaciones')


####
# Conclusiones: 
# 
# CP no afecta demasiado, lo unico que importa es que sea negativo
# El resto de las variables parece mejorar la ganancia cuando se achican 
# Max depth parece concentrar los mejores valores de ganancia en el 8, aunque hay ejemplos de ganancias grandes en el 6 10 12 y 14 
# min_split: Hay de todo entre 5 y 100, estand el maximo absoluto en 800
# min_bucket: Parece ser que mientras mas chico mejor el 5 acumula muchos valores
#

### quiero probar valores bajso de y altos de ambas variables, tengo miedo que un valor bajo de min_split y min_bucket me llebe a que el
##  Algoritmo ajuste muy bien los datos de prueba y muy poco la poblacion, pero lo quiero probar igual, especialmente si Max depth es alta
##  CP lo ignoro para el analisis


#1)   Max depth baja, min split bajo, min bucket alta
      #NO hay combinacion posible por la restriccion
  
#2) Max depth baja, min split baja, min bucket baja

ResultadosFiltrados2[ResultadosFiltrados2$max_depth==8]
#max_depth min_split vmin_bucket cpValue ganancia_promedio
#42:         8        50           5   -0.70           9818667

#3) Max depth baja, min split alto, min bucket baja (Maximo)
ResultadosFiltrados2[ResultadosFiltrados2$max_depth==8]

#max_depth min_split vmin_bucket cpValue ganancia_promedio
#1:         8       800           5     -10          10100000


#4) Max depth baja, min split alto, min bucket alta

ResultadosFiltrados2[ResultadosFiltrados2$max_depth==6]

#max_depth min_split vmin_bucket cpValue ganancia_promedio
##1:         6       800         400     -10           9708000


#5)   Max depth alta , min split bajo, min bucket alta
#NO hay combinacion posible por la restriccion


#6) Max depth alta, min split baja, min bucket baja

#max_depth min_split vmin_bucket cpValue ganancia_promedio
#12        10           5    0.00           9713333

ResultadosFiltrados2[ResultadosFiltrados2$max_depth==12]

#7) Max depth alta, min split alto, min bucket baja 

#No hay (no por nada en especial, no hay)


#8) Max depth alta, min split alto, min bucket alta

ResultadosFiltrados2[ResultadosFiltrados2$max_depth==16]

#max_depth min_split vmin_bucket cpValue ganancia_promedio
#16       800         400   -0.30           9548000

################################ Fumada a ver que veo con la distancia del indio

# Calculo la distancia
means=apply(ResultadosFiltrados2,2,mean,na.rm =T)
covar= cov(ResultadosFiltrados2)
distancias = mahalanobis(ResultadosFiltrados2,means,covar)

#   
boxplot(distancias,xlab="Datos de estidio", ylab="Distancia de mahalanobis", main ="Distancia de mahalanobis de los datos de estudio",col="purple")

# Calculo analíticamente los límites del bloxpot para identificar outlayers
cuartiles=quantile(distancias,c(0.25,0.5,0.75))
distanciaIntercuartil=cuartiles[3]-cuartiles[1]  
limitTop=cuartiles[3]+1.5*distanciaIntercuartil 
limitBottom= cuartiles[1]-1.5*distanciaIntercuartil

# Calculo manualmente los outlayers
OutTop=ResultadosFiltrados2[distancias>limitTop,]
OutBottom=ResultadosFiltrados2[distancias<limitBottom,] 

# Imprimo los outlayers con sus datos
print(OutTop)  
print(OutBottom) 

# Imprimo solo el nombre
rownames(OutTop)
rownames(OutBottom)

### pruebo 1 outlayers de la distancia del indio

#max_depth min_split vmin_bucket cpValue ganancia_promedio
#4:        16       800         400     -10           9548000




