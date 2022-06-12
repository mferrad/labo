#limpio la memoria
rm( list=ls() )
gc()

library("data.table")
library("rpart")
library("rpart.plot")




################## Funcion Arbol canarios ###############


arbolesCanaritos  <- function( dtrain,dapply,peso)
{
  
  #creo copias locales para que no me modifique nada
  
  dtrain1=data.table(dtrain)
  dapply1=data.table(dapply)
  
  
  #ordeno para que queden en orden BAJA+1, BAJA+2, CONTINUA
  #The order of the loss matrix depends on the order of your factor variable in R
  setorder( dtrain1, clase_ternaria )
  
  if(class(dtrain1$clase_ternaria)=="factor")
    dtrain1$clase_ternaria=as.character(dtrain1$clase_ternaria)
  
  
  campos_originales  <- copy( colnames( dtrain1 ) )
  NumeroCanaritos=round(length(names(dtrain1))*0.2)
  
  
  #agrego canaritos
  for( i in 1:NumeroCanaritos)  dtrain1[ , paste0("canarito", i ) :=  runif( nrow(dtrain1) ) ]
  for( i in 1:NumeroCanaritos)  dapply1[ , paste0("canarito", i ) :=  runif( nrow(dapply1) ) ]
  
  #reodeno los campos de  dtrain1 de forma que los canaritos queden primeros
  campos_extendidos  <- colnames( dtrain1 )
  campos_lindos  <-  c( setdiff( campos_extendidos, campos_originales ),  campos_originales )
  setcolorder(  dtrain1, campos_lindos )
  
  
  #la matrix de perdida,  con 59 vs 1 , definida por el problema
  matriz_perdida  <- matrix(c( 0,peso,1,   1,0,1,   1,peso,0), nrow = 3)
  
  #Genero un arbol sin limite
  #los canaritos me van a proteger
  modelo_original  <- rpart(formula= "clase_ternaria ~ . ",
                            data= dtrain1,
                            xval= 0,
                            model= TRUE,
                            cp=        -1,
                            maxdepth=  30,  #lo dejo crecer lo mas posible
                            minsplit=   2,
                            minbucket=  1, 
                            parms= list( loss= matriz_perdida)  )
  
  
  #hago el pruning de los canaritos
  #haciendo un hackeo a la estructura  modelo_original$frame
  # -666 es un valor arbritrariamente negativo que jamas es generado por rpart
  modelo_original$frame[ modelo_original$frame$var %like% "canarito", "complexity"] <- -666
  modelo_pruned  <- prune(  modelo_original, -666 )
  
  nodes <- as.numeric(rownames(modelo_pruned$frame))
  max(rpart:::tree.depth(nodes))
  
  
  prediccion  <- predict( modelo_pruned, dapply1, type = "prob")[,"BAJA+2"]
  
  return(prediccion)
  
}





#############################################################



#setwd( "D:\\gdrive\\ITBA2022A\\" )  #establezco la carpeta donde voy a trabajar
#setwd("C:\\Users\\Martin\\Desktop\\MineriaDeDatos\\")  
setwd("~/buckets/b1/")   #Establezco el Working Directory

dataset  <- fread("./datasets/paquete_premium.csv.gz", stringsAsFactors= TRUE)



setorder( dataset, clase_ternaria )

datasetAux=data.table(dataset)


#cargo donde voy a aplicar el modelo
#dapply  <- fread( "./datasets/paquete_premium_202101.csv")


#uso esta semilla para los canaritos
set.seed(10219)

#### Agrego la probabilidad de darse de baja a Enero 2021 #####
dtrain=datasetAux[foto_mes==202011]
setorder( dtrain, clase_ternaria )
dapply=datasetAux[foto_mes==202101]


dataset[foto_mes==202101,probCan1:=arbolesCanaritos(dtrain,dapply,peso=1)]
dataset[foto_mes==202101,probCan59:=arbolesCanaritos(dtrain,dapply,peso=59)]
dataset[foto_mes==202101,probCan400:=arbolesCanaritos(dtrain,dapply,peso=400)]

print(dataset[foto_mes==202101])

#### Fin de Enero 2021 #####


#### Agrego la probabilidad de darse de baja a Diciembre 2020 #####

dtrain=datasetAux[foto_mes==202010]
setorder( dtrain, clase_ternaria )
dapply=datasetAux[foto_mes==202012]

dataset[foto_mes==202012,probCan1:=arbolesCanaritos(dtrain,dapply,peso=1)]
dataset[foto_mes==202012,probCan59:=arbolesCanaritos(dtrain,dapply,peso=59)]
dataset[foto_mes==202012,probCan400:=arbolesCanaritos(dtrain,dapply,peso=400)]

print(dataset[foto_mes==202012])

#### Fin de Diciembre 2020 #####






#### Agrego la probabilidad de darse de baja a Noviembre 2020 #####

dtrain=datasetAux[foto_mes==202009]
setorder( dtrain, clase_ternaria )
dapply=datasetAux[foto_mes==202011]

dataset[foto_mes==202011,probCan1:=arbolesCanaritos(dtrain,dapply,peso=1)]
dataset[foto_mes==202011,probCan59:=arbolesCanaritos(dtrain,dapply,peso=59)]
dataset[foto_mes==202011,probCan400:=arbolesCanaritos(dtrain,dapply,peso=400)]

print(dataset[foto_mes==202011])

#### Fin de Noviembre 2020 #####

#### Agrego la probabilidad de darse de baja a Octubre 2020 #####

dtrain=datasetAux[foto_mes==202008]
setorder( dtrain, clase_ternaria )
dapply=datasetAux[foto_mes==202010]

dataset[foto_mes==202010,probCan1:=arbolesCanaritos(dtrain,dapply,peso=1)]
dataset[foto_mes==202010,probCan59:=arbolesCanaritos(dtrain,dapply,peso=59)]
dataset[foto_mes==202010,probCan400:=arbolesCanaritos(dtrain,dapply,peso=400)]

print(dataset[foto_mes==202010])

#### Fin de Octubre 2020 #####


#### Agrego la probabilidad de darse de baja a Sept 2020 #####

dtrain=datasetAux[foto_mes==202007]
setorder( dtrain, clase_ternaria )
dapply=datasetAux[foto_mes==202009]

dataset[foto_mes==202009,probCan1:=arbolesCanaritos(dtrain,dapply,peso=1)]
dataset[foto_mes==202009,probCan59:=arbolesCanaritos(dtrain,dapply,peso=59)]
dataset[foto_mes==202009,probCan400:=arbolesCanaritos(dtrain,dapply,peso=400)]

print(dataset[foto_mes==202009])


#### Fin de Sept 2020 #####



#### Agrego la probabilidad de darse de baja a Agosto 2020 #####

dtrain=datasetAux[foto_mes==202005]   #Aca uso mayo porque junio es horrible
setorder( dtrain, clase_ternaria )
dapply=datasetAux[foto_mes==202008]

dataset[foto_mes==202008,probCan1:=arbolesCanaritos(dtrain,dapply,peso=1)]
dataset[foto_mes==202008,probCan59:=arbolesCanaritos(dtrain,dapply,peso=59)]
dataset[foto_mes==202008,probCan400:=arbolesCanaritos(dtrain,dapply,peso=400)]

print(dataset[foto_mes==202008])

#### Fin de Agosto 2020 #####



#### Agrego la probabilidad de darse de baja a Julio 2020 #####

dtrain=datasetAux[foto_mes==202005]   
setorder( dtrain, clase_ternaria )
dapply=datasetAux[foto_mes==202007]

dataset[foto_mes==202007,probCan1:=arbolesCanaritos(dtrain,dapply,peso=1)]
dataset[foto_mes==202007,probCan59:=arbolesCanaritos(dtrain,dapply,peso=59)]
dataset[foto_mes==202007,probCan400:=arbolesCanaritos(dtrain,dapply,peso=400)]

print(dataset[foto_mes==202007])

#### Fin de Julio 2020 #####

## IGNORO JUNIO


#### Agrego la probabilidad de darse de baja a  Mayo 2020 #####

dtrain=datasetAux[foto_mes==202003]   
setorder( dtrain, clase_ternaria )
dapply=datasetAux[foto_mes==202005]

dataset[foto_mes==202005,probCan1:=arbolesCanaritos(dtrain,dapply,peso=1)]
dataset[foto_mes==202005,probCan59:=arbolesCanaritos(dtrain,dapply,peso=59)]
dataset[foto_mes==202005,probCan400:=arbolesCanaritos(dtrain,dapply,peso=400)]

print(dataset[foto_mes==202005])


#### Fin de Mayo 2020 #####


#### Agrego la probabilidad de darse de baja a  Abril 2020 #####

dtrain=datasetAux[foto_mes==202002]   
setorder( dtrain, clase_ternaria )
dapply=datasetAux[foto_mes==202004]

dataset[foto_mes==202004,probCan1:=arbolesCanaritos(dtrain,dapply,peso=1)]
dataset[foto_mes==202004,probCan59:=arbolesCanaritos(dtrain,dapply,peso=59)]
dataset[foto_mes==202004,probCan400:=arbolesCanaritos(dtrain,dapply,peso=400)]

print(dataset[foto_mes==202004])


#### Fin de Abril 2020 #####


#### Agrego la probabilidad de darse de baja a  Marzo 2020 #####

dtrain=datasetAux[foto_mes==202001]   
setorder( dtrain, clase_ternaria )
dapply=datasetAux[foto_mes==202003]

dataset[foto_mes==202003,probCan1:=arbolesCanaritos(dtrain,dapply,peso=1)]
dataset[foto_mes==202003,probCan59:=arbolesCanaritos(dtrain,dapply,peso=59)]
dataset[foto_mes==202003,probCan400:=arbolesCanaritos(dtrain,dapply,peso=400)]

print(dataset[foto_mes==202003])

#### Fin de Marzo 2020 #####

#### Agrego la probabilidad de darse de baja a  Febrero 2020 #####

dtrain=datasetAux[foto_mes==201912]   
setorder( dtrain, clase_ternaria )
dapply=datasetAux[foto_mes==202002]

dataset[foto_mes==202002,probCan1:=arbolesCanaritos(dtrain,dapply,peso=1)]
dataset[foto_mes==202002,probCan59:=arbolesCanaritos(dtrain,dapply,peso=59)]
dataset[foto_mes==202002,probCan400:=arbolesCanaritos(dtrain,dapply,peso=400)]

print(dataset[foto_mes==202002])


#### Fin de Febrero 2020 #####


#### Agrego la probabilidad de darse de baja a  Enero 2020 #####

dtrain=datasetAux[foto_mes==201911]   
setorder( dtrain, clase_ternaria )
dapply=datasetAux[foto_mes==202001]

dataset[foto_mes==202001,probCan1:=arbolesCanaritos(dtrain,dapply,peso=1)]
dataset[foto_mes==202001,probCan59:=arbolesCanaritos(dtrain,dapply,peso=59)]
dataset[foto_mes==202001,probCan400:=arbolesCanaritos(dtrain,dapply,peso=400)]

print(dataset[foto_mes==202001])


#### Fin de Enero 2020 #####



