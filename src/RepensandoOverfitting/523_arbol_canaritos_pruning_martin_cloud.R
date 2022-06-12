#limpio la memoria
rm( list=ls() )
gc()

library("data.table")
library("rpart")
library("rpart.plot")




################## Funcion Arbol canarios ###############


arbolesCanaritos  <- function( dtrain,dapply,pesos,labels)
{
  
  #ordeno para que queden en orden BAJA+1, BAJA+2, CONTINUA
  #The order of the loss matrix depends on the order of your factor variable in R
  setorder( dtrain, clase_ternaria )
  
  if(class(dtrain$clase_ternaria)=="factor")
    dtrain$clase_ternaria=as.character(dtrain$clase_ternaria)
  
  
  campos_originales  <- copy( colnames( dtrain ) )
  NumeroCanaritos=round(length(names(dtrain))*0.2)
  
  
  #agrego canaritos
  for( i in 1:NumeroCanaritos)  dtrain[ , paste0("canarito", i ) :=  runif( nrow(dtrain) ) ]
  for( i in 1:NumeroCanaritos)  dapply[ , paste0("canarito", i ) :=  runif( nrow(dapply) ) ]
  
  #reodeno los campos de  dtrain de forma que los canaritos queden primeros
  campos_extendidos  <- colnames( dtrain )
  campos_lindos  <-  c( setdiff( campos_extendidos, campos_originales ),  campos_originales )
  setcolorder(  dtrain, campos_lindos )
  
  auxdf=NULL
  datasetFinal=data.frame(dapply)
  
  
  for(i in 1:length(pesos))
  {
    #la matrix de perdida,  con 59 vs 1 , definida por el problema
    matriz_perdida  <- matrix(c( 0,pesos[i],1,   1,0,1,   1,pesos[i],0), nrow = 3)
    
    #Genero un arbol sin limite
    #los canaritos me van a proteger
    modelo_original  <- rpart(formula= "clase_ternaria ~ . ",
                              data= dtrain,
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
    
    
    prediccion  <- predict( modelo_pruned, dapply, type = "prob")[,"BAJA+2"]
    
    nombre=labels[i]
    valor=prediccion
    auxdf = data.frame(valor)
    colnames(auxdf)=nombre
    
    datasetFinal=cbind(datasetFinal,auxdf)
    auxdf=NULL
    
  }
  datasetFinal=data.table(datasetFinal)
  
  return(datasetFinal)
  
}





#############################################################



#setwd( "D:\\gdrive\\ITBA2022A\\" )  #establezco la carpeta donde voy a trabajar
#setwd("C:\\Users\\Martin\\Desktop\\MineriaDeDatos\\")  
setwd("~/buckets/b1/")   #Establezco el Working Directory

dataset  <- fread("./datasets/paquete_premium.csv.gz", stringsAsFactors= TRUE)

setorder( dataset, clase_ternaria )




#cargo donde voy a aplicar el modelo
#dapply  <- fread( "./datasets/paquete_premium_202101.csv")


#uso esta semilla para los canaritos
set.seed(10219)
#pesos=c(1,25,59,120,180,220,400)
pesos=c(1)
labels=pesos
for(i in 1:length(pesos))
{
  labels[i]=paste0("probCan",as.character(pesos[i]))
  dataset[ , paste0( labels[i], "") := -1  ]
}

dataset$probCan1=NA

#### Agrego la probabilidad de darse de baja a Enero 2021 #####
dtrain=dataset[foto_mes==202011]
setorder( dtrain, clase_ternaria )
dapply=dataset[foto_mes==202101]

datasetFinal=arbolesCanaritos(dtrain,dapply,pesos,labels)

#Esta asignacion esta mal, tengo que ver como hacerlo sin referenciarlo directamente
dataset[foto_mes==202101,probCan1:=datasetFinal$probCan1]
dataset[foto_mes==202101,probCan25:=datasetFinal$probCan25]
dataset[foto_mes==202101,probCan59:=datasetFinal$probCan59]
dataset[foto_mes==202101,probCan120:=datasetFinal$probCan120]
dataset[foto_mes==202101,probCan180:=datasetFinal$probCan180]
dataset[foto_mes==202101,probCan220:=datasetFinal$probCan220]
dataset[foto_mes==202101,probCan400:=datasetFinal$probCan400]

#### Fin de Enero 2021 #####

#### Agrego la probabilidad de darse de baja a Diciembre 2020 #####

dtrain=dataset[foto_mes==202010]
setorder( dtrain, clase_ternaria )
dapply=dataset[foto_mes==202012]

datasetFinal=arbolesCanaritos(dtrain,dapply,pesos,labels)

#Esta asignacion esta mal, tengo que ver como hacerlo sin referenciarlo directamente
dataset[foto_mes==202012,probCan1:=datasetFinal$probCan1]
dataset[foto_mes==202012,probCan25:=datasetFinal$probCan25]
dataset[foto_mes==202012,probCan59:=datasetFinal$probCan59]
dataset[foto_mes==202012,probCan120:=datasetFinal$probCan120]
dataset[foto_mes==202012,probCan180:=datasetFinal$probCan180]
dataset[foto_mes==202012,probCan220:=datasetFinal$probCan220]
dataset[foto_mes==202012,probCan400:=datasetFinal$probCan400]

#### Fin de Diciembre 2020 #####



#### Agrego la probabilidad de darse de baja a Noviembre 2020 #####

dtrain=dataset[foto_mes==202009]
setorder( dtrain, clase_ternaria )
dapply=dataset[foto_mes==202011]

datasetFinal=arbolesCanaritos(dtrain,dapply,pesos,labels)

#Esta asignacion esta mal, tengo que ver como hacerlo sin referenciarlo directamente
dataset[foto_mes==202011,probCan1:=datasetFinal$probCan1]
dataset[foto_mes==202011,probCan25:=datasetFinal$probCan25]
dataset[foto_mes==202011,probCan59:=datasetFinal$probCan59]
dataset[foto_mes==202011,probCan120:=datasetFinal$probCan120]
dataset[foto_mes==202011,probCan180:=datasetFinal$probCan180]
dataset[foto_mes==202011,probCan220:=datasetFinal$probCan220]
dataset[foto_mes==202011,probCan400:=datasetFinal$probCan400]

#### Fin de Noviembre 2020 #####

#### Agrego la probabilidad de darse de baja a Octubre 2020 #####

dtrain=dataset[foto_mes==202008]
setorder( dtrain, clase_ternaria )
dapply=dataset[foto_mes==202010]

datasetFinal=arbolesCanaritos(dtrain,dapply,pesos,labels)

#Esta asignacion esta mal, tengo que ver como hacerlo sin referenciarlo directamente
dataset[foto_mes==202010,probCan1:=datasetFinal$probCan1]
dataset[foto_mes==202010,probCan25:=datasetFinal$probCan25]
dataset[foto_mes==202010,probCan59:=datasetFinal$probCan59]
dataset[foto_mes==202010,probCan120:=datasetFinal$probCan120]
dataset[foto_mes==202010,probCan180:=datasetFinal$probCan180]
dataset[foto_mes==202010,probCan220:=datasetFinal$probCan220]
dataset[foto_mes==202010,probCan400:=datasetFinal$probCan400]

#### Fin de Octubre 2020 #####


#### Agrego la probabilidad de darse de baja a Sept 2020 #####

dtrain=dataset[foto_mes==202007]  
setorder( dtrain, clase_ternaria )
dapply=dataset[foto_mes==202009]  

datasetFinal=arbolesCanaritos(dtrain,dapply,pesos,labels)

#Esta asignacion esta mal, tengo que ver como hacerlo sin referenciarlo directamente
dataset[foto_mes==202009,probCan1:=datasetFinal$probCan1]
dataset[foto_mes==202009,probCan25:=datasetFinal$probCan25]
dataset[foto_mes==202009,probCan59:=datasetFinal$probCan59]
dataset[foto_mes==202009,probCan120:=datasetFinal$probCan120]
dataset[foto_mes==202009,probCan180:=datasetFinal$probCan180]
dataset[foto_mes==202009,probCan220:=datasetFinal$probCan220]
dataset[foto_mes==202009,probCan400:=datasetFinal$probCan400]

#### Fin de Sept 2020 #####

#### Fin de Octubre 2020 #####


#### Agrego la probabilidad de darse de baja a Agosto 2020 #####

dtrain=dataset[foto_mes==202005]  #OJO entreno con mayo xq junio es horrible
setorder( dtrain, clase_ternaria )
dapply=dataset[foto_mes==202008]  

datasetFinal=arbolesCanaritos(dtrain,dapply,pesos,labels)

#Esta asignacion esta mal, tengo que ver como hacerlo sin referenciarlo directamente
dataset[foto_mes==202008,probCan1:=datasetFinal$probCan1]
dataset[foto_mes==202008,probCan25:=datasetFinal$probCan25]
dataset[foto_mes==202008,probCan59:=datasetFinal$probCan59]
dataset[foto_mes==202008,probCan120:=datasetFinal$probCan120]
dataset[foto_mes==202008,probCan180:=datasetFinal$probCan180]
dataset[foto_mes==202008,probCan220:=datasetFinal$probCan220]
dataset[foto_mes==202008,probCan400:=datasetFinal$probCan400]

#### Fin de Agosto 2020 #####



#### Agrego la probabilidad de darse de baja a Julio 2020 #####

dtrain=dataset[foto_mes==202005]  
setorder( dtrain, clase_ternaria )
dapply=dataset[foto_mes==202007]  

datasetFinal=arbolesCanaritos(dtrain,dapply,pesos,labels)

#Esta asignacion esta mal, tengo que ver como hacerlo sin referenciarlo directamente
dataset[foto_mes==202007,probCan1:=datasetFinal$probCan1]
dataset[foto_mes==202007,probCan25:=datasetFinal$probCan25]
dataset[foto_mes==202007,probCan59:=datasetFinal$probCan59]
dataset[foto_mes==202007,probCan120:=datasetFinal$probCan120]
dataset[foto_mes==202007,probCan180:=datasetFinal$probCan180]
dataset[foto_mes==202007,probCan220:=datasetFinal$probCan220]
dataset[foto_mes==202007,probCan400:=datasetFinal$probCan400]

#### Fin de Julio 2020 #####

## IGNORO JUNIO


#### Agrego la probabilidad de darse de baja a  Mayo 2020 #####

dtrain=dataset[foto_mes==202003]  
setorder( dtrain, clase_ternaria )
dapply=dataset[foto_mes==202005]  

datasetFinal=arbolesCanaritos(dtrain,dapply,pesos,labels)

#Esta asignacion esta mal, tengo que ver como hacerlo sin referenciarlo directamente
dataset[foto_mes==202003,probCan1:=datasetFinal$probCan1]
dataset[foto_mes==202003,probCan25:=datasetFinal$probCan25]
dataset[foto_mes==202003,probCan59:=datasetFinal$probCan59]
dataset[foto_mes==202003,probCan120:=datasetFinal$probCan120]
dataset[foto_mes==202003,probCan180:=datasetFinal$probCan180]
dataset[foto_mes==202003,probCan220:=datasetFinal$probCan220]
dataset[foto_mes==202003,probCan400:=datasetFinal$probCan400]

#### Fin de Mayo 2020 #####


#### Agrego la probabilidad de darse de baja a  Abril 2020 #####

dtrain=dataset[foto_mes==202002]  
setorder( dtrain, clase_ternaria )
dapply=dataset[foto_mes==202004]  

datasetFinal=arbolesCanaritos(dtrain,dapply,pesos,labels)

#Esta asignacion esta mal, tengo que ver como hacerlo sin referenciarlo directamente
dataset[foto_mes==202002,probCan1:=datasetFinal$probCan1]
dataset[foto_mes==202002,probCan25:=datasetFinal$probCan25]
dataset[foto_mes==202002,probCan59:=datasetFinal$probCan59]
dataset[foto_mes==202002,probCan120:=datasetFinal$probCan120]
dataset[foto_mes==202002,probCan180:=datasetFinal$probCan180]
dataset[foto_mes==202002,probCan220:=datasetFinal$probCan220]
dataset[foto_mes==202002,probCan400:=datasetFinal$probCan400]

#### Fin de Abril 2020 #####


#### Agrego la probabilidad de darse de baja a  Marzo 2020 #####

dtrain=dataset[foto_mes==202001]  
setorder( dtrain, clase_ternaria )
dapply=dataset[foto_mes==202003]  

datasetFinal=arbolesCanaritos(dtrain,dapply,pesos,labels)

#Esta asignacion esta mal, tengo que ver como hacerlo sin referenciarlo directamente
dataset[foto_mes==202003,probCan1:=datasetFinal$probCan1]
dataset[foto_mes==202003,probCan25:=datasetFinal$probCan25]
dataset[foto_mes==202003,probCan59:=datasetFinal$probCan59]
dataset[foto_mes==202003,probCan120:=datasetFinal$probCan120]
dataset[foto_mes==202003,probCan180:=datasetFinal$probCan180]
dataset[foto_mes==202003,probCan220:=datasetFinal$probCan220]
dataset[foto_mes==202003,probCan400:=datasetFinal$probCan400]

#### Fin de Marzo 2020 #####

#### Agrego la probabilidad de darse de baja a  Febrero 2020 #####

dtrain=dataset[foto_mes==201912]  
setorder( dtrain, clase_ternaria )
dapply=dataset[foto_mes==202002]  

datasetFinal=arbolesCanaritos(dtrain,dapply,pesos,labels)

#Esta asignacion esta mal, tengo que ver como hacerlo sin referenciarlo directamente
dataset[foto_mes==202002,probCan1:=datasetFinal$probCan1]
dataset[foto_mes==202002,probCan25:=datasetFinal$probCan25]
dataset[foto_mes==202002,probCan59:=datasetFinal$probCan59]
dataset[foto_mes==202002,probCan120:=datasetFinal$probCan120]
dataset[foto_mes==202002,probCan180:=datasetFinal$probCan180]
dataset[foto_mes==202002,probCan220:=datasetFinal$probCan220]
dataset[foto_mes==202002,probCan400:=datasetFinal$probCan400]

#### Fin de Febrero 2020 #####


#### Agrego la probabilidad de darse de baja a  Enero 2020 #####

dtrain=dataset[foto_mes==201912]  
setorder( dtrain, clase_ternaria )
dapply=dataset[foto_mes==202002]  

datasetFinal=arbolesCanaritos(dtrain,dapply,pesos,labels)

#Esta asignacion esta mal, tengo que ver como hacerlo sin referenciarlo directamente
dataset[foto_mes==202002,probCan1:=datasetFinal$probCan1]
dataset[foto_mes==202002,probCan25:=datasetFinal$probCan25]
dataset[foto_mes==202002,probCan59:=datasetFinal$probCan59]
dataset[foto_mes==202002,probCan120:=datasetFinal$probCan120]
dataset[foto_mes==202002,probCan180:=datasetFinal$probCan180]
dataset[foto_mes==202002,probCan220:=datasetFinal$probCan220]
dataset[foto_mes==202002,probCan400:=datasetFinal$probCan400]

#### Fin de Enero 2020 #####






entrega=datasetFinal[, .(numero_de_cliente,probCan59, probCan120,probCan180,probCan220,probCan400)]



#creo la carepta donde guardo el resultado
dir.create( "./labo/exp/",  showWarnings = FALSE ) 
dir.create( "./labo/exp/KA5230/", showWarnings = FALSE )
#setwd("D:\\gdrive\\ITBA2022A\\labo\\exp\\KA5230\\")   #Establezco el Working Directory DEL EXPERIMENTO
setwd("C:\\Users\\Martin\\Desktop\\MineriaDeDatos\\labo\\exp\\KA5230\\")  


#grabo la salida para Kaggle
fwrite( entrega, 
        file= "stopping_at_canaritos.csv",
        sep= "," ) 







#cargo el dataset donde voy a entrenar

#cargo donde voy a aplicar el modelo



#dapply  <- fread( "./datasets/paquete_premium_202101.csv")

