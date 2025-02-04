#limpio la memoria
rm( list=ls() )
gc()

library("data.table")
library("rpart")
library("rpart.plot")




################## Funcion Arbol canarios ###############


arbolesCanaritos  <- function( dtrain,dapply,pesos,labels)
{
  
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
setwd("C:\\Users\\Martin\\Desktop\\MineriaDeDatos\\")  

#cargo el dataset
dtrain  <- fread( "./datasets/paquete_premium_202011.csv")
#ordeno para que queden en orden BAJA+1, BAJA+2, CONTINUA
#The order of the loss matrix depends on the order of your factor variable in R
setorder( dtrain, clase_ternaria )

#cargo donde voy a aplicar el modelo
dapply  <- fread( "./datasets/paquete_premium_202101.csv")

#uso esta semilla para los canaritos
set.seed(10219)
pesos=c(59,120,180,220,400)
labels=pesos
for(i in 1:length(pesos))
{
  labels[i]=paste0("probCan",as.character(pesos[i]))
}
  
datasetFinal=arbolesCanaritos(dtrain,dapply,pesos,labels)



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

