rm( list=ls() )
gc()

require("data.table")
require("stringr")

setwd("~/buckets/b1")   #Establezco el Working Directory

dataset  <- data.frame(fread("./exp/FE8120MartinFullSinLags/paquete_premium_ext.csv.gz", stringsAsFactors= TRUE))
targetMonthVars=dataset[dataset$foto_mes==202101,]
aux=data.frame(targetMonthVars)
dataset=NULL



dataset  <- data.frame(fread("./exp/ZZ8420MartinFullSinLags/futuro_prediccion_101.csv")) #uso un set de datos con la prob estimada en lugar del 1 0 en clase +2

sum(abs(dataset$numero_de_cliente-targetMonthVars$numero_de_cliente))

targetMonthVars$clase_ternaria=dataset$prob

dataset3=targetMonthVars

datasetFinal=dataset3

cor(dataset3$clase_ternaria,dataset3$clase_ternaria)

nombres=names(dataset3)

resultados=data.frame("feature"=nombres[1:(length(nombres)-1)],"correlacion"=1:(length(nombres)-1))


for (featureIndex in 1:(length(nombres)-1))
{
  resultados$feature[featureIndex]=nombres[featureIndex]
  resultados$correlacion[featureIndex]=cor(dataset3$clase_ternaria ,dataset3[,featureIndex],use="pairwise.complete.obs")
  
}

resultados=subset(resultados,resultados$correlacion!='NA') #elimino los NA
aux3=resultados
#resultados=aux3

resultados=subset(resultados,abs(resultados$correlacio)>0.4)  #elimino correlaciones bajas
resultados
