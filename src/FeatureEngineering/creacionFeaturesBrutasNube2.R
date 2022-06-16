#Feature Engineering
#creo nuevas variables dentro del mismo mes

#este script se muestra como esqueleto para que los alumnos agreguen sus propias variables
#ya sea basados en la teoria economica  o en el salvaje empiricismo
# "No es que la nueva variable creada, que funciona, no tenga sentido, lo que sucede es que yo estoy siendo capaz de encontrarselo en este momento"

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")






#################MIs FUNCIONES ########################

LimpiarDataset  <- function( dataset )
{
  
  #Borre todo lo que agregaba esta funcion, ahora solo limpia el dataset y crea el archivo
  
  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos      <- lapply( names(dataset),
                            function(.name) dataset[ , sum(is.infinite( get(.name) )) ]  )
  
  infinitos_qty  <- sum( unlist( infinitos ) )
  if( infinitos_qty > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <- NA
  }
  
  
  #valvula de seguridad para evitar valores NaN  que es 0/0
  #paso los NaN a 0 , decision polemica si las hay
  #se invita a asignar un valor razonable segun la semantica del campo creado
  nans      <- lapply( names(dataset),
                       function(.name) dataset[ , sum( is.nan( get(.name) )) ] )
  
  nans_qty  <- sum( unlist( nans) )
  if( nans_qty > 0 )
  {
    cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
    cat( "Si no te gusta la decision, modifica a gusto el script!\n\n")
    dataset[mapply(is.nan, dataset)] <- NA
  }
  
  #FIN de la seccion donde se deben hacer cambios con variables nuevas
  
  return(dataset)
}


fuerzaBruta  <- function( dataset3 , resultados )
{
  
  datasetFinal=dataset3
  
  varName='div'
  auxdf=NULL
  
  
  ### Operacion division
  
  for (i in 1:length(resultados$feature))
  {
    
    for(j in 1:length(resultados$feature))
    {
      
      if(resultados$feature[i]==resultados$feature[j])
        next
      
      nombre=str_replace_all(paste(varName,resultados$feature[i],resultados$feature[j])," ","_")
      valor=dataset3[,resultados$feature[i]]/dataset3[,resultados$feature[j]]
      nombre=gsub(" ","",nombre)
      
      auxdf = data.frame(valor)
      colnames(auxdf)=nombre
      
      datasetFinal=cbind(datasetFinal,auxdf)
      
      auxdf=NULL
      
    }
    
  }
  
  
  
  ### Operacion multiplicacion
  
  varName='mul'
  auxdf=NULL
  
  
  
  for (i in 1:length(resultados$feature))
  {
    
    for(j in i:length(resultados$feature))
    {
      
      if(resultados$feature[i]==resultados$feature[j])
        next
      
      nombre=str_replace_all(paste(varName,resultados$feature[i],resultados$feature[j])," ","_")
      valor=dataset3[,resultados$feature[i]]*dataset3[,resultados$feature[j]]
      nombre=gsub(" ","",nombre)
      
      auxdf = data.frame(valor)
      colnames(auxdf)=nombre
      
      datasetFinal=cbind(datasetFinal,auxdf)
      
      auxdf=NULL
      
    }
    
  }
  
  
  
  ### Operacion maximo
  
  varName='max'
  auxdf=NULL
  
  
  
  for (i in 1:length(resultados$feature))
  {
    
    for(j in i:length(resultados$feature))
    {
      
      if(resultados$feature[i]==resultados$feature[j])
        next
      
      nombre=str_replace_all(paste(varName,resultados$feature[i],resultados$feature[j])," ","_")
      valor=pmax( dataset3[,resultados$feature[i]],dataset3[,resultados$feature[j]], na.rm = TRUE)
      nombre=gsub(" ","",nombre)
      
      auxdf = data.frame(valor)
      colnames(auxdf)=nombre
      
      datasetFinal=cbind(datasetFinal,auxdf)
      
      auxdf=NULL
      
    }
    
  }
  
  ### Operacion minimo
  
  varName='min'
  auxdf=NULL
  
  
  
  for (i in 1:length(resultados$feature))
  {
    
    for(j in i:length(resultados$feature))
    {
      
      if(resultados$feature[i]==resultados$feature[j])
        next
      
      nombre=str_replace_all(paste(varName,resultados$feature[i],resultados$feature[j])," ","_")
      valor=pmin( dataset3[,resultados$feature[i]],dataset3[,resultados$feature[j]], na.rm = TRUE)
      nombre=gsub(" ","",nombre)
      
      auxdf = data.frame(valor)
      colnames(auxdf)=nombre
      
      datasetFinal=cbind(datasetFinal,auxdf)
      
      auxdf=NULL
      
    }
    
  }
  
  
  
  
  ### Operacion suma
  
  varName='suma'
  auxdf=NULL
  
  
  
  for (i in 1:length(resultados$feature))
  {
    
    for(j in i:length(resultados$feature))
    {
      
      if(resultados$feature[i]==resultados$feature[j])
        next
      
      nombre=str_replace_all(paste(varName,resultados$feature[i],resultados$feature[j])," ","_")
      valor=dataset3[,resultados$feature[i]]+dataset3[,resultados$feature[j]]
      nombre=gsub(" ","",nombre)
      
      auxdf = data.frame(valor)
      colnames(auxdf)=nombre
      
      datasetFinal=cbind(datasetFinal,auxdf)
      
      auxdf=NULL
      
    }
  }
  
  ### Operacion resta
  
  varName='resta'
  auxdf=NULL
  
  
  
  for (i in 1:length(resultados$feature))
  {
    
    for(j in i:length(resultados$feature))
    {
      
      if(resultados$feature[i]==resultados$feature[j])
        next
      
      nombre=str_replace_all(paste(varName,resultados$feature[i],resultados$feature[j])," ","_")
      valor=dataset3[,resultados$feature[i]]-dataset3[,resultados$feature[j]]
      nombre=gsub(" ","",nombre)
      
      auxdf = data.frame(valor)
      colnames(auxdf)=nombre
      
      datasetFinal=cbind(datasetFinal,auxdf)
      
      auxdf=NULL
      
    }
    
  }
  
  
  return(datasetFinal)
  
  
}

###################FIN DE MIS FUNCIONES ################################################


#aqui comienza el programa


#Establezco el Working Directory
setwd( "C:/Users/Martin/Desktop/MineriaDeDatos/datasets")


#cargo el dataset donde voy a entrenar

require("stringr")
dataset2  <- data.frame(fread("paquete_premium_202011.csv", stringsAsFactors= TRUE))



#### Me creo un data frame con los valores que quiero analizar ########

variablesTarget=(read.table("C:/Users/Martin/Desktop/MineriaDeDatos/datasets/variablesTarget.txt"))
colnames(variablesTarget)="feature"
  

### Creo la combinacion de variables ####
dataset1Mod=fuerzaBruta( dataset2 , variablesTarget )

### Limpio los datos y creo el archivo de salida #####
dataset=LimpiarDataset( data.table(dataset1Mod))

  
  
  