
rm( list=ls() )  #remove all objects

require("data.table")

#calcula cuantos encestes logra un jugador con indice de enceste prob que hace qyt tiros libres
ftirar  <- function( prob, qty )
{
  return( sum( runif(qty) < prob ) )
}



# Estrategia: En cada etapa hago tirar a los competidors 70 tiros libres (a excepcion de la ultima que son 160), y en cada iteracion computo la probabilidad medida 
# de ensestar teniendo en cuenta TODOS los tiros ejecutados
# en todas las etapas, en cada iteracion elimino la mitad con probabilidad mas baja, a la larga el bueno queda estadisticamente primero

ksemillas  <- c(111222,222223,6785421,135791,123456,789101,456799,200443, 200461, 200467, 200569, 200587) 
#ksemillas=c(200443)

                    
for(semilla in ksemillas)
{
  
  #defino los jugadores
  mejor      <-  0.7
  peloton    <-  ( 501:599 ) / 1000
  jugadores  <-  c( peloton,mejor ) #intencionalmente el mejor esta al final
  
  NumeroMejor=100 #dorsal del mejor jugador
  
  numeros=(1:100)
  contadorTiros=0
  jugadores=rbind(numeros,jugadores)
  
  #AciertosEtapa1=0
  #AciertosEtapa2=0
  #AciertosEtapa3=0
  #AciertosEtapa4=0
  #AciertosEtapa5=0
  AciertosEtapa6=0
  
  iteraciones=10000
  
  
  
  set.seed(semilla)
  
  for (i in 1:iteraciones)
  {
      n1=70
      Eliminar=50
      vaciertos1  <- mapply( ftirar, jugadores[2,], n1 )
      contadorTiros=n1*length(jugadores[2,])
      planilla=data.frame("dorsal"= jugadores[1,],"probReal"=jugadores[2,],"aciertos"=vaciertos1,"tirosTotales"=n1,"probabilidadMedida"=vaciertos1/n1) #creo una planilla que voy actualizando
      planilla=planilla[order(planilla$probabilidadMedida),] #Ordena la planilla segun los aciertos medidos (mejor al final)
      planilla=planilla[-(1:Eliminar),] #Elimino la mitad
      
      #if(NumeroMejor %in% planilla$dorsal) #Actualizo la probabilidad de no eliminar al mejor en esta etapa (solo para entender, no es relevante)
        #AciertosEtapa1=AciertosEtapa1+1
      
      #print(planilla)
      
      n2=70
      Eliminar2=25
      vaciertos2  <- mapply( ftirar, planilla$probReal, n2 )
      contadorTiros=contadorTiros + n2*length(planilla$probReal)
      planilla$aciertos=planilla$aciertos+vaciertos2 #actualizo aciertos
      planilla$tirosTotales=planilla$tirosTotales+n2 #actualizo tiros totales
      planilla$probabilidadMedida=planilla$aciertos/planilla$tirosTotales #actualizo probabilidades medidas
      planilla=planilla[order(planilla$probabilidadMedida),] #Ordena la planilla segun los aciertos medidos (mejor al final)
      planilla=planilla[-(1:Eliminar2),] #Elimino la mitad
      
      #if(NumeroMejor %in% planilla$dorsal) #Actualizo la probabilidad de no eliminar al mejor en esta etapa (solo para entender, no es relevante)
        #AciertosEtapa2=AciertosEtapa2+1
      
      #print(planilla)
      
      n3=70
      Eliminar3=13
      vaciertos3  <- mapply( ftirar, planilla$probReal, n3 )
      contadorTiros=contadorTiros + n3*length(planilla$probReal)
      planilla$aciertos=planilla$aciertos+vaciertos3 #actualizo aciertos
      planilla$tirosTotales=planilla$tirosTotales+n3 #actualizo tiros totales
      planilla$probabilidadMedida=planilla$aciertos/planilla$tirosTotales #actualizo probabilidades medidas
      planilla=planilla[order(planilla$probabilidadMedida),] #Ordena la planilla segun los aciertos medidos (mejor al final)
      planilla=planilla[-(1:Eliminar3),] #Elimino la mitad
      
      #if(NumeroMejor %in% planilla$dorsal) #Actualizo la probabilidad de no eliminar al mejor en esta etapa (solo para entender, no es relevante)
        #AciertosEtapa3=AciertosEtapa3+1
      
      #print(planilla)
      
      n4=70
      Eliminar4=6
      vaciertos4  <- mapply( ftirar, planilla$probReal, n4 )
      contadorTiros=contadorTiros + n4*length(planilla$probReal)
      planilla$aciertos=planilla$aciertos+vaciertos4 #actualizo aciertos
      planilla$tirosTotales=planilla$tirosTotales+n4 #actualizo tiros totales
      planilla$probabilidadMedida=planilla$aciertos/planilla$tirosTotales #actualizo probabilidades medidas
      planilla=planilla[order(planilla$probabilidadMedida),] #Ordena la planilla segun los aciertos medidos (mejor al final)
      planilla=planilla[-(1:Eliminar4),] #Elimino la mitad
      
      #if(NumeroMejor %in% planilla$dorsal) #Actualizo la probabilidad de no eliminar al mejor en esta etapa (solo para entender, no es relevante)
        #AciertosEtapa4=AciertosEtapa4+1
      
      #print(planilla)
      
      n5=70
      Eliminar5=3
      vaciertos5  <- mapply( ftirar, planilla$probReal, n5 ) 
      contadorTiros=contadorTiros + n5*length(planilla$probReal)
      planilla$aciertos=planilla$aciertos+vaciertos5#actualizo aciertos
      planilla$tirosTotales=planilla$tirosTotales+n5 #actualizo tiros totales
      planilla$probabilidadMedida=planilla$aciertos/planilla$tirosTotales #actualizo probabilidades medidas
      planilla=planilla[order(planilla$probabilidadMedida),] #Ordena la planilla segun los aciertos medidos (mejor al final)
      planilla=planilla[-(1:Eliminar5),] #Elimino la mitad
      
      #if(NumeroMejor %in% planilla$dorsal) #Actualizo la probabilidad de no eliminar al mejor en esta etapa (solo para entender, no es relevante)
        #AciertosEtapa5=AciertosEtapa5+1
      
      #print(planilla)
      
      n6=160
      Eliminar6=2
      vaciertos6  <- mapply( ftirar, planilla$probReal, n6 )
      contadorTiros=contadorTiros + n6*length(planilla$probReal)
      planilla$aciertos=planilla$aciertos+vaciertos6#actualizo aciertos
      planilla$tirosTotales=planilla$tirosTotales+n6 #actualizo tiros totales
      planilla$probabilidadMedida=planilla$aciertos/planilla$tirosTotales #actualizo probabilidades medidas
      planilla=planilla[order(planilla$probabilidadMedida),] #Ordena la planilla segun los aciertos medidos (mejor al final)
      planilla=planilla[-(1:Eliminar6),] #Elimino la mitad
    
      #print(planilla)
      
      if(NumeroMejor %in% planilla$dorsal) #Actualizo la probabilidad de seleccionar al mejor en esta etapa 
        AciertosEtapa6=AciertosEtapa6+1
  
  }
  
  
  #Petapa1=AciertosEtapa1/iteraciones
  #Petapa2=AciertosEtapa2/iteraciones
  #Petapa3=AciertosEtapa3/iteraciones
  #Petapa4=AciertosEtapa4/iteraciones
  #Petapa5=AciertosEtapa5/iteraciones
  Petapa6=AciertosEtapa6/iteraciones
  
  cat('semilla: ',semilla,' probabilidad: ', Petapa6, ' tiros: ',contadorTiros, '\n') #imprimo la semilla, la probabilidad de encontrar el mejor y la cantidad de tiros

}

