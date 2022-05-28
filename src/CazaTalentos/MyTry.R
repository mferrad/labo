#intencionalmente el mejor jugador va al final de la lista de jugadores
#porque la funcion which.max() de R hace trampa
#si hay un empate ( dos máximos) se queda con el que esta primero en el vector

rm( list=ls() )  #remove all objects

require("data.table")



set.seed( 102191 )

#calcula cuantos encestes logra un jugador con indice de enceste prob que hace qyt tiros libres
ftirar  <- function( prob, qty )
{
  return( sum( runif(qty) < prob ) )
}

#defino los jugadores
mejor      <-  0.7
peloton    <-  ( 501:599 ) / 1000
jugadores  <-  c( peloton, mejor ) #intencionalmente el mejor esta al final

#veo que tiene el vector
jugadores

#hago que los 100 jugadores tiren 10 veces cada uno
mapply(  ftirar, jugadores, 10 )

#resultados=c('competidores','tiros_libres','posicion','probabilidad')

resultados=c(0,0,0,0)

tries=1000

for(competidores in c (100,50,25,12,6,3))
#for(competidores in c (5,10,15,20))
{

  for(  tiros_libres  in c(70,140,210,280,350,420,490) )
  #for(  tiros_libres  in c(10,20,30,40,50,60 ) )
  {

    #for(posicion in c(10,20,30,40))
    for(posicion in c(50,25,12,6,3,1))
    {
          if(posicion>=competidores)
            next
      

         
          primero_entre_ganadores  <- 0
          
          for( i in 1:tries)  #diez mil experimentos
          {
            vaciertos  <- mapply( ftirar, jugadores[(100+1-competidores):100], tiros_libres )
            #mejor  <- which.max( vaciertos )
            
            MejoresQueElmejor=sum(vaciertos>=vaciertos[length(vaciertos)])
 
            if( MejoresQueElmejor <= posicion )  primero_entre_ganadores  <- primero_entre_ganadores + 1
          }
    
          cat(competidores, tiros_libres, posicion ,primero_entre_ganadores/tries, "\n" )
          resultados=rbind(resultados,c(competidores, tiros_libres, posicion ,primero_entre_ganadores/tries))
          
    }
    
  } 
}

c('competidores','tiros_libres','posicion','probabilidad')
resultados[resultados[,4]>0.99,]





numeros
jugadores


n1=70






  