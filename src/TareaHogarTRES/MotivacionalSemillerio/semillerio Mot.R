#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection


require("lightgbm")


#Aqui se debe poner la carpeta de la computadora local
setwd("C:/Users/Martin/Desktop/MineriaDeDatos/labo/src/TareaHogarTRES/MotivacionalSemillerio")   #Establezco el Working Directory

Myfile=paste0( "exp_MOT6520_MOT_652_", 1, ".csv" )
resultado  <- data.frame(fread(Myfile))

setorder( resultado, numero_de_cliente)






for (i in 2:50)
{

  Myfile=paste0( "exp_MOT6520_MOT_652_", i, ".csv" )
  expi  <- data.frame(fread(Myfile))
  setorder( expi, numero_de_cliente)
  resultado$Predicted=resultado$Predicted+expi$Predicted
  setorder( resultado, numero_de_cliente)
  print(resultado)
}


  fwrite( resultado, 
          file= "Resutado_Hib.csv" ,
          sep= "," )
