# LightGBM  cambiando algunos de los parametros

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

p=1/60

require("data.table")
require("lightgbm")

#Aqui se debe poner la carpeta de la computadora local
#setwd("D:\\gdrive\\ITBA2022A\\")   #Establezco el Working Directory
setwd("C:\\Users\\Martin\\Desktop\\MineriaDeDatos\\")  



#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/paquete_premium_202011.csv", stringsAsFactors= TRUE)


#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 := ifelse( clase_ternaria=="BAJA+2", 1L, 0L) ]

dataset[ , ctarjeta_visa_trx2 := ctarjeta_visa_trx ]
dataset[ ctarjeta_visa==0 & ctarjeta_visa_trx==0,  ctarjeta_visa_trx2 := NA ]

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )


#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01 )


#nota 
#Agregue el learning rate que no estaba
#Agrege la prob de corte que tampoco estaba
#Cambie la semilla por la mia
#agrego el max bin tambien 

# Este es el motivacional

#modelo  <- lgb.train( data= dtrain,
#                      param= list( objective=        "binary",
#                                   max_bin=              31,
#                                   learning_rate=         0.0300696989,
#                                   num_iterations=      567,
#                                   num_leaves=         1002,
#                                   min_data_in_leaf=   6263,
#                                   feature_fraction=      0.9100319271,
#                                   seed=             102191
#                      )
#)

# Este es el default (me faltan parametros comparados con el modelo de la bayesiana ignorar)
#
#modelo  <- lgb.train( data= dtrain,
#                      param= list( objective=        "binary",
#                                   learning_rate=         0.1,
#                                   min_data_in_leaf=   20,
#                                   feature_fraction=      1.0,
#                                   seed=             102191,
#                                   num_leaves =        31,
#                                   prob_corte =       1/60,
                                  
#                                   max_bin=            31
#                      )
#)
#p=1/60


#Este es el modelo de la bayesiana
#modelo  <- lgb.train( data= dtrain,
#                      param= list( objective=        "binary",
#                                   max_bin=            31,
#                                   num_iterations=     533,
#                                   num_leaves=         1315,
#                                   feature_fraction=    0.509636738987337,
#                                   min_data_in_leaf= 3934,
#                                   learning_rate=0.0101636464496809,
#                                   seed= 999983 )
#                    )
#p=0.0169832543626759


#Este es el modelo de la bayesiana con lamdas y demas parametros
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=        "binary",
                                   max_bin=            31,
                                   num_iterations=     234,
                                   num_leaves=         996,
                                   feature_fraction=    0.440686173080777,
                                   min_data_in_leaf= 1334,
                                   learning_rate=0.0438942719169011,
                                   
                                   min_gain_to_split=0.188008489892813,
                                   max_depth=7,
                                   lambda_l2=73.713755483181,
                                   lambda_l1=0.59365566296864,
                                   seed= 200443 )
)
p=0.0145648611386611



#aplico el modelo a los datos sin clase
dapply  <- fread("./datasets/paquete_premium_202101.csv")

dapply[ , ctarjeta_visa_trx2 := ctarjeta_visa_trx ]
dapply[ ctarjeta_visa==0 & ctarjeta_visa_trx==0,  ctarjeta_visa_trx2 := NA ]


#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ]) )


#Genero la entrega para Kaggle
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= as.integer(prediccion > p ) )  ) #genero la salida

dir.create( "./labo/exp/",  showWarnings = FALSE ) 
dir.create( "./labo/exp/KA2512/", showWarnings = FALSE )
archivo_salida  <- "./labo/exp/KA2512/KA_512_001.csv"

#genero el archivo para Kaggle
fwrite( entrega, 
        file= archivo_salida, 
        sep= "," )


#ahora imprimo la importancia de variables
tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
archivo_importancia  <- "./labo/exp/KA2512/512_importancia_001.txt"

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )

