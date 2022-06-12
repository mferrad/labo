#limpio la memoria
rm( list=ls() )
gc()

library("data.table")
library("rpart")
library("rpart.plot")



setwd("~/buckets/b1/")   #Establezco el Working Directory

dataset  <- fread("./datasets/paquete_premium.csv.gz", stringsAsFactors= TRUE)


##### Variables de Negocio #######

### Tarjetas ####

dataset[ , neg_suma_salidas_tarjeta       := Master_mconsumospesos+Master_mconsumosdolares+Master_madelantopesos+Master_madelantodolares+Visa_mconsumospesos+Visa_mconsumosdolares+Visa_madelantopesos+Visa_madelantodolares ]
dataset[ , neg_max_salidas_tarjeta       := pmax( Master_mconsumospesos,Master_mconsumosdolares,Master_madelantopesos,Master_madelantodolares,Visa_mconsumospesos,Visa_mconsumosdolares,Visa_madelantopesos,Visa_madelantodolares, na.rm = TRUE) ]
dataset[ , neg_min_salidas_tarjeta       := pmin( Master_mconsumospesos,Master_mconsumosdolares,Master_madelantopesos,Master_madelantodolares,Visa_mconsumospesos,Visa_mconsumosdolares,Visa_madelantopesos,Visa_madelantodolares, na.rm = TRUE) ]

### Transacciones ####

dataset[ , neg_suma_Transacciones_Recividas       :=  mtransferencias_recibidas+mcheques_depositados+mcheques_depositados-mcheques_depositados_rechazados]
dataset[ , neg_max_Transacciones_Recividas       := pmax( mtransferencias_recibidas,(mcheques_depositados-mcheques_depositados_rechazados), na.rm = TRUE) ]
dataset[ , neg_min_Transacciones_Recividas       := pmin( mtransferencias_recibidas,(mcheques_depositados-mcheques_depositados_rechazados), na.rm = TRUE) ]

dataset[ , neg_suma_Transacciones_Enviadas       :=  mtransferencias_emitidas+mextraccion_autoservicio+mcheques_emitidos+mautoservicio-mcheques_emitidos_rechazados+mpagomiscuentas+mpagodeservicios]
dataset[ , neg_max_Transacciones_Enviadas       := pmax( mtransferencias_emitidas,mextraccion_autoservicio,(mcheques_emitidos-mcheques_emitidos_rechazados),mautoservicio,mpagomiscuentas,mpagodeservicios, na.rm = TRUE) ]
dataset[ , neg_min_Transacciones_Enviadas       := pmin( mtransferencias_emitidas,mextraccion_autoservicio,(mcheques_emitidos-mcheques_emitidos_rechazados),mautoservicio,mpagomiscuentas,mpagodeservicios, na.rm = TRUE) ]


dataset[ , neg_suma_Transacciones_Indice  :=  neg_suma_Transacciones_Recividas/neg_suma_Transacciones_Enviadas]
dataset[ , neg_resta_Transacciones  :=  neg_suma_Transacciones_Recividas-neg_suma_Transacciones_Enviadas]
dataset[ , neg_suma_Transacciones_Indice_max  :=  neg_max_Transacciones_Recividas/neg_max_Transacciones_Enviadas]


#### Salidas y acreditaciones fijas ####

dataset[ , neg_suma_salidas_fijas       := mcuenta_debitos_automaticos+mtarjeta_visa_debitos_automaticos+mttarjeta_master_debitos_automaticos]
dataset[ , neg_max_salidas_fijas    := pmax(mcuenta_debitos_automaticos,mtarjeta_visa_debitos_automaticos,mttarjeta_master_debitos_automaticos , na.rm = TRUE)] 
dataset[ , neg_min_salidas_fijas    := pmin(mcuenta_debitos_automaticos,mtarjeta_visa_debitos_automaticos,mttarjeta_master_debitos_automaticos, na.rm = TRUE)]

dataset[ , neg_suma_entradas_fijas       := mpayroll+mpayroll2]
dataset[ , neg_max_entradas_fijas    := pmax(mpayroll,mpayroll2 , na.rm = TRUE)] 
dataset[ , neg_min_entradas_fijas    := pmin(mpayroll,mpayroll2, na.rm = TRUE)]


dataset[ , neg_Indice_salidas_Entradas_fijas := neg_suma_entradas_fijas/neg_suma_salidas_fijas]
dataset[ , neg_Indice_salidas_Entradas_fijas_max := neg_max_entradas_fijas/neg_max_salidas_fijas]
dataset[ , neg_Indice_salidas_Entradas_fijas_min := neg_min_entradas_fijas/neg_min_salidas_fijas]
dataset[ , neg_resta_salidas_Entradas_fijas := neg_suma_entradas_fijas-neg_suma_salidas_fijas]

#### Inversiones y Deuda #########


dataset[ , neg_suma_inversion       := mplazo_fijo_dolares+mplazo_fijo_pesos+minversion1_pesos+minversion1_dolares]
dataset[ , neg_max_inversion     := pmax( mplazo_fijo_dolares,mplazo_fijo_pesos,minversion1_pesos,minversion1_dolares, na.rm = TRUE) ]  
dataset[ , neg_min_inversion     := pmin( mplazo_fijo_dolares,mplazo_fijo_pesos,minversion1_pesos,minversion1_dolares, na.rm = TRUE) ]

dataset[ , neg_suma_dueda       := mprestamos_personales+mprestamos_prendarios+mprestamos_hipotecarios]
dataset[ , neg_max_deuda     := pmax( mprestamos_personales,mprestamos_prendarios,mprestamos_hipotecarios, na.rm = TRUE) ]  
dataset[ , neg_min_deuda     := pmin( mprestamos_personales,mprestamos_prendarios,mprestamos_hipotecarios, na.rm = TRUE) ]

dataset[ , neg_Indice_duedaInversion       := neg_suma_inversion/neg_suma_dueda]
dataset[ , neg_resta_duedaInversion       := neg_suma_inversion-neg_suma_dueda]
dataset[ , neg_Indice_duedaInversion_max       := neg_max_inversion/neg_max_dueda]
dataset[ , neg_Indice_duedaInversion_min       := neg_min_inversion/neg_min_dueda]

#### Activos ####

dataset[ , neg_suma_Activos       := mcuenta_corriente_adicional+mcaja_ahorro+mcaja_ahorro_dolares]
dataset[ , neg_max_Activos     := pmax( mcuenta_corriente_adicional,mcaja_ahorro,mcaja_ahorro_dolares, na.rm = TRUE) ]  
dataset[ , neg_min_Activos     := pmin( mcuenta_corriente_adicional,mcaja_ahorro,mcaja_ahorro_dolares, na.rm = TRUE) ]  


#### Otros indicies y mas variables ######
dataset[ , neg_Indices_Activos_Deuda       := neg_suma_Activos/neg_suma_dueda]
dataset[ , neg_resta_Activos_Deuda       := neg_suma_Activos-neg_suma_dueda]
dataset[ , neg_Indice_Activos_Inversion       := neg_suma_Activos/neg_suma_inversion]
dataset[ , neg_Indices_Cliente_Edad_Antigueadad := cliente_edad/cliente_antiguedad]

dataset[, neg_ingresos_totales := neg_suma_Transacciones_Recividas+neg_suma_entradas_fijas]
dataset[, neg_salidas_totales := neg_suma_salidas_tarjeta+neg_suma_Transacciones_Enviadas+neg_suma_salidas_fijas]

dataset[,neg_resta_Ingresos_Salidas_totales := neg_ingresos_totales-neg_salidas_totales]
dataset[,neg_Indice_Ingresos_Salidas_totales := neg_ingresos_totales/neg_salidas_totales]

dataset[,neg_Indice_Patrimonio_Ingresos := neg_resta_Activos_Deuda/neg_resta_Ingresos_Salidas_totales]

dataset[,neg_Indice_Activos_Ingresos := neg_ingresos_totales/neg_suma_Activos]
dataset[,neg_Indice_deuda_Ingresos := neg_ingresos_totales/neg_suma_dueda]
dataset[,neg_Indice_Activos_salidas := neg_salidas_totales/neg_suma_Activos]
dataset[,neg_Indice_deuda_salidas := neg_salidas_totales/neg_suma_dueda]

