#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")
require("rpart.plot")

#setwd("." )  #establezco la carpeta donde voy a trabajar
#cargo el dataset
dataset <- fread("~/dmeyf23/dmeyf2023/datasets/competencia_01.csv")

#creo las variables producto
dataset[, variable_producto1 := ctrx_quarter * Visa_status]
dataset[, variable_producto2 := ctrx_quarter * cpayroll_trx]
dataset[, variable_producto3 := ctrx_quarter * mcuentas_saldo]
dataset[, variable_producto4 := ctrx_quarter * mprestamos_personales]
dataset[, variable_producto5 := ctrx_quarter * cdescubierto_preacordado]

dataset[, variable_producto6 := Visa_status * cpayroll_trx]
dataset[, variable_producto7 := Visa_status * mcuentas_saldo]
dataset[, variable_producto8 := Visa_status * mprestamos_personales]
dataset[, variable_producto9 := Visa_status * cdescubierto_preacordado]

dataset[, variable_producto10 := cpayroll_trx * mcuentas_saldo]
dataset[, variable_producto11 := cpayroll_trx * mprestamos_personales]
dataset[, variable_producto12 := cpayroll_trx * cdescubierto_preacordado]

dataset[, variable_producto13 := mcuentas_saldo * mprestamos_personales]
dataset[, variable_producto14 := mcuentas_saldo * cdescubierto_preacordado]

dataset[, variable_producto15 := mprestamos_personales * cdescubierto_preacordado]


#uso esta semilla para los canaritos
set.seed(119831)

#agrego una variable canarito, random distribucion uniforme en el intervalo [0,1]
dataset[ ,  canarito1 :=  runif( nrow(dataset) ) ]

#agrego los siguientes canaritos
for( i in 13:100 ) dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]

#creo la clase binaria2
dataset[, clase_nueva := ifelse( clase_ternaria %in% c( "BAJA+1","BAJA+2"), "POS", "NEG" ) ]


#Primero  veo como quedan mis arboles
modelo  <- rpart(formula= "clase_nueva ~ . -clase_ternaria",
                 data= dataset[ foto_mes==202103 ,],
                 model= TRUE,
                 xval= 0,
                 cp= 0,
                 minsplit= 10,
                 maxdepth= 10)


pdf(file = "./arbol_canaritos_kaggle.pdf", width=40, height=4)  
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, cex=0.15)
dev.off()

