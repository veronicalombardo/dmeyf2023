# Script del modelo elegido en Kaggle
#Public Leaderboard 58.4962

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection


# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

PARAM <- list()

PARAM$experimento <- "KA4000-01"

# Elijo binaria2
PARAM$clase <- "binaria2"

# Elijo hacer oversampling
PARAM$oversampling <- 100

PARAM$prob_corte <-  -1 #dejo en -1 porque voy a usar corte

# por resultados de pruebas anteriores y en base a la meseta planteada por
#Lautaro decido como punto de corte 9000
PARAM$corte <- 9000

PARAM$rpart$cp <- -1
PARAM$rpart$minsplit <- 900
PARAM$rpart$minbucket <- 215
PARAM$rpart$maxdepth <- 9

#------------------------------------------------------------------------------
#Aqui comienza el programa

#No establezco Working Directory porque estoy trabajando en un proyecto
#setwd("~/buckets/b1/") 


if( PARAM$prob_corte <= 0 & PARAM$corte <= 0 ) stop("debe elegir al menos una opcion de corte" )
if( PARAM$prob_corte > 0 & PARAM$corte > 0 ) stop("No puede elegir dos opcinoes de corte al mismo tiempo" )
if( PARAM$clase == "binaria2" & PARAM$corte <= 0 ) stop( "en el caso de clase binaria2  debe cortar por envios, PARAM$corte" )

# cargo el dataset
dataset <- fread("~/dmeyf23/dmeyf2023/datasets/competencia_01.csv")

# creo la carpeta donde va el experimento
#  HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create( paste0("./exp/", PARAM$experimento, "/"),
            showWarnings = FALSE)


###Hago un árbol de max depth 3 para crear una variable nueva:
#Lo dejo comentado para que no corra en este proceso

'''
dtrain <- dataset[foto_mes == 202103] # defino donde voy a entrenar

dapply <- dataset[foto_mes == 202105] # defino donde voy a aplicar el modelo
dapply[, clase_ternaria := NA ]

modelo <- rpart(
  formula = "clase_ternaria ~ .",
  data = dtrain, 
  xval = 0,
  cp = -1, 
  maxdepth = 3
)

# grafico el arbol
prp(modelo,
    extra = 101, digits = -5,
    branch = 1, type = 4, varlen = 0, faclen = 0
)
'''

### Creo una variable nueva por inspección de un arbol de maxdepth 3
#dataset[, variable_nueva1 := ifelse(ctrx_quarter >= 48 & Visa_status < 8, 1, 0)]

##como esa variable nueva no funcionó porque me da peor que sin incluirla
##selecciono algunas variables que salen del arbol canaritos y de un
##árbol de max depth 4, y realizo el procuto de ellas:

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

# No lo establezco porque estoy trabajando con un proyecto
#setwd( paste0("./exp/", PARAM$experimento, "/") )

switch(PARAM$clase,
       "ternaria"  = dataset[, clase_nueva := ifelse( clase_ternaria=="BAJA+2", "POS", clase_ternaria ) ],
       "binaria1"  = dataset[, clase_nueva := ifelse( clase_ternaria=="BAJA+2", "POS", "NEG" ) ],
       "binaria2"  = dataset[, clase_nueva := ifelse( clase_ternaria %in% c( "BAJA+1","BAJA+2"), "POS", "NEG" ) ],
       stop(" PARAM$clase debe tener un valor valido")
)

#Separo en train y test
dtrain <- dataset[foto_mes == 202103] # defino donde voy a entrenar

dapply <- dataset[foto_mes == 202105] # defino donde voy a aplicar el modelo
dapply[, clase_ternaria := NA ]

# hago el oversampling
vector_pesos <- rep( 1.0, nrow(dtrain) )
if( PARAM$oversampling > 0 )   
  vector_pesos <- dtrain[, ifelse( clase_nueva=="POS", PARAM$oversampling, 1) ]

# genero el modelo,  aqui se construye el arbol
modelo <- rpart(
  formula = "clase_nueva ~ . -clase_ternaria",
  data = dtrain, # los datos donde voy a entrenar
  xval = 0,
  control = PARAM$rpart,
  weights =  vector_pesos
)

# aplico el modelo a los datos nuevos
prediccion <- predict(
  object = modelo,
  newdata = dapply,
  type = "prob"
)

# calculo en tablita lo necesario para generar el archivo prediccion
tablita <- dapply[, list(numero_de_cliente) ]
tablita[, prob := prediccion[, "POS"] ]
setorder( tablita, -prob )
tablita[, Predicted := 0L ]

if( PARAM$prob_corte > 0 ) {
  tablita[, Predicted := as.numeric(prob > PARAM$prob_corte)]
} else {
  if( PARAM$corte > 0 ) {
    tablita[ 1:PARAM$corte, Predicted := 1]
  }
}


# genero el submit para Kaggle
nom_archivo <- paste0( PARAM$experimento, "_kaggle_ternaria_con_var_producto_4.csv" )

fwrite(tablita[, list(numero_de_cliente, Predicted)],
       file = paste("./exp/KA4000-01/",nom_archivo,sep=""),
       sep = ","
)

#luego corrí el script de canaritos para ver en que lugar quedarían esos
#canaritos con las variables creadas
