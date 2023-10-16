# se entrena con clase_binaria2  POS =  { BAJA+1, BAJA+2 }
# Optimizacion Bayesiana de hiperparametros de  lightgbm,

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rlist")

require("lightgbm")

# paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

# para que se detenga ante el primer error
# y muestre el stack de funciones invocadas
options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})



# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()

PARAM$experimento <- "HT8230_8oct"

PARAM$input$dataset <- "./datasets/competencia_02.csv.gz"

# los meses en los que vamos a entrenar
#  mucha magia emerger de esta eleccion
PARAM$input$testing <- c(202105)
PARAM$input$validation <- c(202104)
PARAM$input$training <- c(201904, 201905,201906,201907,
                          201908,201909,201910,201911,201912, 202001, 202002, 202003,
                          202004, 202005,202006,202007,
                          202008,202009,202010,202011,202012,
                          202101, 202102, 202103)

# un undersampling de 0.1  toma solo el 10% de los CONTINUA
PARAM$trainingstrategy$undersampling <- 0.1
PARAM$trainingstrategy$semilla_azar <- 119831

PARAM$hyperparametertuning$POS_ganancia <- 273000
PARAM$hyperparametertuning$NEG_ganancia <- -7000

# Aqui poner su segunda semilla
PARAM$lgb_semilla <- 119831
#------------------------------------------------------------------------------

# Hiperparametros FIJOS de  lightgbm
PARAM$lgb_basicos <- list(
  boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest
  objective = "binary",
  metric = "custom",
  first_metric_only = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  force_row_wise = TRUE, # para reducir warnings
  verbosity = -100,
  max_depth = -1L, # -1 significa no limitar,  por ahora lo dejo fijo
  min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
  min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
  lambda_l1 = 0.0, # lambda_l1 >= 0.0
  lambda_l2 = 0.0, # lambda_l2 >= 0.0
  max_bin = 31L, # lo debo dejar fijo, no participa de la BO
  num_iterations = 9999, # un numero muy grande, lo limita early_stopping_rounds
  
  bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
  pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
  neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
  is_unbalance = FALSE, #
  scale_pos_weight = 1.0, # scale_pos_weight > 0.0
  
  drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop = 50, # <=0 means no limit
  skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0
  
  extra_trees = TRUE, # Magic Sauce
  
  seed = PARAM$lgb_semilla
)


# Aqui se cargan los hiperparametros que se optimizan
#  en la Bayesian Optimization
PARAM$bo_lgb <- makeParamSet(
  makeNumericParam("learning_rate", lower = 0.02, upper = 0.3),
  makeNumericParam("feature_fraction", lower = 0.01, upper = 1.0),
  makeIntegerParam("num_leaves", lower = 8L, upper = 1024L),
  makeIntegerParam("min_data_in_leaf", lower = 100L, upper = 50000L)
)

# si usted es ambicioso, y tiene paciencia, podria subir este valor a 100
PARAM$bo_iteraciones <- 50 # iteraciones de la Optimizacion Bayesiana

#------------------------------------------------------------------------------
# graba a un archivo los componentes de lista
# para el primer registro, escribe antes los titulos

loguear <- function(
  reg, arch = NA, folder = "./exp/",
  ext = ".txt", verbose = TRUE) {
  archivo <- arch
  if (is.na(arch)) archivo <- paste0(folder, substitute(reg), ext)
  
  if (!file.exists(archivo)) # Escribo los titulos
  {
    linea <- paste0(
      "fecha\t",
      paste(list.names(reg), collapse = "\t"), "\n"
    )
    
    cat(linea, file = archivo)
  }
  
  linea <- paste0(
    format(Sys.time(), "%Y%m%d %H%M%S"), "\t", # la fecha y hora
    gsub(", ", "\t", toString(reg)), "\n"
  )
  
  cat(linea, file = archivo, append = TRUE) # grabo al archivo
  
  if (verbose) cat(linea) # imprimo por pantalla
}
#------------------------------------------------------------------------------
GLOBAL_arbol <- 0L
GLOBAL_gan_max <- -Inf
vcant_optima <- c()

fganancia_lgbm_meseta <- function(probs, datos) {
  vlabels <- get_field(datos, "label")
  vpesos <- get_field(datos, "weight")
  
  
  GLOBAL_arbol <<- GLOBAL_arbol + 1
  tbl <- as.data.table(list(
    "prob" = probs,
    "gan" = ifelse(vlabels == 1 & vpesos > 1,
                   PARAM$hyperparametertuning$POS_ganancia,
                   PARAM$hyperparametertuning$NEG_ganancia  )
  ))
  
  setorder(tbl, -prob)
  tbl[, posicion := .I]
  tbl[, gan_acum := cumsum(gan)]
  
  tbl[, gan_suavizada :=
        frollmean(
          x = gan_acum, n = 2001, align = "center",
          na.rm = TRUE, hasNA = TRUE
        )]
  
  gan <- tbl[, max(gan_suavizada, na.rm = TRUE)]
  
  
  pos <- which.max(tbl[, gan_suavizada])
  vcant_optima <<- c(vcant_optima, pos)
  
  if (GLOBAL_arbol %% 10 == 0) {
    if (gan > GLOBAL_gan_max) GLOBAL_gan_max <<- gan
    
    cat("\r")
    cat(
      "Validate ", GLOBAL_iteracion, " ", " ",
      GLOBAL_arbol, "  ", gan, "   ", GLOBAL_gan_max, "   "
    )
  }
  
  
  return(list(
    "name" = "ganancia",
    "value" = gan,
    "higher_better" = TRUE
  ))
}
#------------------------------------------------------------------------------

EstimarGanancia_lightgbm <- function(x) {
  gc()
  GLOBAL_iteracion <<- GLOBAL_iteracion + 1L
  
  # hago la union de los parametros basicos y los moviles que vienen en x
  param_completo <- c(PARAM$lgb_basicos, x)
  
  param_completo$early_stopping_rounds <-
    as.integer(400 + 4 / param_completo$learning_rate)
  
  GLOBAL_arbol <<- 0L
  GLOBAL_gan_max <<- -Inf
  vcant_optima <<- c()
  set.seed(PARAM$lgb_semilla, kind = "L'Ecuyer-CMRG")
  modelo_train <- lgb.train(
    data = dtrain,
    valids = list(valid = dvalidate),
    eval = fganancia_lgbm_meseta,
    param = param_completo,
    verbose = -100
  )
  
  cat("\n")
  
  cant_corte <- vcant_optima[modelo_train$best_iter]
  
  # aplico el modelo a testing y calculo la ganancia
  prediccion <- predict(
    modelo_train,
    data.matrix(dataset_test[, campos_buenos, with = FALSE])
  )
  
  tbl <- copy(dataset_test[, list("gan" = ifelse(clase_ternaria == "BAJA+2",
                                                 PARAM$hyperparametertuning$POS_ganancia, 
                                                 PARAM$hyperparametertuning$NEG_ganancia))])
  
  tbl[, prob := prediccion]
  setorder(tbl, -prob)
  tbl[, gan_acum := cumsum(gan)]
  tbl[, gan_suavizada := frollmean(
    x = gan_acum, n = 2001,
    align = "center", na.rm = TRUE, hasNA = TRUE
  )]
  
  
  ganancia_test <- tbl[, max(gan_suavizada, na.rm = TRUE)]
  
  cantidad_test_normalizada <- which.max(tbl[, gan_suavizada])
  
  rm(tbl)
  gc()
  
  ganancia_test_normalizada <- ganancia_test
  
  
  # voy grabando las mejores column importance
  if (ganancia_test_normalizada > GLOBAL_gananciamax) {
    GLOBAL_gananciamax <<- ganancia_test_normalizada
    tb_importancia <- as.data.table(lgb.importance(modelo_train))
    
    fwrite(tb_importancia,
           file = paste0("impo_", sprintf("%03d", GLOBAL_iteracion), ".txt"),
           sep = "\t"
    )
    
    rm(tb_importancia)
  }
  
  
  # logueo final
  ds <- list("cols" = ncol(dtrain), "rows" = nrow(dtrain))
  xx <- c(ds, copy(param_completo))
  
  xx$early_stopping_rounds <- NULL
  xx$num_iterations <- modelo_train$best_iter
  xx$estimulos <- cantidad_test_normalizada
  xx$ganancia <- ganancia_test_normalizada
  xx$iteracion_bayesiana <- GLOBAL_iteracion
  
  loguear(xx, arch = "BO_log.txt")
  
  set.seed(PARAM$lgb_semilla, kind = "L'Ecuyer-CMRG")
  return(ganancia_test_normalizada)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

# Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/") # Establezco el Working Directory

# cargo el dataset donde voy a entrenar el modelo
dataset <- fread(PARAM$input$dataset)

# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

# en estos archivos quedan los resultados
kbayesiana <- paste0(PARAM$experimento, ".RDATA")
klog <- paste0(PARAM$experimento, ".txt")

#------------------------------------------------------------------------------
#hago las modificaciones al dataset:

#Cambio todas las variables que en un foto_mes tiene ratio_zeros igual a 1 por NA
dataset[foto_mes == 201904 & ctarjeta_visa_debitos_automaticos == 0, ctarjeta_visa_debitos_automaticos := NA]
dataset[foto_mes == 201904 & mttarjeta_visa_debitos_automaticos == 0, mttarjeta_visa_debitos_automaticos := NA]
dataset[foto_mes == 201905 & ccomisiones_otras == 0, ccomisiones_otras := NA]
dataset[foto_mes == 201905 & mactivos_margen == 0, mactivos_margen := NA]
dataset[foto_mes == 201905 & mcomisiones == 0, mcomisiones := NA]
dataset[foto_mes == 201905 & mcomisiones_otras == 0, mcomisiones_otras := NA]
dataset[foto_mes == 201905 & mpasivos_margen == 0, mpasivos_margen := NA]
dataset[foto_mes == 201905 & mrentabilidad == 0, mrentabilidad := NA]
dataset[foto_mes == 201905 & mrentabilidad_annual == 0, mrentabilidad_annual := NA]
dataset[foto_mes == 201910 & ccajeros_propios_descuentos == 0, ccajeros_propios_descuentos := NA]
dataset[foto_mes == 201910 & ccomisiones_otras == 0, ccomisiones_otras := NA]
dataset[foto_mes == 201910 & chomebanking_transacciones == 0, chomebanking_transacciones := NA]
dataset[foto_mes == 201910 & ctarjeta_master_descuentos == 0, ctarjeta_master_descuentos := NA]
dataset[foto_mes == 201910 & ctarjeta_visa_descuentos == 0, ctarjeta_visa_descuentos := NA]
dataset[foto_mes == 201910 & mactivos_margen == 0, mactivos_margen := NA]
dataset[foto_mes == 201910 & mcajeros_propios_descuentos == 0, mcajeros_propios_descuentos := NA]
dataset[foto_mes == 201910 & mcomisiones == 0, mcomisiones := NA]
dataset[foto_mes == 201910 & mcomisiones_otras == 0, mcomisiones_otras := NA]
dataset[foto_mes == 201910 & mpasivos_margen == 0, mpasivos_margen := NA]
dataset[foto_mes == 201910 & mrentabilidad == 0, mrentabilidad := NA]
dataset[foto_mes == 201910 & mrentabilidad_annual == 0, mrentabilidad_annual := NA]
dataset[foto_mes == 201910 & mtarjeta_master_descuentos == 0, mtarjeta_master_descuentos := NA]
dataset[foto_mes == 201910 & mtarjeta_visa_descuentos == 0, mtarjeta_visa_descuentos := NA]
dataset[foto_mes == 202002 & ccajeros_propios_descuentos == 0, ccajeros_propios_descuentos := NA]
dataset[foto_mes == 202002 & ctarjeta_master_descuentos == 0, ctarjeta_master_descuentos := NA]
dataset[foto_mes == 202002 & ctarjeta_visa_descuentos == 0, ctarjeta_visa_descuentos := NA]
dataset[foto_mes == 202002 & mcajeros_propios_descuentos == 0, mcajeros_propios_descuentos := NA]
dataset[foto_mes == 202002 & mtarjeta_master_descuentos == 0, mtarjeta_master_descuentos := NA]
dataset[foto_mes == 202002 & mtarjeta_visa_descuentos == 0, mtarjeta_visa_descuentos := NA]
dataset[foto_mes == 202006 & active_quarter == 0, active_quarter := NA]
dataset[foto_mes == 202006 & catm_trx == 0, catm_trx := NA]
dataset[foto_mes == 202006 & catm_trx_other == 0, catm_trx_other := NA]
dataset[foto_mes == 202006 & ccajas_consultas == 0, ccajas_consultas := NA]
dataset[foto_mes == 202006 & ccajas_depositos == 0, ccajas_depositos := NA]
dataset[foto_mes == 202006 & ccajas_extracciones == 0, ccajas_extracciones := NA]
dataset[foto_mes == 202006 & ccajas_otras == 0, ccajas_otras := NA]
dataset[foto_mes == 202006 & ccajas_transacciones == 0, ccajas_transacciones := NA]
dataset[foto_mes == 202006 & ccajeros_propios_descuentos == 0, ccajeros_propios_descuentos := NA]
dataset[foto_mes == 202006 & ccallcenter_transacciones == 0, ccallcenter_transacciones := NA]
dataset[foto_mes == 202006 & ccheques_depositados == 0, ccheques_depositados := NA]
dataset[foto_mes == 202006 & ccheques_depositados_rechazados == 0, ccheques_depositados_rechazados := NA]
dataset[foto_mes == 202006 & ccheques_emitidos == 0, ccheques_emitidos := NA]
dataset[foto_mes == 202006 & ccheques_emitidos_rechazados == 0, ccheques_emitidos_rechazados := NA]
dataset[foto_mes == 202006 & ccomisiones_otras == 0, ccomisiones_otras := NA]
dataset[foto_mes == 202006 & cextraccion_autoservicio == 0, cextraccion_autoservicio := NA]
dataset[foto_mes == 202006 & chomebanking_transacciones == 0, chomebanking_transacciones := NA]
dataset[foto_mes == 202006 & cmobile_app_trx == 0, cmobile_app_trx := NA]
dataset[foto_mes == 202006 & ctarjeta_debito_transacciones == 0, ctarjeta_debito_transacciones := NA]
dataset[foto_mes == 202006 & ctarjeta_master_descuentos == 0, ctarjeta_master_descuentos := NA]
dataset[foto_mes == 202006 & ctarjeta_master_transacciones == 0, ctarjeta_master_transacciones := NA]
dataset[foto_mes == 202006 & ctarjeta_visa_descuentos == 0, ctarjeta_visa_descuentos := NA]
dataset[foto_mes == 202006 & ctarjeta_visa_transacciones == 0, ctarjeta_visa_transacciones := NA]
dataset[foto_mes == 202006 & internet == 0, internet := NA]
dataset[foto_mes == 202006 & mactivos_margen == 0, mactivos_margen := NA]
dataset[foto_mes == 202006 & matm == 0, matm := NA]
dataset[foto_mes == 202006 & matm_other == 0, matm_other := NA]
dataset[foto_mes == 202006 & mautoservicio == 0, mautoservicio := NA]
dataset[foto_mes == 202006 & mcajeros_propios_descuentos == 0, mcajeros_propios_descuentos := NA]
dataset[foto_mes == 202006 & mcheques_depositados == 0, mcheques_depositados := NA]
dataset[foto_mes == 202006 & mcheques_depositados_rechazados == 0, mcheques_depositados_rechazados := NA]
dataset[foto_mes == 202006 & mcheques_emitidos == 0, mcheques_emitidos := NA]
dataset[foto_mes == 202006 & mcheques_emitidos_rechazados == 0, mcheques_emitidos_rechazados := NA]
dataset[foto_mes == 202006 & mcomisiones == 0, mcomisiones := NA]
dataset[foto_mes == 202006 & mcomisiones_otras == 0, mcomisiones_otras := NA]
dataset[foto_mes == 202006 & mcuentas_saldo == 0, mcuentas_saldo := NA]
dataset[foto_mes == 202006 & mextraccion_autoservicio == 0, mextraccion_autoservicio := NA]
dataset[foto_mes == 202006 & mpasivos_margen == 0, mpasivos_margen := NA]
dataset[foto_mes == 202006 & mrentabilidad == 0, mrentabilidad := NA]
dataset[foto_mes == 202006 & mrentabilidad_annual == 0, mrentabilidad_annual := NA]
dataset[foto_mes == 202006 & mtarjeta_master_consumo == 0, mtarjeta_master_consumo := NA]
dataset[foto_mes == 202006 & mtarjeta_master_descuentos == 0, mtarjeta_master_descuentos := NA]
dataset[foto_mes == 202006 & mtarjeta_visa_consumo == 0, mtarjeta_visa_consumo := NA]
dataset[foto_mes == 202006 & mtarjeta_visa_descuentos == 0, mtarjeta_visa_descuentos := NA]
dataset[foto_mes == 202006 & tcallcenter == 0, tcallcenter := NA]
dataset[foto_mes == 202006 & thomebanking == 0, thomebanking := NA]
dataset[foto_mes == 202006 & tmobile_app == 0, tmobile_app := NA]
dataset[foto_mes == 202009 & ccajeros_propios_descuentos == 0, ccajeros_propios_descuentos := NA]
dataset[foto_mes == 202009 & ctarjeta_master_descuentos == 0, ctarjeta_master_descuentos := NA]
dataset[foto_mes == 202009 & ctarjeta_visa_descuentos == 0, ctarjeta_visa_descuentos := NA]
dataset[foto_mes == 202009 & mcajeros_propios_descuentos == 0, mcajeros_propios_descuentos := NA]
dataset[foto_mes == 202009 & mtarjeta_master_descuentos == 0, mtarjeta_master_descuentos := NA]
dataset[foto_mes == 202009 & mtarjeta_visa_descuentos == 0, mtarjeta_visa_descuentos := NA]
dataset[foto_mes == 202010 & ccajeros_propios_descuentos == 0, ccajeros_propios_descuentos := NA]
dataset[foto_mes == 202010 & ctarjeta_master_descuentos == 0, ctarjeta_master_descuentos := NA]
dataset[foto_mes == 202010 & ctarjeta_visa_descuentos == 0, ctarjeta_visa_descuentos := NA]
dataset[foto_mes == 202010 & mcajeros_propios_descuentos == 0, mcajeros_propios_descuentos := NA]
dataset[foto_mes == 202010 & mtarjeta_master_descuentos == 0, mtarjeta_master_descuentos := NA]
dataset[foto_mes == 202010 & mtarjeta_visa_descuentos == 0, mtarjeta_visa_descuentos := NA]
dataset[foto_mes == 202102 & ccajeros_propios_descuentos == 0, ccajeros_propios_descuentos := NA]
dataset[foto_mes == 202102 & ctarjeta_master_descuentos == 0, ctarjeta_master_descuentos := NA]
dataset[foto_mes == 202102 & ctarjeta_visa_descuentos == 0, ctarjeta_visa_descuentos := NA]
dataset[foto_mes == 202102 & mcajeros_propios_descuentos == 0, mcajeros_propios_descuentos := NA]
dataset[foto_mes == 202102 & mtarjeta_master_descuentos == 0, mtarjeta_master_descuentos := NA]
dataset[foto_mes == 202102 & mtarjeta_visa_descuentos == 0, mtarjeta_visa_descuentos := NA]
dataset[foto_mes == 202105 & ccajas_depositos == 0, ccajas_depositos := NA]


#rankeo las variables de comienzan con “m” miníscula que refieren a monto, que son las
#de mayor data drifting según inspección de densidades:

columnas_a_calcular <- names(dataset)[grepl("^(m|Visa_m|Master_m)", names(dataset))]

columnas_a_calcular <- setdiff(columnas_a_calcular, 
                               c("numero_de_cliente", "foto_mes", "clase_ternaria", "clase01", "azar", "training")
)

# Calculo el rank centrado en 0 para las columnas seleccionadas

setorder(dataset, foto_mes, numero_de_cliente)

dataset[, (paste0(columnas_a_calcular, "_rank_centered")) :=
          lapply(.SD, function(x) rank(x) - (length(x) + 1) / 2),
        .SDcols = columnas_a_calcular, ,  by = foto_mes]

# Elimino las columnas originales
dataset[, (columnas_a_calcular) := NULL]


# Hago un lag de 2 meses (según BO anterior los lags 1 y 2 son más importantes en modelo lags más alejados)
#Hago lag sólo para columnas de monto y cantidad
columnas_lag <- names(dataset)[grepl("^(m|Visa_m|Master_m|c|Visa_c|Master_c)", names(dataset)) & names(dataset) != "clase_ternaria"]

setorder(dataset, numero_de_cliente, foto_mes)

periods <- seq(1, 3) # Para 3 meses

for (i in periods){
  lagcolumns <- paste("lag", columnas_lag,i, sep=".")
  dataset[, (lagcolumns):= shift(.SD, type = "lag", fill = NA, n=i), .SDcols = columnas_lag,  by =numero_de_cliente]
}

## Calculo la diferencia entre el ranking actual y el promedio de rank de tres meses antes
# Obtener columnas a calcular que terminan con "_rank_centered"
columnas_rank_centered <- names(dataset)[grepl("_rank_centered$", names(dataset))]

# Crear columnas de diferencias
for (col in columnas_rank_centered) {
  # Extraer el nombre base de la variable
  base_name <- sub("_rank_centered$", "", col)
  
  # Obtener las columnas lag correspondientes
  lag_columns <- paste0("lag.", base_name, ".1", ":lag.", base_name, ".3")
  
  # Calcular el promedio de las columnas lag
  dataset[, paste0("promedio_", col) := rowMeans(.SD, na.rm = TRUE), .SDcols = lag_columns]
  
  # Calcular la diferencia entre la columna rank_centered y el promedio de las columnas lag
  dataset[, paste0("diff_", col) := get(col) - get(paste0("promedio_", col))]
}

#----------------------------------------------------------------------------------------


# ahora SI comienza la optimizacion Bayesiana

GLOBAL_iteracion <- 0 # inicializo la variable global
GLOBAL_gananciamax <- -1 # inicializo la variable global

# si ya existe el archivo log, traigo hasta donde llegue
if (file.exists(klog)) {
  tabla_log <- fread(klog)
  GLOBAL_iteracion <- nrow(tabla_log)
  GLOBAL_gananciamax <- tabla_log[, max(ganancia)]
}



# paso la clase a binaria que tome valores {0,1}  enteros
dataset[, clase01 := ifelse(clase_ternaria == "CONTINUA", 0L, 1L)]


# los campos que se van a utilizar
campos_buenos <- setdiff(
  colnames(dataset),
  c("clase_ternaria", "clase01", "azar", "training")
)

# defino los datos que forma parte del training
# aqui se hace el undersampling de los CONTINUA
set.seed(PARAM$trainingstrategy$semilla_azar)
dataset[, azar := runif(nrow(dataset))]
dataset[, training := 0L]
dataset[
  foto_mes %in% PARAM$input$training &
    (azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")),
  training := 1L
]

# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[training == 1L, campos_buenos, with = FALSE]),
  label = dataset[training == 1L, clase01],
  weight = dataset[training == 1L, 
                   ifelse(clase_ternaria == "BAJA+2", 1.0000001, 
                          ifelse(clase_ternaria == "BAJA+1", 1.0, 1.0))],
  free_raw_data = FALSE
)



# defino los datos que forman parte de validation
#  no hay undersampling
dataset[, validation := 0L]
dataset[ foto_mes %in% PARAM$input$validation,  validation := 1L]

dvalidate <- lgb.Dataset(
  data = data.matrix(dataset[validation == 1L, campos_buenos, with = FALSE]),
  label = dataset[validation == 1L, clase01],
  weight = dataset[validation == 1L, 
                   ifelse(clase_ternaria == "BAJA+2", 1.0000001, 
                          ifelse(clase_ternaria == "BAJA+1", 1.0, 1.0))],
  free_raw_data = FALSE
)


# defino los datos de testing
dataset[, testing := 0L]
dataset[ foto_mes %in% PARAM$input$testing,  testing := 1L]


dataset_test <- dataset[testing == 1, ]

# libero espacio
rm(dataset)
gc()

# Aqui comienza la configuracion de la Bayesian Optimization
funcion_optimizar <- EstimarGanancia_lightgbm # la funcion que voy a maximizar

configureMlr(show.learner.output = FALSE)

# configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
# por favor, no desesperarse por lo complejo
obj.fun <- makeSingleObjectiveFunction(
  fn = funcion_optimizar, # la funcion que voy a maximizar
  minimize = FALSE, # estoy Maximizando la ganancia
  noisy = TRUE,
  par.set = PARAM$bo_lgb, # definido al comienzo del programa
  has.simple.signature = FALSE # paso los parametros en una lista
)

# cada 600 segundos guardo el resultado intermedio
ctrl <- makeMBOControl(
  save.on.disk.at.time = 600, # se graba cada 600 segundos
  save.file.path = kbayesiana
) # se graba cada 600 segundos

# indico la cantidad de iteraciones que va a tener la Bayesian Optimization
ctrl <- setMBOControlTermination(
  ctrl,
  iters = PARAM$bo_iteraciones
) # cantidad de iteraciones

# defino el método estandar para la creacion de los puntos iniciales,
# los "No Inteligentes"
ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())


# establezco la funcion que busca el maximo
surr.km <- makeLearner(
  "regr.km",
  predict.type = "se",
  covtype = "matern3_2",
  control = list(trace = TRUE)
)

# inicio la optimizacion bayesiana
if (!file.exists(kbayesiana)) {
  run <- mbo(obj.fun, learner = surr.km, control = ctrl)
} else {
  run <- mboContinue(kbayesiana) # retomo en caso que ya exista
}


cat("\n\nLa optimizacion Bayesiana ha terminado\n")