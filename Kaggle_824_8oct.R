#Para Kaggle, de BO 8oct

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")


# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento <- "KA8240_8oct"

PARAM$input$dataset <- "./datasets/competencia_02.csv.gz"

# meses donde se entrena el modelo
PARAM$input$training <- c(201904, 201905,201906,201907,
                          201908,201909,201910,201911,201912, 202001, 202002, 202003,
                          202004, 202005,202006,202007,
                          202008,202009,202010,202011,202012,
                          202101, 202102, 202103, 202104, 202105)
PARAM$input$future <- c(202107) # meses donde se aplica el modelo

PARAM$finalmodel$semilla <- 119831

# hiperparametros óptimos según BO_ganancia 83654582.71
PARAM$finalmodel$optim$num_iterations <- 480
PARAM$finalmodel$optim$learning_rate <- 0.04130927085
PARAM$finalmodel$optim$feature_fraction <- 	0.2696403008
PARAM$finalmodel$optim$min_data_in_leaf <- 260
PARAM$finalmodel$optim$num_leaves <- 234


# Hiperparametros FIJOS de  lightgbm
PARAM$finalmodel$lgb_basicos <- list(
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
  
  bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
  pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
  neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
  is_unbalance = FALSE, #
  scale_pos_weight = 1.0, # scale_pos_weight > 0.0
  
  drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop = 50, # <=0 means no limit
  skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0
  
  extra_trees = TRUE, # Magic Sauce
  
  seed = PARAM$finalmodel$semilla
)


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
setwd("~/buckets/b1")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

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


#----------------------------------------------------------------------------------------


# paso la clase a binaria que tome valores {0,1}  enteros
# set trabaja con la clase  POS = { BAJA+1, BAJA+2 }
# esta estrategia es MUY importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------

# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

#--------------------------------------


# establezco donde entreno
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]

#--------------------------------------
# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))



# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)


# genero el modelo
param_completo <- c(PARAM$finalmodel$lgb_basicos,
                    PARAM$finalmodel$optim)

modelo <- lgb.train(
  data = dtrain,
  param = param_completo,
)

#--------------------------------------
# ahora imprimo la importancia de variables
tb_importancia <- as.data.table(lgb.importance(modelo))
archivo_importancia <- "impo.txt"

fwrite(tb_importancia,
       file = archivo_importancia,
       sep = "\t"
)

#--------------------------------------


# aplico el modelo a los datos sin clase
dapply <- dataset[foto_mes == PARAM$input$future]

# aplico el modelo a los datos nuevos
prediccion <- predict(
  modelo,
  data.matrix(dapply[, campos_buenos, with = FALSE])
)

# genero la tabla de entrega
tb_entrega <- dapply[, list(numero_de_cliente, foto_mes)]
tb_entrega[, prob := prediccion]

# grabo las probabilidad del modelo
fwrite(tb_entrega,
       file = "prediccion.txt",
       sep = "\t"
)

# ordeno por probabilidad descendente
setorder(tb_entrega, -prob)


# genero archivos con los  "envios" mejores
# deben subirse "inteligentemente" a Kaggle para no malgastar submits
# si la palabra inteligentemente no le significa nada aun
# suba TODOS los archivos a Kaggle
# espera a la siguiente clase sincronica en donde el tema sera explicado

cortes <- seq(8000, 15000, by = 500)
for (envios in cortes) {
  tb_entrega[, Predicted := 0L]
  tb_entrega[1:envios, Predicted := 1L]
  
  fwrite(tb_entrega[, list(numero_de_cliente, Predicted)],
         file = paste0(PARAM$experimento, "_", envios, ".csv"),
         sep = ","
  )
}

cat("\n\nLa generacion de los archivos para Kaggle ha terminado\n")