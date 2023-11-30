# Limpio la memoria
rm(list = ls()) # remuevo todos los objetos
gc() # garbage collection

require("data.table")
require("lightgbm")

#-----------------------------------CONFIGURAR PARÁMETROS-------------------------------------------#
# Defino los parametros de la corrida, en una lista, la variable global  PARAM
PARAM <- list()

# Nombre del experimento
PARAM$experimento <- "KA_C3_SEM_goss_0504_SP" 

# Path donde se aloja el dataset (puede cargar su dataset preprocesado o puede hacerlo en el apartado de preprocesamiento de abajo)
PARAM$input$dataset <- "./datasets/competencia_03_V2.csv.gz"

# Meses donde se entrena el modelo
PARAM$input$training <- c(201906, 201907, 201908, 201909, 201910, 201911,
                          201912, 202001, 202002, 202011,
                          202012, 202101, 202102, 202103, 202104, 202105) 

# Mes donde aplico el modelo
PARAM$input$future <- c(202109)

# Defino parámetros:

# Parámetro variable (esto genera semillas con valor entre 15k y 80k, puede ajustar a preferencia)
cantidad_semillas = 10 #pruebo primero con estas
semillas <- as.integer(seq(15000, 80000, length.out = cantidad_semillas))

# Parámetros fijos obtenidos en la Optimización Bayesiana 
#iteración 46 ganancia: 87.02 (estímulos 11049)
PARAM$finalmodel$num_iterations <- 1254
PARAM$finalmodel$learning_rate <- 0.0207368173898424
PARAM$finalmodel$feature_fraction <- 0.992445662711932
PARAM$finalmodel$min_data_in_leaf <- 12518
PARAM$finalmodel$num_leaves <- 221
PARAM$finalmodel$feature_fraction_bynode <- 0.697408019403877
PARAM$finalmodel$max_depth <- 5
PARAM$finalmodel$top_rate <- 0.25963554579698
PARAM$finalmodel$other_rate <- 0.445368746596336
  
  
  
  #---------------------------------CARGAR DATOS---------------------------------------------#
  # Aqui empieza el programa que voy a ejecutar para cada semilla
  # Directorio de origen
  setwd("~/buckets/b1/")

# Cargo el conjunto de datos
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

#---------------------------------PREPROCESAMIENTO DE DATOS---------------------------------------------#
###############VARIABLES ROTAS#######################################

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


#############FE intra mes####################################

#calculo la inversa de los movimientos, para dar más peso a bajo movimientos

dataset[, inv_ctx_quarter := ifelse(!is.na(ctrx_quarter) & ctrx_quarter != 0, ctrx_quarter^1, NA)]

#paso los infinitos y NaN a NA
infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
infinitos_qty  <- sum( unlist( infinitos) )
if( infinitos_qty > 0 )
{
  dataset[mapply(is.infinite, dataset)] <- NA
}


nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
nans_qty  <- sum( unlist( nans) )
if( nans_qty > 0 )
{
  dataset[mapply(is.nan, dataset)] <- NA
}

#############Data drifting###################################

#elimino rank

#############FE histórico################

#se agrega lags(1,3,6) delta-lags(1,3,6) + media (lags 1,...,5)

# ordernar por mes:
setorderv(dataset, cols=c("foto_mes"), order=c(1L))

# No se eliminan los meses, se usan todos
# meses_a_usar = c(202003, 202004, 202005, 202006, 202007, 202008, 202009,
#                  202010, 202011, 202012, 202101, 202102, 202103, 202104, 
#                  202105, 202106, 202107)
# dataset = dataset[foto_mes %in% meses_a_usar]

if (FALSE) {  # Si se quiere borrar información de la pandemia
  meses_pandemia = c(202003, 202004, 202005, 202006, 202007, 202008, 202009)
  cols_a_borrar = setdiff(colnames(dataset), c("numero_de_cliente", "foto_mes"))
  dataset[foto_mes %in% meses_pandemia, (cols_a_borrar) := NA]
  rm(cols_a_borrar)
} else {      # Si se quiere mantener información de la pandemia
  # No se reemplazan los ceros con NA
  # cols_reemplazar_ceros = c("active_quarter", "internet", "mcuentas_saldo", ...)
  # dataset[foto_mes==202006, (cols_reemplazar_ceros) := NA]
  # rm(cols_reemplazar_ceros)
}

# Media en ventana, lags y delta lags

cols_con_lag <- setdiff(
  colnames(dataset),
  c("clase_ternaria", "foto_mes", "numero_de_cliente",
    "cliente_edad", "cliente_antiguedad")
)

# Media móvil en ventana
if (TRUE) {
  n <- 5L
  cols_media = c()
  for (col in cols_con_lag) {
    cols_media = c(cols_media, paste0(col, "_media_", n))
  }
  
  dataset[, (cols_media) := frollmean(.SD, n = n, fill = NA, na.rm = TRUE, align = "right", algo = "fast"),
          .SDcols = (cols_con_lag), by = numero_de_cliente]
  
  dataset[, (cols_media) := shift(.SD, n = 1L, fill = NA, type = "lag"),
          .SDcols = (cols_media), by = numero_de_cliente]
  
  rm(cols_media, n)
}

# Lags
n_lags = c(1, 3, 6)

for (i in n_lags) {
  cols_lag = c()
  for (col in cols_con_lag) {
    cols_lag = c(cols_lag, paste0(col, "_lag_", i))
  }
  dataset[, (cols_lag) := shift(.SD, n = (i), fill = NA, type = "lag"),
          .SDcols = (cols_con_lag), by = numero_de_cliente]
  
  rm(cols_lag)
}

# Delta lags
if (TRUE) {
  for (i in n_lags) {
    for (col in cols_con_lag) {
      col_lag = paste0(col, "_lag_", i)
      col_delta_lag = paste0(col, "_delta_", i)
      dataset[, (col_delta_lag) := get(col) - get(col_lag)]
    }
    rm(col_lag, col_delta_lag)
  }
}

rm(cols_con_lag)
#----------------------------------------------------------------------------------------


# Configuro la variable target como binaria
# El criterio: POS = { BAJA+1, BAJA+2 }, NEG {CONTINUA}
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#-----------------------------------SELECCIONAR DATOS-------------------------------------------#
# Campos a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

# Establezco qué datos usaré para entrenar
# Creo columna train con valor cero en todas sus filas
dataset[, train := 0L]

# Asigno un 1 a todas las filas correspondiente al foto_mes configurado en los parámetros de entrada
dataset[foto_mes %in% PARAM$input$training, train := 1L]

#---------------------------------CREAR DIRECTORIOS---------------------------------------------#
# Creo carpeta donde guardar los experimentos en caso de que no exista
dir.create("./competencia_3/exp/", showWarnings = FALSE)

# Creo carpeta donde guardar este experimento en caso de que no exista
dir.create(paste0("./competencia_3/exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory de este experimento
setwd(paste0("./competencia_3/exp/", PARAM$experimento, "/"))


#----------------------------------CONFIGURAR DATOS DE ENTRADA MODELO----------------------------------#
# Dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)

#----------------------------------ITERACIÓN----------------------------------#

# Obtengo los datos a predecir
dapply <- dataset[foto_mes == PARAM$input$future]

# Selecciono columna con numero de cliente y foto mes en df para guardar las predicciones
predicciones <- dapply[, list(numero_de_cliente, foto_mes)]

cat("\n\nEmpieza la iteración, hora:", Sys.time(), "\n")

for (semilla in semillas) {
  #----------------------------------CONFIGURAR MODELO--------------------------------------------#
  # Utilizo los parámetros configurados al inicio para el modelo
  
  modelo <- lgb.train(
    data = dtrain,
    param = list(
      boosting = "gbdt",
      objective = "binary",
      learning_rate = PARAM$finalmodel$learning_rate,
      num_iterations = PARAM$finalmodel$num_iterations,
      num_leaves = PARAM$finalmodel$num_leaves,
      min_data_in_leaf = PARAM$finalmodel$min_data_in_leaf,
      feature_fraction = PARAM$finalmodel$feature_fraction,
      feature_fraction_bynode = PARAM$finalmodel$feature_fraction_bynode,
      top_rate = PARAM$finalmodel$top_rate, 
      other_rate = PARAM$finalmodel$other_rate,
      metric = "custom",
      sample_strategy = "goss",
      first_metric_only = TRUE,
      boost_from_average = TRUE,
      feature_pre_filter = FALSE,
      force_row_wise = TRUE, # para reducir warnings
      verbosity = -100,
      max_depth = PARAM$finalmodel$max_depth, # -1 significa no limitar,  por ahora lo dejo fijo
      min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
      min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
      lambda_l1 = 0.0, # lambda_l1 >= 0.0
      lambda_l2 = 0.0, # lambda_l2 >= 0.0
      max_bin = 31L, # lo debo dejar fijo, no participa de la BO
      num_iterations = 9999, # un numero muy grande, lo limita early_stopping_rounds
      neg_bagging_fraction = 1.0,
      bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
      pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
      is_unbalance = FALSE, #
      scale_pos_weight = 1.0, # scale_pos_weight > 0.0
      drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
      max_drop = 50, # <=0 means no limit
      skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0
      extra_trees = TRUE, # Magic Sauce
      seed = semilla 
    )
  )
  
  #----------------------------------PERSISTIR IMPORTANCIA DE VARIABLES---------------------------------#
  # Este paso guarda la importancia de variables de cada modelo generado, puede descomentar si desea guardarlas)
  # Calculo la importancia de variables del modelo
  # tb_importancia <- as.data.table(lgb.importance(modelo))
  
  # Configuro nombre del archivo
  # archivo_importancia <- paste0("impo_", semilla, ".txt")
  
  # Guardo en el archivo 
  # fwrite(tb_importancia,
  # file = archivo_importancia,
  # sep = "\t"
  #)
  
  #----------------------------------PREDECIR SOBRE MES DE INTERÉS---------------------------------#
  # Aplico el modelo a los nuevos datos
  prediccion <- predict(
    modelo,
    data.matrix(dapply[, campos_buenos, with = FALSE])
  )
  
  # Agrego columna con las predicciones de cada semilla
  col_name <- paste0("semilla_", semilla)
  predicciones[, (col_name) := prediccion] 
  cat("\n\nSemilla número", semilla , "hora:", Sys.time(), "\n")
  
}

#-------------------------------PERSISTO SALIDA CON LAS PREDICCIONES DE CADA SEMILLA------------------------------#
# Guardo el archivo (con probas)
archivo_salida <- paste0(PARAM$experimento, "_predicciones_semillas.csv")
fwrite(predicciones, file = archivo_salida, sep = ",")

#-----------------------------------------------GENERO ENSEMBLE---------------------------------------------------#

# Calcular el promedio de las predicciones (probas) de los 100 modelos ejecutados (excluyo cols "numero_de_cliente" y "foto_mes")
predicciones$proba_ensemble <- rowMeans(predicciones[, .SD, .SDcols = -(1:2)])

cat("\n\nEnsemble generado, hora:", Sys.time(), "\n")

#------------------------------------------GENERO ENTREGA A KAGGLE------------------------------------------------#
# Ordeno por probabilidad descendente
setorder(predicciones, -proba_ensemble)


# Genero archivos variando la cantidad de estímulos
cortes <- seq(8000, 15000, by = 500)
for (envios in cortes) {
  predicciones[, Predicted := 0L]
  predicciones[1:envios, Predicted := 1L]
  
  fwrite(predicciones[, list(numero_de_cliente, Predicted)],
         file = paste0(PARAM$experimento, "_", envios, ".csv"),
         sep = ","
  )
}

cat("\n\nLa generacion de los archivos para Kaggle ha terminado, hora:", Sys.time(),"\n")
