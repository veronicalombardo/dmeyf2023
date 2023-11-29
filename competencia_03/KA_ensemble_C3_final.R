#no establezco setwd porque lo hice local a través de un proyecto
#setwd('~/buckets/b1')

# Limpio la memoria
rm(list = ls()) # remuevo todos los objetos
gc() # garbage collection

require("data.table")

mod_goss <- fread("competencia_3_exp_KA_C3_SEM_goss_KA_C3_SEM_goss_predicciones_semillas.csv")
mod_lr1_SP <- fread("competencia_3_exp_KA_C3_SEM_LR1_SP_KA_C3_SEM_LR1_SP_predicciones_semillas.csv")
mod_JR <- fread("competencia_3_exp_KA_C3_JR_SP_KA_C3_JR_SP_predicciones_semillas.csv")
mod_0405 <- fread("competencia_3_exp_KA_C3_SEM_goss_0504_SP_KA_C3_SEM_goss_0504_SP_predicciones_semillas.csv")

mod_goss$prob <- rowMeans(mod_goss[, .SD, .SDcols = -(1:2)])
mod_lr1_SP$prob <- rowMeans(mod_lr1_SP[, .SD, .SDcols = -(1:2)])
mod_JR$prob <- rowMeans(mod_JR[, .SD, .SDcols = -(1:2)])
mod_0405$prob <- rowMeans(mod_0405[, .SD, .SDcols = -(1:2)])

mod_goss <- mod_goss[,list(numero_de_cliente,foto_mes,prob)]
mod_lr1_SP <- mod_lr1_SP[,list(numero_de_cliente,foto_mes,prob)]
mod_0405 <- mod_0405[,list(numero_de_cliente,foto_mes,prob)]

df <- rbind(mod_goss,
            mod_JR,
            mod_lr1_SP,
            mod_0405)
df <- df[,sum(prob),by=numero_de_cliente]
df <- setorder(df,-V1)

cortes <- seq(8000, 15000, by = 500)
for (envios in cortes) {
  df[, Predicted := 0L]
  df[1:envios, Predicted := 1L]
  
  fwrite(df[, list(numero_de_cliente, Predicted)],
         file = paste0("KA_ensemble_final", "_", envios, ".csv"),
         sep = ","
  )
}