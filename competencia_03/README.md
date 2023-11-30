La ganancia en el **leaderboard público de 128 millones** seleccionada surge de:

1) BO entrenando desde 201906 a 202105 (sólo excluye 202010), validando en 202106 y testeando en 202107, se agregan los hiperparámetros neg_bagging_fraction, max_depth, top_rate, other_rate y se aplica goss
BO incluye variables rotas a NA, rank fijo, lags y deltas 1,3 y 6, media(lags(1 a 5)), FE intra-mes => **Archivo: BO_C3_0607_goss**
2) con los mejores hiperparámetros de esa BO se hace semillerío (sólo 10 semillas... pobreza total por tiempo) => **Archivo: KA_C3_SEM_0607_goss**
3) Por otro lado se aplica lightgbm final con semillerío (50 semillas) para learning_rate =1 (clase plenaria), en el que se elimina los meses 202003 a 202009 (pandemia),
se pasan variables rotas a NA y lags y deltas 1,3 y 6, media(lags(1 a 5)) => **Archivo: KA_C3_LR1_SP**
4) BO entrenando desde 201906 a 202103 (excluye meses 202003 a 202009), validando en 202104 y testeando en 202105, se agregan los hiperparámetros neg_bagging_fraction, max_depth, top_rate, other_rate y se aplica goss
BO incluye variables rotas a NA, se elimina rank fijo, incluye lags y deltas 1,3 y 6, media(lags(1 a 5)), se eliminando FE intra-mes (salvo inv_ctrx_quarter) => **Archivo: BO_C3_0405_goss**
5) Se realiza mismo semillerío que en el punto 2 => **KA_C3_SEM_0504_goss_SP**
6) Se aplica semillerío de 20 semillas, mismo FE que el modelo anterior con los hiperparámetros compartidos por Juan Raman => **KA_C3_JR_SP**
7) Se realiza un ensemble de los 4 modelos y se sube a Kaggle obteniendo para **9500 envíos** una ganancia de 128 millones (public LB) 
