La ganancia en el leaderboard público de 124 millones seleccionada surge de:
1) BO con entrenando desde 201906 a 202105 (sólo excluye 202010), se agregan los hiperparámetros neg_bagging_fraction, max_depth, top_rate, other_rate y se aplica goss
2) BO incluye variables rotas a NA, rank fijo, lags y deltas 1,3 y 6, media(lags(1 a 5)), FE intra-mes
3) con los mejores hiperparámetros de esa BO se hace semillerío
4) Por otro lado se aplica lightgbm final con semillerío para learning_rate =1 (clase plenaria), en el que se elimina los meses 202003 a 202009 (pandemia),
   se pasan variables rotas a NA y lags y deltas 1,3 y 6, media(lags(1 a 5))
5) Se realiza un ensemble de ambos modelos (Goss y LR1) y se sube a Kaggle obteniendo para 10500 envíos una ganancia de 124 millones (public LB) 
