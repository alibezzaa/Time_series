library(forecast)
t = c(1:12)
demande = c(6, 11, 9, 5, 8, 14, 10, 7, 7, 15, 14, 9)
demande_ts = ts(demande, start = c(2017, 1), frequency = 4)
autoplot(demande_ts)

# étape 1 : trend 
trend_demande = ma(demande_ts, order = 4, centre = TRUE)
trend_demande
autoplot(trend_demande)

# étape 2 : supprimer le trend 
demande_detrend = demande_ts/trend_demande

# étape 3 : faire la moyenne des coefficients saisonniers
matrice_saison = t(matrix( data = demande_detrend, nrow = 4))
matrice_saison
coef_saison = colMeans(matrice_saison, na.rm = TRUE)
coef_saison

# étape 4 : calculer le résidu

residu = demande_ts/(trend_demande*coef_saison)
residu
autoplot(residu)

# étape 5 : reconstruire la série originale

demande_recomp = coef_saison*trend_demande*residu
demande_recomp
demande_ts

# la fonction decompose

decompose(demande_ts, type = "multiplicative")
coef_saison
plot(decompose(demande_ts, type = "multiplicative"))
library(Quandl)
data = Quandl("FRED/NATURALGAS", type = "ts", )
autoplot(data)
decompose(data)
