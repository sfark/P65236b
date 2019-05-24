stats::AIC(fracdiff(X_t, nar = 1, nma = 2, drange = c(0)))
stats::BIC(fracdiff(X_t, nar = 1, nma = 2, drange = c(0)))
myrmse(fracdiff(X_t, nar = 1, nma = 2, drange = c(0)))


stats::AIC(fracdiff(X_t, nar = 3, nma = 4))
stats::BIC(fracdiff(X_t, nar = 3, nma = 4))
myrmse(fracdiff(X_t, nar = 3, nma = 4, drange = 0.19))

fracdiff(X_t, nar = 3, nma = 4)

