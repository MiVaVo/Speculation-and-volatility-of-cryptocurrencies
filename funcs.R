ARIMA_GARCH_optimizer <- function(p_range, q_range,P_range,Q_range,ext_regress,y){
  seq_of_res=c()
  for (p in p_range){
    for (q in q_range){
      for (P in P_range){
        for (Q in Q_range){
          for (include.mean in c(TRUE,FALSE)){
            for (arfima in c(TRUE,FALSE)){
              if ( p==q && p==0){
                next
              }
              g1=ugarchspec(variance.model = list(model = "sGARCH",
                                                  garchOrder = c(p, q),
                                                  submodel = NULL,
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model  = list(armaOrder = c(P,Q),
                                               arfima =arfima,include.mean = include.mean,
                                               external.regressors = ext_regress),
                            distribution.model = "std")
              g1fit=ugarchfit(g1,data=y)
              print(c(infocriteria(g1fit)[1],P,Q,include.mean,arfima))
              seq_of_res<-c(seq_of_res,c(infocriteria(g1fit)[1],P,Q,include.mean,arfima))
            }
          }
        }
      }
    }
  }
  return(seq_of_res)
}

