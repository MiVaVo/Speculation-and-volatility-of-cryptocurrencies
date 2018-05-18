rm(list=ls())
library(lmtest) ; library(sandwich)

# data sim
install.packages('texreg')
install.packages('sandwich')
install.packages('lmtest')
library(texreg)

set.seed(1)
x <- rnorm(1000)
y <- 5 + 2*x + rnorm(1000,0,1)
# regression
m1 <- lm(y~x)
summary(m1)

# triple data
dat <- data.frame(x=c(x,x,x),y=c(y,y,y),g=c(1:1000,1:1000,1:1000))
# regressions
m2 <- lm(y~x, dat) # smaller StErrs
coeftest(m2, vcov = sandwich)                # robust; sandwich
summary()
coeftest(m2, df = Inf, vcov = vcovHAC)
# cluster robust standard error function
robust.se <- function(model, cluster){
  require(sandwich)
  require(lmtest)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
  rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
  rcse.se <- coeftest(model, rcse.cov)
  return(list(rcse.cov, rcse.se))
}

m3 <- robust.se(m2,dat$g)[[2]] # StErrs now back to what they are
m3<-coeftest(m2,df = Inf, vcov = vcovHAC)
texreg(list(m1,m2,m2),
       caption="The Importance of Clustering Standard Errors",
       dcolumn=FALSE,
       model.names=c("M1","M2","M3"),
       override.se=list(summary(m1)$coef[,2],
                        summary(m2)$coef[,2],
                        m3[,2]),
       override.pval=list(summary(m1)$coef[,4],
                          summary(m2)$coef[,4],
                          m3[,4]))
model_names_in_seq=c('1','2')
list_of_models=list(m1,m2)
list_of_models_with_robus_se=list(coeftest(m1,df = Inf, vcov = vcovHAC),
                                  coeftest(m2,df = Inf, vcov = vcovHAC)
                                  )
# make_latex_table_with_robust_coeffs<-function(list_of_models,
#                                            model_names_in_seq){
#   list_of_models_with_robus_se=lapply(list_of_models,function(i) coeftest(i,df = Inf, vcov = vcovHAC))
#   texreg(list_of_models,
#          # caption="The Importance of Clustering Standard Errors",
#          dcolumn=FALSE,
#          # model.names=model_names_in_seq,
#          custom.model.names=model_names_in_seq,
#          
#          override.se=lapply(list_of_models_with_robus_se,function(i) i[,2] ),
#          override.pval=lapply(list_of_models_with_robus_se,function(i) i[,4] ))
#   
# }
make_latex_table_with_robust_coeffs(list_of_models = lm_models_list,
                                    model_names_in_seq = c("BTC Model","ETH Model","XRP Model") )
