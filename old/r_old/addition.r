library(fGarch)
library(rugarch)
library(rmgarch)
library(fOptions)
library(timeSeries)
library(lubridate)
library(tseries)
library(bayesGARCH)
library(betategarch)
library(dplyr)
library(psych)
library(Quandl)
library(curl)
library(stringi)
library(zoo)
library(ccgarch)
library(mgarchBEKK)
library(ggplot2)
library(forecast)
library(memisc)
library(stargazer)

# Loading data frame
setwd("C:/Users/Van Khachatryan/Desktop/Models")
df <- read.csv("Daily19.csv", header = TRUE)
head(df)
tail(df)

# Transforming data to data format
df$Date <- mdy(df$Date)

# Composing time series for USDRUB and Brent
usdrub <- timeSeries(df$USDRUB, as.Date(df$Date))
brent <- timeSeries(df$Brent, as.Date(df$Date))


plot(usdrub, type = "l", ylab = "Обменный курс руб/$", xlab = "", yaxt = NULL, col = "red")
grid(5)
plot(brent, type = "l", ylab = "Стоимость индекса Brent", xlab = "", yaxt = NULL, col = "red")
grid(5)
par(mfrow = c(2, 1))

# Computing logreturns
n <- length(df$Date)
log_usdrub <- log(usdrub[-1] / usdrub[-n])
log_brent <- log(brent[-1] / brent[-n])

# Log returns to time series
tslog_usdrub <- timeSeries(log_usdrub, as.Date(df$Date[-1]))
tslog_brent <- timeSeries(log_brent, as.Date(df$Date[-1]))

# Plotting returns and prices
plot(usdrub, type = "l", main = "Обменный курс руб/$", ylab = "Значение", xlab = "Дата")
plot(brent, type = "l", main = "Стоимость индекса Brent", ylab = "Значение", xlab = "Дата")
plot(tslog_usdrub, type = "l", ylab = "Лог-доходность обменного курса", xlab = "", col = "red")
grid(6)
plot(tslog_brent, type = "l", ylab = "Лог-доходность индекса Brent", xlab = "", col = "red")
grid(6)
# Creating data frame for BEKK model
r_t <- cbind("log_USDRUB" = log_usdrub, "log_Brent" = log_brent)

#xspec = ugarchspec(mean.model = list(armaOrder = c(1, 1)), 
#                   variance.model = list(garchOrder = c(1,1), model = 'sGARCH'), 
#                   distribution.model = 'norm')
#uspec = multispec(replicate(2, xspec))

#spec = dccspec(uspec = uspec, dccOrder = c(1, 1), distribution = 'mvnorm')


#fit1 = dccfit(spec, data = r_t, fit.control = list(eval.se = TRUE))
#print(fit1)       

#sigma_usdrub <- fit1@model$sigma[,1]
#sigma_brent <- fit1@model$sigma[,2]
#reg <- lm(sigma_usdrub~sigma_brent)
#summary(reg)

# BEKK from 2005 to 2016
bivGarchall <- BEKK(r_t, order = c(1, 1), params = NULL, fixed = NULL, method = "BFGS", verbose = F)
sigma_usdruball <- bivGarchall$sd[[1]]
sigma_brentall <- bivGarchall$sd[[2]]
reg <- lm(sigma_usdruball ~ sigma_brentall)
summary(reg)


cb <- c(rep(1, 1810), rep(0, 618))
reg1 <- lm(sigma_usdruball ~ sigma_brentall + cb)
summary(reg1)
mtable(reg, reg1)
stargazer(reg, reg1)


bivGarchall$estimation
cov_rubbrent <- bivGarchall$cor
cov_rubbrent
covdf <- data.frame(do.call(rbind, cov_rubbrent))
cov_rubbrent <- covdf[, 1][[2]]
# Moving window for estimated sigma from 2005 to 2016


summary(reg)
plot(timeSeries(sigma_usdruball, as.Date(df$Date[-1])), type = "l", col = "red", xlab = "", ylab = "")
lines(timeSeries(sigma_brentall, as.Date(df$Date[-1])), type = "l", col = "dark blue")
legend("top", legend = c("Обменный курс", "Brent"),
       col = c("red", "dark blue"), lty = c(1, 1), lwd = c(1, 1), cex = 0.9, bty = "n")
grid(6)

par(mfrow = c(1, 1))

summary(reg)$r.squared
summary(reg)$coef[2, 4]
coef(reg)[2]
coef(summary(reg))[2, 4]
# Creating moving window cycle
reg_rsq <- numeric()
brent_coef <- numeric()
brent_pval <- numeric()
for (i in 0:435) {
  bivGarch <- BEKK(r_t[((5 * i) + 1):((5 * i) + 250),], order = c(1, 1), params = NULL,
                   fixed = NULL, method = "BFGS", verbose = F)
  sigma_usdrub <- bivGarch$sd[[1]]
  sigma_brent <- bivGarch$sd[[2]]
  reg <- lm(sigma_usdrub ~ sigma_brent + cb[((5 * i) + 1):((5 * i) + 250)])
  reg_rsq[i] <- summary(reg)$r.squared # R-squared
  brent_coef[i] <- coef(reg)[2] # brent coefficient
  brent_pval[i] <- coef(summary(reg))[2, 4] # brent coefficient p-value
  cat(i)
}

plot(reg_rsq, type = "l", main = "R-squared of linear regression", ylim = c(-0.1, 1), col = "red")
legend("topleft", legend = "R-squared of linear regression sigma_usdrub ~ c + sigma_brent",
       col = "red", lty = 1, lwd = 2, cex = 0.9, bty = "n")
grid(6)
plot(brent_coef, type = "l", main = "Sigma_brent coefficient estimate", ylim = c(-0.1, 5), col = "red")
lines(brent_coefall, type = "l", main = "Sigma_brent coefficient estimate", col = "blue")
legend("topleft", legend = "beta_sigma_brent",
       col = "red", lty = 1, lwd = 2, cex = 0.9, bty = "n")
grid(6)
plot(brent_pval, type = "l", main = "P-value of sigma_brent coefficient estimate", col = "red")
legend("topleft", legend = "p-value of beta_sigma_brent",
       col = "red", lty = 1, lwd = 2, cex = 0.9, bty = "n")
grid(6)

#############
############

plot(reg_rsq, type = "l", ylim = c(-0.1, 1))
lines(brent_pval, lty = 2, col = "red", lwd = 2)
lines(brent_coef, lty = 2, col = "blue", lwd = 2)
legend("topleft", legend = c("R-для регрессии sigma_usdrub ~ c + sigma_brent", "p-value для sigma_brent", "параметр при sigma_brent"),
       col = c("black", "red", "blue"), lty = 1:3, lwd = 2, cex = 0.9, bty = "n")
grid(6)
##########
##############

# Creating moving window cycle
reg_rsqall <- numeric()
brent_coefall <- numeric()
brent_pvalall <- numeric()
for (i in 0:435) {
  sigma_usdruball <- bivGarchall$sd[[1]]
  sigma_brentall <- bivGarchall$sd[[2]]
  reg_cycle <- lm(sigma_usdruball[((5 * i) + 1):((5 * i) + 250)] ~ sigma_brentall[((5 * i) + 1):((5 * i) + 250)]
    + cb[((5 * i) + 1):((5 * i) + 250)])
  reg_rsqall[i] <- summary(reg_cycle)$r.squared # R-squared
  brent_coefall[i] <- coef(reg_cycle)[2] # brent coefficient
  brent_pvalall[i] <- coef(summary(reg_cycle))[2, 4] # brent coefficient p-value
  cat(i)
}

reg_rsqall <- numeric()
brent_coefall <- numeric()
brent_pvalall <- numeric()
for (i in 0:2177) {
  sigma_usdruball <- bivGarchall$sd[[1]]
  sigma_brentall <- bivGarchall$sd[[2]]
  reg_cycle <- lm(sigma_usdruball[(i + 1):(i + 250)] ~ sigma_brentall[(i + 1):(i + 250)]
    + cb[(i + 1):(i + 250)])
  reg_rsqall[i] <- summary(reg_cycle)$r.squared # R-squared
  brent_coefall[i] <- coef(reg_cycle)[2] # brent coefficient
  brent_pvalall[i] <- coef(summary(reg_cycle))[2, 4] # brent coefficient p-value
  cat(i)
}

plot(timeSeries(reg_rsqall, as.Date(df$Date[1:2177])), type = "l", xlab = "",
     ylab = "R-квадрад линейной регрессии", ylim = c(-0.1, 1), col = "dark blue")
grid(6)
plot(timeSeries(brent_coefall, as.Date(df$Date[1:2177])), type = "l", xlab = "",
     ylab = "Параметр при дисперсии Brent", col = "red")
grid(6)
plot(timeSeries(brent_pvalall, as.Date(df$Date[1:2177])), type = "l", xlab = "",
     ylab = "p-значение параметра при Brent", col = "blue")
grid(6)

# GARCH(1,1)
Variance.Model <- ugarchspec(mean.model = list(armaOrder = c(1, 1)), distribution = "std")
GARCH.Fit1 <- ugarchfit(Variance.Model, log_usdrub, out.sample = 0)
GARCH.Fit2 <- ugarchfit(Variance.Model, log_brent, out.sample = 0)
sigma_usdruball <- GARCH.Fit1@fit$sigma
sigma_brentall <- GARCH.Fit2@fit$sigma
greg <- lm(sigma_usdruball ~ sigma_brentall)
summary(greg)
greg1 <- lm(sigma_usdruball ~ sigma_brentall + cb)
summary(greg1)
mtable(reg, reg1, greg, greg1)
stargazer(reg, reg1, greg, greg1)

plot(timeSeries(sigmag_usdrub, as.Date(df$Date[-1])), type = "l", col = "red", xlab = "", ylab = "")
lines(timeSeries(sigmag_brent, as.Date(df$Date[-1])), type = "l", col = "dark blue")
legend("top", legend = c("Обменный курс", "Brent"),
       col = c("red", "dark blue"), lty = c(1, 1), lwd = c(1, 1), cex = 0.9, bty = "n")
grid(6)

# Creating moving window cycle
reg_rsqall <- numeric()
brent_coefall <- numeric()
brent_pvalall <- numeric()
for (i in 0:2177) {
  sigmag_usdrub <- GARCH.Fit1@fit$sigma
  sigmag_brent <- GARCH.Fit2@fit$sigma
  reg_cycle <- lm(sigmag_usdrub[(i + 1):(i + 250)] ~ sigmag_brent[(i + 1):(i + 250)])
  reg_rsqall[i] <- summary(reg_cycle)$r.squared # R-squared
  brent_coefall[i] <- coef(reg_cycle)[2] # brent coefficient
  brent_pvalall[i] <- coef(summary(reg_cycle))[2, 4] # brent coefficient p-value
  cat(i)
}

plot(timeSeries(reg_rsqall, as.Date(df$Date[1:2177])), type = "l", xlab = "",
     ylab = "R-квадрад линейной регрессии", ylim = c(-0.1, 1), col = "dark blue")
grid(6)
plot(timeSeries(brent_coefall, as.Date(df$Date[1:2177])), type = "l", xlab = "",
     ylab = "Параметр при дисперсии Brent", col = "red")
grid(6)
plot(timeSeries(brent_pvalall, as.Date(df$Date[1:2177])), type = "l", xlab = "",
     ylab = "p-значение параметра при Brent", col = "blue")
grid(6)


plot(reg_rsqall, type = "l", main = "R-squared of linear regression", ylim = c(-0.1, 1), col = "red")
legend("topleft", legend = "R-squared of linear regression sigma_usdrub ~ c + sigma_brent",
       col = "red", lty = 1, lwd = 2, cex = 0.9, bty = "n")
grid(6)
plot(brent_coefall, type = "l", main = "Sigma_brent coefficient estimate", col = "red")
legend("topleft", legend = "beta_sigma_brent",
       col = "red", lty = 1, lwd = 2, cex = 0.9, bty = "n")
grid(6)
plot(brent_pvalall, type = "l", main = "P-value of sigma_brent coefficient estimate", col = "red")
legend("topleft", legend = "p-value of beta_sigma_brent",
       col = "red", lty = 1, lwd = 2, cex = 0.9, bty = "n")
grid(6)

plot(sigmag_usdrub, type = "l")
lines(sigmag_brent, type = "l", col = "red")

tssigmag_usdrub <- timeSeries(sigmag_usdrub, as.Date(df$Trade.Date[-1]))
tssigmag_brent <- timeSeries(sigmag_brent, as.Date(df$Trade.Date[-1]))

tssigma_usdruball <- timeSeries(sigma_usdruball, as.Date(df$Trade.Date[-1]))
tssigma_brentall <- timeSeries(sigma_brentall, as.Date(df$Trade.Date[-1]))

plot(tssigmag_usdrub, type = "l")
lines(tssigmag_brent, type = "l", col = "red")
lines(tssigma_usdruball, type = "l", col = "blue")
lines(tssigma_brentall, type = "l", col = "orange")
tscov_rubbrent <- timeSeries(cov_rubbrent, as.Date(df$Trade.Date[-1]))
plot(cov_rubbrent, type = "l")
lines(as.numeric(ma(cov_rubbrent, order = 20)), col = "blue")
mean(cov_rubbrent[-1])


corrdf <- as.data.frame(cbind(cov_rubbrent, df$Trade.Date[-1]))
ggplot(corrdf, aes(df$Trade.Date[-1], cov_rubbrent)) + geom_line()


acf(log_brent, main = "ACF доходности Brent", xlab = "", col = "red")
grid(4)
pacf(log_brent, main = "PACF доходности Brent", xlab = "", col = "red")
grid(4)
acf(abs(log_brent), main = "ACF абсолютной доходности Brent", xlab = "", col = "red")
grid(4)
pacf(abs(log_brent), main = "PACF абсолютной доходности Brent", xlab = "", col = "red")
grid(4)

acf(log_usdrub, main = "ACF доходности RUB/USD", xlab = "", col = "red")
grid(4)
pacf(log_usdrub, main = "PACF доходности RUB/USD", xlab = "", col = "red")
grid(4)
acf(abs(log_usdrub), main = "ACF абсолютной доходности RUB/USD", xlab = "", col = "red")
grid(4)
pacf(abs(log_usdrub), main = "PACF абсолютной доходности RUB/USD", xlab = "", col = "red")
grid(4)

set.seed(91923)
rnv <- rnorm(100000, 0, 1)
rtv1 <- (rt(100000, 3, 0) - mean(rt(100000, 3, 0))) / sd(rt(100000, 3, 0))
rtv2 <- (rt(100000, 4, 0) - mean(rt(100000, 4, 0))) / sd(rt(100000, 4, 0))
plot(density(((log_usdrub - mean(log_usdrub)) / sd(log_usdrub))), type = "l", main = " ", ylab = "Плотность", xlab = "Доходность",
     lwd = 2, xlim = c(-10, 10))

polygon(density(((log_usdrub - mean(log_usdrub)) / sd(log_usdrub))), col = "grey", border = "black")
lines(density(rnv), lty = 2, col = "red", lwd = 2)
lines(density(rtv1), lty = 3, col = "blue", lwd = 3)
lines(density(rtv2), lty = 4, col = "green", lwd = 2)
legend("topleft", legend = c("Наблюдаемая доходность USD/RUB", "Нормальное распр.", "t - распр. (q = 3)",
                             "t - распр. (q = 4)"),
       col = c("black", "red", "blue", "green"), lty = 1:4, lwd = 2, cex = 0.9, bty = "n")
grid(4)

set.seed(91923)
rnv <- rnorm(100000, 0, 1)
rtv1 <- (rt(100000, 3, 0) - mean(rt(100000, 3, 0))) / sd(rt(100000, 3, 0))
rtv2 <- (rt(100000, 4, 0) - mean(rt(100000, 4, 0))) / sd(rt(100000, 4, 0))
plot(density(((log_brent - mean(log_brent)) / sd(log_brent))), type = "l", main = " ", ylab = "Плотность", xlab = "Доходность",
     lwd = 2, xlim = c(-10, 10))
polygon(density(((log_brent - mean(log_brent)) / sd(log_brent))), col = "grey", border = "black")
lines(density(rnv), lty = 2, col = "red", lwd = 2)
lines(density(rtv1), lty = 3, col = "blue", lwd = 2)
lines(density(rtv2), lty = 4, col = "green", lwd = 3)
legend("topleft", legend = c("Наблюдаемая доходность Brent", "Нормальное распр.", "t - распр. (q = 3)",
                             "t - распр. (q = 4)"),
       col = c("black", "red", "blue", "green"), lty = 1:4, lwd = 2, cex = 0.9, bty = "n")
grid(4)


plot(density(RTS.Norm), type = "l", main = " ", ylab = "Плотность", xlab = "Доходность",
     lwd = 2, xlim = c(-10, 10))
polygon(density(RTS.Norm), col = "grey", border = "black")
lines(density(rnv), lty = 2, col = "red", lwd = 2)
legend("topleft", legend = c("Наблюдаемая доходность", "Нормальное распр."),
       col = c("black", "red"), lty = 1:2, lwd = 2, cex = 0.9, bty = "n")
grid(6)


