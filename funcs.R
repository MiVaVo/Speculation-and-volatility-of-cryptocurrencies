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

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

par(mar=c(4.1,4.1,3.1,2.1),mfrow=c(3,1))
plot_1_chart <- function(x,name){
  # name='BTC'
  vmodel  = x@model$modeldesc$vmodel
  T = x@model$modeldata$T
  insample = 1:T
  xdates  = x@model$modeldata$index[insample]
  xseries = x@model$modeldata$data[insample]
  xsigma  = x@fit$sigma
  ci = 2
  # if is_top
  plot(xdates, xseries, type = "l",  col = "steelblue", ylab = "Доходность", xlab="Время",
       main = paste("Временной ряд с 2 SD  доходности",name,sep=','),
       cex.main = 2)
  lines(xdates, +ci* xsigma , col = "tomato1")
  lines(xdates, -ci* xsigma, col = "tomato1")
  mtext(paste("GARCH model : ", vmodel), side = 4, adj = 0, padj=0, col = "gray", cex = 0.5)
  if(vmodel == "fGARCH"){
    mtext(paste("fGARCH submodel: ", x@model$modeldesc$vsubmodel, sep = ""), side = 4, adj = 0, padj=1.5, col = "gray", cex = 0.5)
  }
  abline(h = 0, col = "grey", lty = 3)
  grid()
}

plot_8_chart<-function(x,name,...){
  vmodel  = x@model$modeldesc$vmodel
  zseries = as.numeric(residuals(x, standardize=TRUE))
  distribution = x@model$modeldesc$distribution
  idx = x@model$pidx
  pars  = x@fit$ipars[,1]
  skew  = pars[idx["skew",1]]
  shape = pars[idx["shape",1]]
  if(distribution == "ghst") ghlambda = -shape/2 else ghlambda = pars[idx["ghlambda",1]]
  xmean 	= mean(zseries)
  xmedian = median(zseries)
  xsd 	= sd(zseries)
  # xlim 	= c(min(zseries), max(zseries))
  xlim = c(-4,4)
  result 	= hist(x = zseries, col = "grey", border = "white",
                 breaks = "Scott", main = paste("Эмпирическое распределение стандартизированных ошибок",
                                                name,sep=','), xlim = xlim, ylim = c(0,0.6),
                 probability = TRUE, ylab="Вероятность",
                 # ,
                 cex.main = 1.1, ...)
  box()
  #s 	= seq(xlim[1], xlim[2], length = 201)
  s = result$breaks
  y	= ddist(distribution, s, lambda = ghlambda, skew = skew, shape = shape)
  lines(s, dnorm(s, 0, 1), lwd = 2, col = "blue")
  lines(s, y, lwd = 2, col = "orange")
  abline(v = xmean, lwd = 2, col = "red")
  abline(v = xmedian, lwd = 2, col = "darkgreen")
  Text = paste("Медиана: ", round(xmedian, 2), "| Среднее: ",signif(xmean, 3))
  mtext(Text, side = 3, adj = 0, col = "darkgrey", cex = 0.8)
  mtext(paste("GARCH model : ", vmodel), side = 4, adj = 0, padj=0, col = "gray", cex = 0.5)
  if(vmodel == "fGARCH"){
    mtext(paste("fGARCH submodel: ", x@model$modeldesc$vsubmodel, sep = ""), side = 4, adj = 0, padj = 1.5, col = "gray", cex = 0.5)
  }
  abline(h = 0, col = "grey")
  lg.txt = c("Нормальное распределение", paste(distribution," (0,1) Восстановленное распределение",sep=""))
  legend("topleft", legend = lg.txt, col = c("blue","orange"), pch = 2, cex = 0.9, bty = "n")
  grid()
}

plot_9_chart<-function(x,name,...){
  vmodel  = x@model$modeldesc$vmodel
  zseries = as.numeric(residuals(x, standardize=TRUE))
  qqnorm(zseries, pch = 1, frame = FALSE,main = paste('QQ-граифик, t-распределение',name,sep=','))
  qqline(zseries, col = "steelblue", lwd = 2)
}

plot_10_chart = function(x,name, ...)
{
  vmodel  = x@model$modeldesc$vmodel
  zseries = as.numeric(residuals(x, standardize=TRUE))
  zseries[is.na(zseries)] = 0
  n 		= length(zseries)
  lag.max = as.integer(10*log10(n))
  acfx 	= acf(zseries, lag.max = lag.max, plot = FALSE)
  clim0	= qnorm((1 + 0.95)/2)/sqrt(acfx$n.used)
  ylim 	= range(c(-clim0, clim0, as.numeric(acfx$acf)[-1]))
  clx 	= vector(mode = "character", length = lag.max)
  clx[which(as.numeric(acfx$acf)[-1]>=0)] = "green"
  clx[which(as.numeric(acfx$acf)[-1]<0)] = "blue"
  barplot(height = as.numeric(acfx$acf)[-1], names.arg = as.numeric(acfx$lag)[-1], ylim=1.2*ylim, col = clx,
          ylab = "ACF", xlab="lag", main = paste("ACF стандартизированных ошибок",
                                                 name,sep=','),
                                                 cex.main = 1.1)
  abline(h = c(clim0, -clim0), col = "red", lty = 4)
  abline(h = 0, col = "black", lty = 1)
  box()
  mtext(paste("GARCH model : ", vmodel), side = 4, adj = 0, padj=0, col = "pink", cex = 0.5)
  if(vmodel == "fGARCH"){
    mtext(paste("fGARCH submodel: ", x@model$modeldesc$vsubmodel, sep = ""), side = 4, adj = 0, padj = 1.5, col = "gray", cex = 0.5)
  }
  grid()
}