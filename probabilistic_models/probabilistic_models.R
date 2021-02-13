library(MASS)

setwd('YOUR_DIR')

lluvias= read.table("lluvias.txt",sep="\t",row.names = 1,header=T)
attach(lluvias)
y= Precipitacion

hist(lluvias$Precipitacion,breaks=seq(0,2400,300),main= "Precipitacion anual en Mexico (2017)",
     xlab="Precipitacion (mm)")

# Tiene un perfil asimétrico a la derecha.

# De acuerdo al perfil del histograma parece que el modelo de probabilidad
# adecuado puede ser:

# Se proponen las siguientes distribuciones (que tienen asimetría a la derecha):
# 1. Weibull de parametros alfa y beta.
# 2. Exponencial de parametro "lambda."
# 3. Gamma de parametros alfa y beta.
# El preferido de entrada es Weibull.


# Modelo Weibull -- Modelo Weibull -- Modelo Weibull -- Modelo Weibull -- Modelo Weibull -- Modelo Weibull -- Modelo Weibull -- Modelo Weibull --

# Función de verosimilitud
lik_wei<- function(theta){
  alfa = theta[1]
  beta = theta[2]
  l=prod( alfa/beta^alfa * y^(alfa-1) * exp(- (y/beta)^alfa) )
  
  return(-l)   # para maximizar
}

alfahat.MOM <- 1.967 ; alfahat.MOM
betahat.MOM <- mean(y)/gamma(1+1/alfahat.MOM) ; betahat.MOM
#[1] 1.967
#[1] 1002.155

fit_mle_wei_p<- optim(c(alfahat.MOM, betahat.MOM), lik_wei); fit_mle_wei_p
# $value parece ser el valor en el que se maximiza la función
#$par
#[1]    1.967 1002.155

#fit_mle_wei_p<- nlm(lik_wei, c(alfahat.MOM, betahat.MOM)); str(fit_mle_wei_p)

# Histograma con modelo MLE usando la función de verosimilitud creada
hist(y, pch=20,prob=T,main="", xlab='Precipitación (mm)', ylim=c(0,1e-03), ylab="Frecuencia relativa")
curve(dweibull(x,shape=fit_mle_wei_p$par[1], scale=fit_mle_wei_p$par[2]),col="red",lwd=2,add=T) # Máxima verosimilitud
legend(1300, 9e-04, legend=c('X~Wei(1.967, 1002.155)'), col=c("red"), lwd=2, cex=0.55)

# Guardar histograma
png(file='precipitacion_hist_wei_mle.png', width=1084, height=874, res=200)
hist(y, pch=20,prob=T,main="", xlab='Precipitación (mm)', ylim=c(0,1e-03), ylab="Frecuencia relativa")
curve(dweibull(x,shape=fit_mle_wei_p$par[1], scale=fit_mle_wei_p$par[2]),col="red",lwd=2,add=T) # Máxima verosimilitud
legend(1300, 9e-04, legend=c('X~Wei(1.967, 1002.155)'), col=c("red"), lwd=2, cex=0.55)
dev.off()


# Máxima verosimilitud usando la función fitdistr() de R:
fit_mle_wei<- fitdistr(y, densfun="weibull", start= list(shape=alfahat.MOM, scale=betahat.MOM), lower = c(-Inf, 0)); str(fit_mle_wei)

# Histograma con modelo fitdistr() y MLE propio:
hist(y, pch=20,prob=T,main="", xlab='Precipitación (mm)', ylim=c(0,1e-03), ylab="Frecuencia relativa")
curve(dweibull(x,shape=fit_mle_wei$estimate[1], scale=fit_mle_wei$estimate[2]),col="green",lwd=2,add=T) # "fitdistr" fit
curve(dweibull(x,shape=fit_mle_wei_p$par[1], scale=fit_mle_wei_p$par[2]),col="red",lwd=2,add=T) # Máxima verosimilitud
legend(1300, 9e-04, legend=c('X~Wei(1.967, 1002.155) (MLE)', 'X~Wei(1.972, 1005.316) (FD)'), col=c('red', 'green'), lwd=c(2, 2), cex=0.55)

# Guardar histograma
png(file='precipitacion_hist_wei_mle&fd.png', width=1084, height=874, res=200)
hist(y, pch=20,prob=T,main="", xlab='Precipitación (mm)', ylim=c(0,1e-03), ylab="Frecuencia relativa")
curve(dweibull(x,shape=fit_mle_wei$estimate[1], scale=fit_mle_wei$estimate[2]),col="green",lwd=2,add=T) # "fitdistr" fit
curve(dweibull(x,shape=fit_mle_wei_p$par[1], scale=fit_mle_wei_p$par[2]),col="red",lwd=2,add=T) # Máxima verosimilitud
legend(1300, 9e-04, legend=c('X~Wei(1.967, 1002.155) (MLE)', 'X~Wei(1.972, 1005.316) (FD)'), col=c('red', 'green'), lwd=c(2, 2), cex=0.55)
dev.off()

# Para comparar ambos modelos, se requiere la función log verosimilitud con el modelo propio
# Función log verosimilitud
loglik_wei<- function(theta){
  alfa = theta[1]
  beta = theta[2]
  ybar = mean(y)
  n = length(y)
  logybar= mean(log(y))
  l= n*log(alfa)-n*log(beta^alfa)-((n*ybar)^alfa/beta^alfa)+n*(alfa-1)*logybar
  
  return(-l)   # para maximizar
}

alfahat.MOM <- 1.96 ; alfahat.MOM
betahat.MOM <- mean(y)/gamma(1+1/alfahat.MOM) ; betahat.MOM

# Maximizar log verosimilitud
loglik_mle3<- optim(c(alfahat.MOM, betahat.MOM), loglik_wei); loglik_mle3


# Comparar modelos: propio y fitdistr()
k=2   # numero de parametros a estimar
n= length(y)
AIC_mle3= 2*k-(2*loglik_mle3$value*-1); AIC_mle3
BIC_mle3= log(n)*k-(2*loglik_mle3$value*-1); BIC_mle3
#[1] 417.4999
#[1] 420.4313

k=2
AIC_wei= 2*k-(2*fit_mle_wei$loglik); AIC_wei
BIC_wei= log(n)*k-(2*fit_mle_wei$loglik); BIC_wei
#[1] 483.8237
#[1] 486.7552


# MODELO EXPONENCIAL -- MODELO EXPONENCIAL -- MODELO EXPONENCIAL -- MODELO EXPONENCIAL -- MODELO EXPONENCIAL -- MODELO EXPONENCIAL -- MODELO EXPONENCIAL --

# Método de momentos

# Se sabe de la teoria que: lambda.hat=1/xbar

lambdahat.MOM<- round(1/mean(y),digits=4); lambdahat.MOM

# Grafica del modelo exponencial
hist(y, pch=20,prob=TRUE,main="")
curve(dexp(x,lambdahat.MOM),col="blue",lwd=2,add=T)   # momentos


# Método MLE: funcion "fitdistr"
fit_mle_exp<- fitdistr(y, densfun="exponential",rate=lambdahat.MOM); fit_mle_exp

# Histograma con el modelo exponencial generado con MLE
hist(y, pch=20,prob=TRUE,main="")
curve(dexp(x,fit_mle_exp$estimate),col="red",lwd=2,add=T) # "fitdistr" fit


# Modelo gamma -- Modelo gamma -- Modelo gamma -- Modelo gamma -- Modelo gamma -- Modelo gamma -- Modelo gamma -- Modelo gamma -- Modelo gamma --

# Método de momentos

n= length(y)
num<- n*(mean(y)^2)
denom<- sum(y^2)-(n*mean(y)^2)
alfahat.MOM<- num/denom ; alfahat.MOM
betahat.MOM<- mean(y)/alfahat.MOM; 1/betahat.MOM
# Debe usarse el recíproco

# Metodo de MLE a partir de fitdistr()
fit_mle_gamma<-fitdistr(y, densfun="gamma")

hist(Precipitacion, pch=20, breaks=15, prob=TRUE,main="")
curve(dgamma(x, fit_mle_gamma$estimate[1], fit_mle_gamma$estimate[2]),col="red",lwd=2,add=T)



# Comparación de modelos distintos:
# k es el número de parámetros a estimar
k=1
n= length(y)
AIC_exp= 2*k-(2*fit_mle_exp$loglik); AIC_exp
BIC_exp= log(n)*k-(2*fit_mle_exp$loglik); BIC_exp
#[1] 500.5257
#[1] 501.9915

k=2
AIC_gamma= 2*k-(2*fit_mle_gamma$loglik); AIC_gamma
BIC_gamma= log(n)*k-(2*fit_mle_gamma$loglik); BIC_gamma
#[1] 482.3427
#[1] 485.2742


k=2
AIC_wei= 2*k-(2*fit_mle_wei$loglik); AIC_wei
BIC_wei= log(n)*k-(2*fit_mle_wei$loglik); BIC_wei
#[1] 483.8237
#[1] 486.7552


hist(y, pch=20,prob=TRUE,main="")
curve(dweibull(x,shape=fit_mle_wei$estimate[1], scale=fit_mle_wei$estimate[2]),col="green",lwd=2,add=T)
curve(dexp(x,fit_mle_exp$estimate),col="blue",lwd=2,add=T)
curve(dgamma(x, fit_mle_gamma$estimate[1], fit_mle_gamma$estimate[2]),col="red",lwd=2,add=T)

