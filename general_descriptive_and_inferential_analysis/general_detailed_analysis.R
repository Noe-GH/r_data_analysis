library(MASS)
library(ggplot2)
library(gridExtra)
library(gtable)
library(grid)

setwd('~/programas_propios/r_projects_github/general_descriptive_and_inferential_analysis')

db = read.table('x_data.csv',sep=",",header=T)
#View(db)


# Análisis exploratorio -- Análisis exploratorio -- Análisis exploratorio -- Análisis exploratorio -- Análisis exploratorio -- 

# Inicia Descripción de los datos

# cat1
# Frecuencias absolutas
frec_cat1 = table(db$cat1)

# Frecuencias relativas
prop_cat1 = prop.table(frec_cat1)


# Tabla de frecuencias
frec_table_cat1 <- cbind(frec_cat1, cumsum(frec_cat1), round(prop_cat1,3), round(cumsum(prop_cat1), 3))


colnames(frec_table_cat1) <- c('fi', 'Fi', 'pi', 'Pi')
rownames(frec_table_cat1) <- c('Zero', 'One')

TSpecial <- ttheme_minimal(
  core=list(bg_params = list(fill = 'white', col='white'),
            fg_params=list(fontface='plain', fontfamily='serif', fontsize=12)),
  colhead=list(fg_params=list(col='black',
                              fontface='plain', fontfamily='serif', fontsize=12,
                              x=0.5, hjust=0.5)),
  rowhead=list(fg_params=list(col='black',
                              fontface='plain', fontfamily='serif', fontsize=12,
                              x=0, hjust=0)))


g = tableGrob(frec_table_cat1, theme=TSpecial)

# Línea superior de encabezados
g <- gtable_add_grob(g,
                     grobs = segmentsGrob(
                       x0 = unit(0,"npc"),
                       y0 = unit(1,"npc"),
                       x1 = unit(1,"npc"),
                       y1 = unit(1,"npc"),
                       gp = gpar(lwd = 1.0)),
                     t = 1, b = 1, r = 1, l = 5)


# Línea inferior de encabezados:
g <- gtable_add_grob(g,
                     grobs = segmentsGrob(
                       x0 = unit(0,"npc"),
                       y0 = unit(1,"npc"),
                       x1 = unit(1,"npc"),
                       y1 = unit(1,"npc"),
                       gp = gpar(lwd = 1.0)),
                     t = 2, b = 2, r = 2, l = 5)

# Línea inferior de última fila:
g <- gtable_add_grob(g,
                     grobs = segmentsGrob(
                       x0 = unit(0,"npc"),
                       y0 = unit(0,"npc"),
                       x1 = unit(1,"npc"),
                       y1 = unit(0,"npc"),
                       gp = gpar(lwd = 1.0)),
                     t = 3, b = 3, r = 1, l = 5)

# Mostrar tabla
grid.arrange(g)
# Guardar tabla
ggsave('tabla_cat1.jpeg', g, dpi=300)



# var1 y var2
png(file='var1_var2.png', width=1084, height=874, res=200)
par(mfrow=c(1, 2))
boxplot(db$var1, horizontal=F, main='var1', ylab='var1', col='white')
boxplot(db$var2, horizontal=F, main='var2', ylab='var2', col='white')
dev.off()
par(mfrow=c(1, 1))

# Coeficientes de asimetría y curtosis:
library(moments)
skewness(na.exclude(db$var1))
kurtosis(na.exclude(db$var1))

skewness(db$var2)
kurtosis(db$var2)

# Resumen de los 5 números:
five_n_general <- cbind(fivenum(db$var1), fivenum(db$var2))
colnames(five_n_general) = c('var1', 'var2')
rownames(five_n_general) = c('Mín', 'q1', 'q2', 'q3', 'Máx')

# Rango (amplitud) intercuartílico
fivenum(db$var1)[4]-fivenum(db$var1)[2]
fivenum(db$var2)[4]-fivenum(db$var2)[2]


g = tableGrob(five_n_general, theme=TSpecial)

# Línea superior de encabezados
g <- gtable_add_grob(g,
                     grobs = segmentsGrob( # line across the bottom
                       x0 = unit(0,"npc"),
                       y0 = unit(1,"npc"),
                       x1 = unit(1,"npc"),
                       y1 = unit(1,"npc"),
                       gp = gpar(lwd = 1.0)),
                     t = 1, b = 1, r = 1, l = 3)

# Línea inferior de encabezados:
g <- gtable_add_grob(g,
                     grobs = segmentsGrob( # line across the bottom
                       x0 = unit(0,"npc"),
                       y0 = unit(1,"npc"),
                       x1 = unit(1,"npc"),
                       y1 = unit(1,"npc"),
                       gp = gpar(lwd = 1.0)),
                     t = 2, b = 2, r = 2, l = 3)

# Línea inferior de la última fila
g <- gtable_add_grob(g,
                     grobs = segmentsGrob( # line across the bottom
                       x0 = unit(0,"npc"),
                       y0 = unit(0,"npc"),
                       x1 = unit(1,"npc"),
                       y1 = unit(0,"npc"),
                       gp = gpar(lwd = 1.0)),
                     t = 6, b = 6, r = 1, l = 3)

# Mostrar tabla
grid.arrange(g)
# Guardar tabla
ggsave('tabla_fnum_gral.jpeg', g, dpi=300)


# Comparación cat1 X var1 & cat1 X var2
png(file='comp_var1_var2.png', width=1084, height=874, res=200)
par(mfrow=c(1, 2))
boxplot(db$var1~db$cat1,col="white", main='var1', xlab='cat1', ylab='var1', names=c('0', '1'))
boxplot(db$var2~db$cat1,col="white", main='var2', xlab='cat1', ylab='var2', names=c('0', '1'))
dev.off()
par(mfrow=c(1, 1))

# Asimetría y curtosis:
# var1
skewness(na.exclude(db$var1[db$cat1==0]))
skewness(na.exclude(db$var1[db$cat1==1]))
kurtosis(na.exclude(db$var1[db$cat1==0]))
kurtosis(na.exclude(db$var1[db$cat1==1]))

#var2
skewness(na.exclude(db$var2[db$cat1==0]))
skewness(na.exclude(db$var2[db$cat1==1]))
kurtosis(na.exclude(db$var2[db$cat1==0]))
kurtosis(na.exclude(db$var2[db$cat1==1]))


# Medianas e IQR:
# var1
median(na.exclude(db$var1[db$cat1==0]))
median(na.exclude(db$var1[db$cat1==1]))
IQR(na.exclude(db$var1[db$cat1==0]))
IQR(na.exclude(db$var1[db$cat1==1]))

# var2
median(na.exclude(db$var2[db$cat1==0]))
median(na.exclude(db$var2[db$cat1==1]))
IQR(na.exclude(db$var2[db$cat1==0]))
IQR(na.exclude(db$var2[db$cat1==1]))


# Asociación entre var1 y var2

par(mfrow=c(1, 1))

png(file='cor_var1_var2.png', width=1084, height=874, res=200)
plot(db$var1, db$var2, xlab='var1', ylab='var2')
dev.off()

cov(db$var1, db$var2, use='complete.obs')


# Finaliza Descripción de los datos


# Estadística inferencial -- Estadística inferencial -- Estadística inferencial -- Estadística inferencial -- Estadística inferencial --

# Inicia ESTIMACIÓN PUNTUAL

y = na.exclude(db$var1)
length(y)

# Histograma de var1
#hist(y, pch=20,prob=F,main="", xlab='var1', ylab="Frecuencia absoluta")
#hist(y, pch=20,prob=T,main="", xlab='var1', ylab="Frecuencia relativa")

# Histograma de var2
#hist(db$var2, pch=20,prob=F,main="", breaks=seq(0, 10, 1), xlab='var2', ylab="Frecuencia absoluta")


# Modelo Exponencial
# Estimación por método de momentos:
lambdahat.MOM<- round(1/mean(y),digits=4); lambdahat.MOM

# MLE
fit_mle_exp<- fitdistr(y, densfun="exponential",rate=lambdahat.MOM); fit_mle_exp

# Histograma con el modelo:
#hist(y, pch=20,prob=T,main="", xlab='var1', ylab="Frecuencia relativa")
#curve(dexp(x,fit_mle_exp$estimate),col="red",lwd=2,add=T)

#hist(y, pch=20,prob=T,main="", breaks=seq(0,36,2), xlab='var1', ylim=c(0, 0.12), ylab="Frecuencia relativa")
#curve(dexp(x,fit_mle_exp$estimate),col="red",lwd=2,add=T)

#hist(y, pch=20,prob=T,main="", breaks=seq(1,36,3), xlim=c(0.001, 36.001), xlab='var1', ylim=c(0, 0.12), ylab="Frecuencia relativa")
#curve(dexp(x,fit_mle_exp$estimate),col="red",lwd=2,add=T)


# Modelo Gamma

n= length(y)
num<- n*(mean(y)^2)
denom<- sum(y^2)-(n*mean(y)^2)
alfahat.MOM<- num/denom ; alfahat.MOM
betahat.MOM<- mean(y)/alfahat.MOM; 1/betahat.MOM

# MLE
fit_mle_gamma<-fitdistr(y, densfun="gamma")

# Histograma con el modelo:
#hist(y, pch=20,prob=T,main="", xlab='var1', ylim=c(0, 0.12), ylab="Frecuencia relativa")
#curve(dgamma(x, fit_mle_gamma$estimate[1], fit_mle_gamma$estimate[2]),col="red",lwd=2,add=T)

#hist(y, pch=20,prob=T,main="", breaks=seq(0,36,2), xlab='var1', ylim=c(0, 0.12), ylab="Frecuencia relativa")
#curve(dgamma(x, fit_mle_gamma$estimate[1], fit_mle_gamma$estimate[2]),col="red",lwd=2,add=T)

#hist(y, pch=20,prob=T,main="", breaks=seq(1,36,3), xlim=c(0.001, 36.001), xlab='var1', ylim=c(0, 0.12), ylab="Frecuencia relativa")
#curve(dgamma(x, fit_mle_gamma$estimate[1], fit_mle_gamma$estimate[2]),col="red",lwd=2,add=T)




# Modelo Weibull

# Estimación por el método de momentos:
alfahat.MOM <- 2.348 ; alfahat.MOM
betahat.MOM <- mean(y)/gamma(1+1/alfahat.MOM) ; betahat.MOM

# MLE
fit_mle_wei<- fitdistr(y, densfun="weibull", start= list(shape=alfahat.MOM, scale=betahat.MOM), lower = c(-Inf, 0))

#hist(y, pch=20,prob=T,main="", xlab='var1', ylim=c(0, 0.12), ylab="Frecuencia relativa")
#curve(dweibull(x,shape=fit_mle_wei$estimate[1], scale=fit_mle_wei$estimate[2]),col="red",lwd=2,add=T)

#hist(y, pch=20,prob=T,main="", breaks=seq(0,36,2), xlab='var1', ylim=c(0, 0.12), ylab="Frecuencia relativa")
#curve(dweibull(x,shape=fit_mle_wei$estimate[1], scale=fit_mle_wei$estimate[2]),col="red",lwd=2,add=T)

#hist(y, pch=20,prob=T,main="", breaks=seq(1,36,3), xlim=c(0.001, 36.001), xlab='var1', ylim=c(0, 0.12), ylab="Frecuencia relativa")
#curve(dweibull(x,shape=fit_mle_wei$estimate[1], scale=fit_mle_wei$estimate[2]),col="red",lwd=2,add=T)


# Histograma con los distintos modelos

#hist(y, pch=20,prob=T,main="", xlab='var1', ylim=c(0, 0.12), ylab="Frecuencia relativa")
#curve(dexp(x,fit_mle_exp$estimate),col="green",lwd=2,add=T)
#curve(dgamma(x, fit_mle_gamma$estimate[1], fit_mle_gamma$estimate[2]),col="red",lwd=2,add=T)
#curve(dweibull(x,shape=fit_mle_wei$estimate[1], scale=fit_mle_wei$estimate[2]),col="blue",lwd=2,add=T)

#hist(y, pch=20,prob=T,main="", breaks=seq(5,37,2), xlab='var1', ylim=c(0, 0.12), ylab="Frecuencia relativa")
#curve(dexp(x,fit_mle_exp$estimate),col="green",lwd=2,add=T)
#curve(dgamma(x, fit_mle_gamma$estimate[1], fit_mle_gamma$estimate[2]),col="red",lwd=2,add=T)
#curve(dweibull(x,shape=fit_mle_wei$estimate[1], scale=fit_mle_wei$estimate[2]),col="blue",lwd=2,add=T)

#hist(y, pch=20,prob=T,main="", breaks=seq(1,36,3), xlab='var1', ylim=c(0, 0.14), ylab="Frecuencia relativa")
#curve(dexp(x,fit_mle_exp$estimate),col="green",lwd=2,add=T)
#curve(dgamma(x, fit_mle_gamma$estimate[1], fit_mle_gamma$estimate[2]),col="red",lwd=2,add=T)
#curve(dweibull(x,shape=fit_mle_wei$estimate[1], scale=fit_mle_wei$estimate[2]),col="blue",lwd=2,add=T)

#hist(y, pch=20,prob=T,main="", breaks=seq(0,35,3), xlab='var1', ylim=c(0, 0.14), ylab="Frecuencia relativa")
#curve(dexp(x,fit_mle_exp$estimate),col="green",lwd=2,add=T)
#curve(dgamma(x, fit_mle_gamma$estimate[1], fit_mle_gamma$estimate[2]),col="red",lwd=2,add=T)
#curve(dweibull(x,shape=fit_mle_wei$estimate[1], scale=fit_mle_wei$estimate[2]),col="blue",lwd=2,add=T)

# Gráfico elegido para el reporte
hist(y, pch=20,prob=T,main="", breaks=seq(1,36,3), xlim=c(0.001, 36.001), xlab='var1', ylim=c(0, 0.12), ylab="Frecuencia relativa")
curve(dexp(x,fit_mle_exp$estimate),col="green",lwd=2,add=T)
curve(dgamma(x, fit_mle_gamma$estimate[1], fit_mle_gamma$estimate[2]),col="red",lwd=2,add=T)
curve(dweibull(x,shape=fit_mle_wei$estimate[1], scale=fit_mle_wei$estimate[2]),col="blue",lwd=2,add=T)
legend(20, 0.11, legend=c('X~Exp(0.099)', 'X~Gamma(7.285, 0.724) (FD)', 'X~Wei(2.348921, 11.334246) (FD)'),
       col=c('green', 'red', 'blue'), lwd=2, cex=0.55)


png(file='modelos_prob_proyecto.png', width=1084, height=874, res=200)
hist(y, pch=20,prob=T,main="", breaks=seq(1,36,3), xlim=c(0.001, 36.001), xlab='var1', ylim=c(0, 0.12), ylab="Frecuencia relativa")
curve(dexp(x,fit_mle_exp$estimate),col="green",lwd=2,add=T)
curve(dgamma(x, fit_mle_gamma$estimate[1], fit_mle_gamma$estimate[2]),col="red",lwd=2,add=T)
curve(dweibull(x,shape=fit_mle_wei$estimate[1], scale=fit_mle_wei$estimate[2]),col="blue",lwd=2,add=T)
legend(17, 0.10, legend=c('X~Exp(0.099)', 'X~Gamma(7.285, 0.724) (FD)', 'X~Wei(2.348921, 11.334246) (FD)'),
       col=c('green', 'red', 'blue'), lwd=2, cex=0.65)
dev.off()


# Comparación de modelos con AIC y BIC:
k=1
n= length(y)
AIC_exp= 2*k-(2*fit_mle_exp$loglik); AIC_exp
BIC_exp= log(n)*k-(2*fit_mle_exp$loglik); BIC_exp
#[1] 319.6582
#[1] 321.5294

k=2
AIC_gamma= 2*k-(2*fit_mle_gamma$loglik); AIC_gamma
BIC_gamma= log(n)*k-(2*fit_mle_gamma$loglik); BIC_gamma
#[1] 262.0029
#[1] 265.7454

k=2
AIC_fd= 2*k-(2*fit_mle_wei$loglik); AIC_fd
BIC_fd= log(n)*k-(2*fit_mle_wei$loglik); BIC_fd
#[1] 275.5765
#[1] 279.3189


# Tabla AIC y BIC

tabla_AIC_BIC <- matrix(cbind(round(AIC_exp,3), round(BIC_exp, 3),
                              round(AIC_gamma, 3), round(BIC_gamma, 3),
                              round(AIC_fd, 3), round(BIC_fd, 3)),
                        byrow=T, nrow=3, ncol=2)

colnames(tabla_AIC_BIC) <- c('AIC', 'BIC')
rownames(tabla_AIC_BIC) <- c('Modelo Exponencial', 'Modelo Gamma', 'Modelo Weibull')


g = tableGrob(tabla_AIC_BIC, theme=TSpecial)

# Línea superior de encabezados
g <- gtable_add_grob(g,
                     grobs = segmentsGrob(
                       x0 = unit(0,"npc"),
                       y0 = unit(1,"npc"),
                       x1 = unit(1,"npc"),
                       y1 = unit(1,"npc"),
                       gp = gpar(lwd = 1.0)),
                     t = 1, b = 1, r = 1, l = 3)


# Línea inferior de encabezados:
g <- gtable_add_grob(g,
                     grobs = segmentsGrob(
                       x0 = unit(0,"npc"),
                       y0 = unit(1,"npc"),
                       x1 = unit(1,"npc"),
                       y1 = unit(1,"npc"),
                       gp = gpar(lwd = 1.0)),
                     t = 2, b = 2, r = 2, l = 3)

# Línea inferior de última fila:
g <- gtable_add_grob(g,
                     grobs = segmentsGrob(
                       x0 = unit(0,"npc"),
                       y0 = unit(0,"npc"),
                       x1 = unit(1,"npc"),
                       y1 = unit(0,"npc"),
                       gp = gpar(lwd = 1.0)),
                     t = 4, b = 4, r = 1, l = 3)

# Mostrar tabla
grid.arrange(g)
# Guardar tabla
ggsave('tabla_AIC_BIC.jpeg', g, dpi=300)






# Proporción muestral de cat1 Zero:
length(db$cat1[db$cat1==0])/length(db$cat1)
# Proporción muestral de cat1 One:
length(db$cat1[db$cat1==1])/length(db$cat1)





# var1:
x = na.exclude(db$var1)
mean(x)
var(x)

# var2:
mean(db$var2)
var(db$var2)

# Correlación:
cor(db$var1, db$var2, use='complete.obs')
cor(db$var1, db$var2, use='pairwise')



# Finaliza ESTIMACIÓN PUNTUAL



# Inicia ESTIMACIÓN POR INTERVALOS

y <- na.exclude(db$var1)

# Evaluar normalidad en variable original
# Método gráfico:
qqnorm(y,pch=16, main='', xlab='Cuantiles de la muestra', ylab='Cuantiles teóricos')
qqline(y,col="red",lwd=2)
# Método estadístico:
library(nortest)
lillie.test(y)
# D = 0.14221, p-value = 0.01637

#install.packages('bimixt')

# Transformación de los datos:
library(forecast)
lambda = BoxCox.lambda(y,method="loglik",lower=-10,upper=10);lambda

trans_y = BoxCox(y, lambda)
hist(trans_y)

library(bimixt) # Para la transformación inversa

# Evaluar normalidad en variable transformada
# Método gráfico:
qqnorm(trans_y,pch=16, main='', xlab='Cuantiles de la muestra', ylab='Cuantiles teóricos')
qqline(trans_y,col="red",lwd=2)
# Método estadístico:
lillie.test(trans_y)
# D = 0.1162, p-value = 0.1078

# Grafico de comparación de la normalidad de los datos antes y después de la transformación:
png(file='box_cox.png', width=1084, height=874, res=200)
par(mfrow=c(1,2))
qqnorm(y,pch=16, main='D. Originales', xlab='Cuantiles de la muestra', ylab='Cuantiles teóricos')
qqline(y,col="red",lwd=2)

qqnorm(trans_y,pch=16, main='D. Transformados', xlab='Cuantiles de la muestra', ylab='Cuantiles teóricos')
qqline(trans_y,col="red",lwd=2)
par(mfrow=c(1,1))
dev.off()



# Datos para estimación por intervalos
xbar<- mean(trans_y)
s<- sd(trans_y)
n<- length(trans_y)

# Estimación por intervalos para la media

#IC 83%
ee_83<- abs(qt(0.17/2,df=n-1,lower.tail = F)*s/sqrt(n))
boxcox.inv(cbind(xbar-ee_83,xbar+ee_83), lambda)
#         [,1]     [,2]
#[1,] 8.448633 9.701664

#IC 97%
ee_97<- abs(qt(0.03/2,df=n-1,lower.tail = F)*s/sqrt(n))
boxcox.inv(cbind(xbar-ee_97,xbar+ee_97), lambda)
#         [,1]     [,2]
# [1,] 8.11956 10.14078


# Estimación por intervalos para la varianza
s2 = boxcox.inv(var(trans_y), lambda^2)

#boxcox.inv(sd(trans_y), lambda)^2
#boxcox.inv(var(trans_y), lambda^2)
#boxcox.inv(sqrt(var(trans_y)),lambda)
#s2 = var(trans_y)
#boxcox.inv(s2, lambda)

# IC 83%
cbind(((n-1)*s2)/qchisq(0.17/2,df=(n-1),lower.tail = F), ((n-1)*s2)/qchisq(0.17/2,df=(n-1),lower.tail = T))
#           [,1]     [,2]
# [1,] 0.7793959 1.379231


#IC 97%
cbind(((n-1)*s2)/qchisq(0.03/2,df=(n-1),lower.tail = F), ((n-1)*s2)/qchisq(0.03/2,df=(n-1),lower.tail = T))
#           [,1]     [,2]
# [1,] 0.6727842 1.664181



muj = na.exclude(trans_y[db$cat1==0])
hom = na.exclude(trans_y[db$cat1==1])

#hist(muj, pch=20,prob=F,main="", breaks = seq(0, 14, 1), xlab='var2', ylab="Frecuencia absoluta")
#hist(hom, pch=20,prob=F,main="", breaks = seq(0, 35, 2), xlab='var2', ylab="Frecuencia absoluta")

n1 = length(muj)
n2 = length(hom)


# Estimación por intervalos Comparación de varianzas
# IC 83%
i.c_cocvar_83<- c((var(muj)/var(hom))*qf(0.915,df1=n2-1,df2=n1-1,lower.tail = F),
                  (var(muj)/var(hom))*qf(0.085,df1=n2-1,df2=n1-1,lower.tail = F))
# [1] 0.5108966 1.9509586

# IC 97%
i.c_cocvar_97<- c((var(muj)/var(hom))*qf(0.985,df1=n2-1,df2=n1-1,lower.tail = F),
                  (var(muj)/var(hom))*qf(0.015,df1=n2-1,df2=n1-1,lower.tail = F))
# [1] 0.3601367 3.0907625

# Estimación por intervalos Comparación de medias

# Varianza común
s2pooled<- (((n1-1)*var(muj))+((n2-1)*var(hom)))/(n1+n2-2); s2pooled
spooled<- sqrt(s2pooled)

dif<- mean(muj)-mean(hom)

# IC 83%
ee_difmed_83<- qt(0.17/2,df=n1+n2-2,lower.tail = F)*spooled*sqrt((1/n1)+(1/n2))
i.c_difmedias_83<- c(dif-ee_difmed_83,dif+ee_difmed_83); i.c_difmedias_83
# [1] -0.10727892 -0.02810222


# IC 97%
ee_difmed_97<- qt(0.03/2,df=n1+n2-2,lower.tail = F)*spooled*sqrt((1/n1)+(1/n2))
i.c_difmedias_97<- c(dif-ee_difmed_97,dif+ee_difmed_97); i.c_difmedias_97
# [1] -0.131287878 -0.004093263



# Finaliza ESTIMACIÓN POR INTERVALOS



# Inicia PRUEBAS DE HIPÓTESIS PARAMÉTRICAS

mu = BoxCox(6, lambda)
sigma = sd(trans_y)

#install.packages('PASWR')
library(PASWR)

# Definición de zonas críticas:
zcrit1<- qnorm(0.03/2,mean=0,sd=1,lower.tail=T)
zcrit2<- qnorm(0.03/2,mean=0,sd=1,lower.tail=F)

# Región crítica:
zcrit1*sigma/sqrt(length(trans_y))

mean(trans_y)-mu >= zcrit1*sigma/sqrt(length(trans_y))

z_trans_y<- z.test(trans_y,alternative="two.sided",mu=mu,sigma.x=sigma,conf.level=0.97)
str(z_trans_y)
# p-value < 2.2e-16
# Se rechaza la hipótesis nula con un 97% de confianza. La media es distinta de 6
boxcox.inv(c(1.193182, 1.250617), lambda)
# 8.145256 10.104293
boxcox.inv(sigma, lambda)
# [1] 1.098892




muj = na.exclude(trans_y[db$cat1==0])
hom = na.exclude(trans_y[db$cat1==1])

boxplot(muj, hom, horizontal = T, main='',
        names= c("Zero","One"))

# Como no se conoce la varianza poblacional, se usa la t.

# Comparación de medias:
# 83%
t_mu_muj_hom_83<- t.test(muj,hom,alternative='two.sided',mu=0,paired=F,conf.level = 0.83)
t_mu_muj_hom_83$statistic
t_mu_muj_hom_83$p.value
t_mu_muj_hom_83$conf.int
# [1] -0.10726837 -0.02811277


# 97%
t_mu_muj_hom_97<- t.test(muj, hom,alternative='two.sided',mu=0,paired=F,conf.level = 0.97)
t_mu_muj_hom_97$statistic
t_mu_muj_hom_97$p.value
t_mu_muj_hom_97$conf.int
# [1] -0.132389870 -0.002991271


# Comparación de varianzas:
# 83%
prueba_var_hom_muj<- var.test(muj,hom,ratio=1,alternative="two.sided",conf.level = 0.83)
prueba_var_hom_muj$p.value
prueba_var_hom_muj$conf.int
# [1] 0.5108966 1.9509586

# 97%
prueba_var_hom_muj<- var.test(muj,hom,ratio=1,alternative="two.sided",conf.level = 0.97)
prueba_var_hom_muj$p.value
prueba_var_hom_muj$conf.int
# [1] 0.3601367 3.0907625


# Finaliza PRUEBAS DE HIPÓTESIS PARAMÉTRICAS


