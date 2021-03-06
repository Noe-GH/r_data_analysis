library(ggplot2)
library(gridExtra)
library(gtable)
library(grid)

setwd('DIR')
load("RDATA FILE")

save.image("RDATA FILE")

datos <- read.table('empleados2.txt', header=T, row.names=1)
View(datos)

# -- TURNO -- -- TURNO -- -- TURNO -- -- TURNO -- -- TURNO -- -- TURNO -- -- TURNO -- -- TURNO -- -- TURNO -- -- TURNO -- -- TURNO --
# Frecuencias absolutas
frec_turno = table(datos$turno)

# Frecuencias relativas
prop_turno = prop.table(frec_turno)

# Tabla de frecuencias
frec_table_turno <- cbind(frec_turno, cumsum(frec_turno), prop_turno, cumsum(prop_turno))

colnames(frec_table_turno) <- c('fi', 'Fi', 'pi', 'Pi')
rownames(frec_table_turno) <- c('Matutino', 'Nocturno', 'Vespertino')

frec_table_turno

TSpecial <- ttheme_minimal(
  core=list(bg_params = list(fill = 'white', col='white'),
            fg_params=list(fontface='plain', fontfamily='serif', fontsize=12)),
  colhead=list(fg_params=list(col='black',
                              fontface='plain', fontfamily='serif', fontsize=12,
                              x=0.5, hjust=0.5)),
  rowhead=list(fg_params=list(col='black',
                              fontface='plain', fontfamily='serif', fontsize=12,
                              x=0, hjust=0)))

g = tableGrob(frec_table_turno, theme=TSpecial)

# L�nea superior de encabezados
g <- gtable_add_grob(g,
                     grobs = segmentsGrob(
                       x0 = unit(0,"npc"),
                       y0 = unit(1,"npc"),
                       x1 = unit(1,"npc"),
                       y1 = unit(1,"npc"),
                       gp = gpar(lwd = 1.0)),
                     t = 1, b = 1, r = 1, l = 5)


# L�nea inferior de encabezados:
g <- gtable_add_grob(g,
                     grobs = segmentsGrob(
                       x0 = unit(0,"npc"),
                       y0 = unit(1,"npc"),
                       x1 = unit(1,"npc"),
                       y1 = unit(1,"npc"),
                       gp = gpar(lwd = 1.0)),
                     t = 2, b = 2, r = 2, l = 5)

# L�nea inferior de �ltima fila:
g <- gtable_add_grob(g,
                     grobs = segmentsGrob(
                       x0 = unit(0,"npc"),
                       y0 = unit(0,"npc"),
                       x1 = unit(1,"npc"),
                       y1 = unit(0,"npc"),
                       gp = gpar(lwd = 1.0)),
                     t = 4, b = 4, r = 1, l = 5)

# Mostrar tabla
grid.arrange(g)

# Guardar tabla
ggsave('table1.jpeg', g, dpi=300)


# Gr�fico de barras Frecuencias relativas
barplot(prop_turno, names.arg=c('Matutino','Nocturno','Vespertino'), col='black',
                             ylab='Frecuencia relativa', ylim=c(0,0.6), xlab='Turno', xpd=T)

# Guardar el gr�fico:
png(file='fig2.png', width=1084, height=874, res=200)
barplot(prop_turno, names.arg=c('Matutino','Nocturno','Vespertino'), col='black',
        ylab='Frecuencia relativa', ylim=c(0,0.6), xlab='Turno', xpd=T)
dev.off()



# --SALARIO X TURNO----SALARIO X TURNO----SALARIO X TURNO----SALARIO X TURNO----SALARIO X TURNO----SALARIO X TURNO----SALARIO X TURNO--
# Boxplot
boxplot(datos$salario~datos$turno,col="white", xlab='Turno', ylab='Salario (d�lares)', names=c('Matutino','Nocturno','Vespertino'))

# Guardar Boxplot
png(file='fig3.png', width=1084, height=874, res=200)
boxplot(datos$salario~datos$turno,col="white", xlab='Turno', ylab='Salario (d�lares)', names=c('Matutino','Nocturno','Vespertino'))
dev.off()

# Estad�sticos de resumen:
matutino_salarios <- datos[datos$turno=='matutino',]$salario
nocturno_salarios <- datos[datos$turno=='nocturno',]$salario
vespertino_salarios <- datos[datos$turno=='vespertino',]$salario

# Resumen de los 5 n�meros
resumen_5num_salario_turno = cbind(fivenum(matutino_salarios), fivenum(nocturno_salarios), fivenum(vespertino_salarios))

colnames(resumen_5num_salario_turno) = c('Matutino', 'Nocturno', 'Vespertino')
rownames(resumen_5num_salario_turno) = c('M�n', 'q1', 'q2', 'q3', 'M�x')

g = tableGrob(resumen_5num_salario_turno, theme=TSpecial)

# L�nea superior de encabezados
g <- gtable_add_grob(g,
                     grobs = segmentsGrob( # line across the bottom
                       x0 = unit(0,"npc"),
                       y0 = unit(1,"npc"),
                       x1 = unit(1,"npc"),
                       y1 = unit(1,"npc"),
                       gp = gpar(lwd = 1.0)),
                     t = 1, b = 1, r = 1, l = 4)

# L�nea inferior de encabezados:
g <- gtable_add_grob(g,
                     grobs = segmentsGrob( # line across the bottom
                       x0 = unit(0,"npc"),
                       y0 = unit(1,"npc"),
                       x1 = unit(1,"npc"),
                       y1 = unit(1,"npc"),
                       gp = gpar(lwd = 1.0)),
                     t = 2, b = 2, r = 2, l = 4)

# L�nea inferior de la �ltima fila
g <- gtable_add_grob(g,
                     grobs = segmentsGrob( # line across the bottom
                       x0 = unit(0,"npc"),
                       y0 = unit(0,"npc"),
                       x1 = unit(1,"npc"),
                       y1 = unit(0,"npc"),
                       gp = gpar(lwd = 1.0)),
                     t = 6, b = 6, r = 1, l = 4)

# Mostrar tabla
grid.arrange(g)

# Guardar tabla
ggsave('table2.jpeg', g, dpi=300)

# Rango (amplitud) intercuart�lico
fivenum(matutino_salarios)[4]-fivenum(matutino_salarios)[2]
fivenum(nocturno_salarios)[4]-fivenum(nocturno_salarios)[2]
fivenum(vespertino_salarios)[4]-fivenum(vespertino_salarios)[2]


# Coeficiente de asimetr�a y coeficiente de curtosis:

salarios_por_turno <- list('matutino'=matutino_salarios, 'nocturno'=nocturno_salarios, 'vespertino'=vespertino_salarios)

asim_salario = c()
curtosis_salario = c()

for(n in names(salarios_por_turno)){
  salarios = salarios_por_turno[[n]]

  asim_salario <- c(asim_salario, round(sum((salarios-mean(salarios))^3)/(length(salarios)-1) / sd(salarios)^3, digits=3))
  curtosis_salario <- c(curtosis_salario, round(sum((salarios-mean(salarios))^4)/(length(salarios)-1) / sd(salarios)^4, digits=3))
}

asim_curtosis_salario_turno <- cbind(asim_salario, curtosis_salario)
colnames(asim_curtosis_salario_turno) <- c('Asimetr�a', 'Curtosis')
rownames(asim_curtosis_salario_turno) <- c('Matutino','Nocturno','Vespertino')

g = tableGrob(asim_curtosis_salario_turno, theme=TSpecial)

# L�nea superior de encabezados
g <- gtable_add_grob(g,
                     grobs = segmentsGrob(
                       x0 = unit(0,"npc"),
                       y0 = unit(1,"npc"),
                       x1 = unit(1,"npc"),
                       y1 = unit(1,"npc"),
                       gp = gpar(lwd = 1.0)),
                     t = 1, b = 1, r = 1, l = 3)


# L�nea inferior de encabezados:
g <- gtable_add_grob(g,
                     grobs = segmentsGrob(
                       x0 = unit(0,"npc"),
                       y0 = unit(1,"npc"),
                       x1 = unit(1,"npc"),
                       y1 = unit(1,"npc"),
                       gp = gpar(lwd = 1.0)),
                     t = 2, b = 2, r = 2, l = 3)

# L�nea inferior de la �ltima columna:
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
ggsave('table3.jpeg', g, dpi=300)



# -- ANTIG�EDAD X SALARIO -- -- ANTIG�EDAD X SALARIO -- -- ANTIG�EDAD X SALARIO -- -- ANTIG�EDAD X SALARIO -- -- ANTIG�EDAD X SALARIO --
# Diagrama de dispersi�n
plot(datos$antiguedad,datos$salario, bg='blue', xlab='Antig�edad (A�os)', ylab='Salario (D�lares)')

# Guardar diagrama de dispersi�n
png(file='fig4.png', width=1084, height=874, res=200)
plot(datos$antiguedad,datos$salario, bg='blue', xlab='Antig�edad (A�os)', ylab='Salario (D�lares)')
dev.off()

# Covarianza
covarianza <- function(x, y){
  errors=0
  n = length(x)
  mean_x = mean(x)
  mean_y = mean(y)
  for (i in 1:n){
    errors <- errors + (x[i]-mean_x) * (y[i]-mean_y)
  }
  return(errors/(n-1))
}

covarianza(datos$antiguedad, datos$salario)

# Coeficiente de correlaci�n de Pearson:
covarianza(datos$antiguedad, datos$salario) / (sd(datos$antiguedad)*sd(datos$salario))
cor(datos$antiguedad, datos$salario, method='spearman')

# Asimetr�a y curtosis
skewness(datos$antiguedad)
kurtosis(datos$antiguedad)

skewness(datos$salario)
kurtosis(datos$salario)
