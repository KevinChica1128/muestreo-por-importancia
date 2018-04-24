#Muestreo por importancia Ejemplo:
x<-rnorm(197,0.6268,0.002649^2)
x11()
hist(x,freq=F)
lines(density(x))