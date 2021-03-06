#Muestreo por importancia Ejemplos:
#Ejemplo 1:
N=10000
X<-rnorm(N)   #Esto se hizo para tener la media y la varianza teorica
xbarra=mean(X) #Media teorica 0.005723869
varianza=1/N   #Varianza teorica 0.0001
theta1<-rt(N,N)  #Tetas generados a partir de la funci�n de importancia t de student
w<-dnorm(theta1,xbarra,sqrt(varianza))/dt(theta1,N) #Pesos
E<-(1/sum(w))*(sum(w*theta1))  #Esperanza estimada  0.006155867
E2<-(1/sum(w))*(sum(w*(theta1^2))) #Esperanza^2 estimada 0.0001399142
V<-E2-(E^2) #Varianza 0.0001020195
#Gr�fica comparativa:
x11()
plot(density(theta1),col="red",lty=2,lwd=2,ylab=expression(paste("h(",theta,"|y)"))
     ,xlab = expression(theta),
     main = "Aproximaci�n distribuciones a posteriori")
lines(density(X),lwd=2)
legend("topright",legend=c("Exacta","T-Student"),
       col=c("Black","Red"),lty=c(1,2))

#Ejemplo 2:
funcion<-function(x){(x^9)*((1-x)^3)}
funcion2exacta<-function(x){(gamma(14)/(gamma(10)*gamma(4)))*(x^9)*((1-x)^3)*(x^2)} 
integrate(funcion,0,1) #Constante de integraci�n
integrate(funcion2exacta,0,1) #Integral de la densidad completa (=1)
Eteorica<-integrate(funcion2exacta,0,1)  #0.7142857
E2teorica<-integrate(funcion2exacta,0,1) #0.5238095
Vteorica<-0.5238095-(0.7142857^2) #Resultado: 0.01360544

teta2<-runif(N) #Funcion de importancia U(0,1)
w1<-funcion(teta2)/dunif(teta2,0,1)
E1<-(1/sum(w1))*(sum(w1*teta2))  #Esperanza estimada  0.7148448
E21<-(1/sum(w1))*(sum(w1*(teta2^2))) #0.5247885
V1<-E21-(E1^2) #Varianza estimada 0.01378547

teta22<-rbeta(N,8.25,2.75) #Funci�n de importancia Beta(8.25,2.75)
w12<-funcion(teta22)/dbeta(teta22,8.25,2.75)
E12<-(1/sum(w12))*(sum(w12*teta22))  #Esperanza estimada 0.7142896
E212<-(1/sum(w12))*(sum(w12*(teta22^2))) #Varianza estimada 0.5238356
V12<-E212-(E12^2) #0.01362596

#Gr�fica comparativa:
x11()
plot(density(teta22),col="chartreuse4",ylab=expression(paste("h(",theta,"|y)"))
     ,xlab = expression(theta),
     main = "Aproximaci�n distribuciones a posteriori",lty=5,lwd=2)
curve(funcion2exacta,0,1,10000,add=TRUE,lwd=3)
lines(density(teta2),lty=2,lwd=2,col="Red")
legend("topleft",legend=c("Exacta","Uniforme","Beta"),
       col=c("Black","Red","chartreuse4"),lty=c(1,2,5))


#Ejemplo 3
#----------------------------------------#
#N=197
m=10000
N=197
y1=125
y2=18
y3=20
y4=34
a=b=1
h<-function(x){((2+x)^y1)*((1-x)^(y2+y3+b-1))*x^(y4+a-1)}
hexacta<-function(x){(1/2.357695e+28)*((2+x)^y1)*((1-x)^(y2+y3+b-1))*x^(y4+a-1)}
#exacta
integrate(h,0,1)
Espteor<-integrate(hexacta,0,1) #0.6228062
Esp2teor<-integrate(hexacta,0,1) #0.3904824
Varteori<-0.3904824-(0.6228062)^2 #0.002594837

#Funci�n de importancia Beta
p<-rbeta(m,54.723205,32.58249)
w<-h(p)/dbeta(p,54.723205,32.58249)
E<-(1/sum(w))*(sum(w*p)) #0.6230844
EB2<-(1/sum(w))*(sum(w*(p^2))) #0.3908071 #E(x^2)
VB<-EB2-(E^2) #0.00257293

#Funci�n de importancia normal
p1<-rnorm(m,0.6268,sqrt(0.002649))
w1<-h(p1)/dnorm(p1,0.6268,sqrt(0.002649))
E1<-(1/sum(w1))*(sum(w1*p1)) #0.623249
EN2<-(1/sum(w1))*(sum(w1*(p1^2))) #0.3910104 #E(x^2)
VN<-EN2-(E1^2) #0.0025704

x11()
plot(density(p),col="chartreuse4",ylab=expression(paste("h(",theta,"|y)"))
     ,xlab = expression(theta),
     main = "Aproximaci�n distribuciones a posteriori N=197",lty=5,lwd=2)
curve(hexacta,0,1,10000,add=TRUE,lwd=3)
lines(density(p1),lty=2,lwd=2,col="Red")
legend("topright",legend=c("Exacta","Normal","Beta"),
         col=c("Black","Red","chartreuse4"),lty=c(1,2,5))

#----------------------------------------------------------#
#N=20
y11=14
y22=0
y33=1
y44=5
h1<-function(x){((2+x)^y11)*((1-x)^(y22+y33+b-1))*x^(y44+a-1)}
h1exacta<-function(x){(1/41575.13)*((2+x)^y11)*((1-x)^(y22+y33+b-1))*x^(y44+a-1)}
#exacta
integrate(h1,0,1) #Constante de integraci�n
Espteor00<-integrate(h1exacta,0,1) #0.8311239 
Esp2teor00<-integrate(h1exacta,0,1) #0.7024182
Varteori00<-0.7024182-(0.8311239)^2 #0.01165126

#Funci�n de importancia Beta
p00<-rbeta(m,8.165772,0.873161)
w00<-h1(p00)/dbeta(p00,8.165772,0.873161)
E00<-(1/sum(w00))*(sum(w00*p00)) #0.8306012
E200<-(1/sum(w00))*(sum(w00*p00^2)) #0.7017816
V00<-E200-(E00^2)  #0.01188336

#Funci�n de importancia normal: Esta normal sin truncar no funciona como f.de importancia.
p11<-rnorm(m,0.9034,sqrt(0.008693))
w11<-h1(p11)/dnorm(p11,0.9034,sqrt(0.008693))
E11<-(1/sum(w11))*(sum(w11*p11))  #1.224096
E211<-(1/sum(w11))*(sum(w11*p11^2)) #1.494372
V11<-E211-(E11^2) #0.0003833

#Funci�n de importancia normal Truncada:
p111<-rtruncnorm(m,a=-Inf,b=1,0.9034,sqrt(0.008693))
w111<-h1(p111)/dtruncnorm(p111,a=-Inf,b=1,0.9034,sqrt(0.008693))
E111<-(1/sum(w111))*(sum(w111*p111))  #0.8377618
E2111<-(1/sum(w111))*(sum(w111*p111^2)) #0.7117673
V111<-E2111-(E111^2) #0.009922389

x11()
plot(density(p00),col="chartreuse4",ylab=expression(paste("h(",theta,"|y)"))
     ,xlab = expression(theta),
     main = "Aproximaci�n distribuciones a posteriori N=20",lty=5,lwd=2)
curve(h1exacta,0,1,10000,add=TRUE,lwd=3)
lines(density(p111),lty=2,lwd=2,col="Red")
legend("topleft",legend=c("Exacta","Normal Truncada","Beta"),
       col=c("Black","Red","chartreuse4"),lty=c(1,2,5))
#-------------------------------------------------#

