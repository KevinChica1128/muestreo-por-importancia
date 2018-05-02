#Muestreo por importancia Ejemplos:
#Ejemplo 1:
N=10000
X<-rnorm(N)   #Esto se hizo para tener la media y la varianza teorica
xbarra=mean(X) #Media teorica
varianza=1/N   #Varianza teorica
theta1<-rt(N,N)  #Tetas generados a partir de la función de importancia t de student
w<-dnorm(theta1,xbarra,sqrt(varianza))/dt(theta1,N) #Pesos
E<-(1/sum(w))*(sum(w*theta1))  #Esperanza estimada
V<-(1/sum(w))*(sum(w*(theta1^2))) #Varianza estimada
#Gráfica comparativa:
x11()
plot(density(theta1),col="red",lty=2,lwd=2)
lines(density(X),lwd=2)


#Ejemplo 2:
funcion<-function(x){(gamma(14)/(gamma(10)*gamma(4)))*(x^9)*((1-x)^3)*x} #E(X)
funcion2<-function(x){(gamma(14)/(gamma(10)*gamma(4)))*(x^9)*((1-x)^3)*(x^2)} #E(X^2)
Eteorica<-integrate(funcion,0,1)
E2teorica<-integrate(funcion2,0,1)
Vteorica<-0.5238095-(0.7142857^2) #Resultado: 0.01360544
teta2<-runif(N)
w1<-funcion(teta2)/dunif(teta2)
E1<-(1/sum(w1))*(sum(w1*teta2))  #Esperanza estimada
E21<-(1/sum(w1))*(sum(w1*(teta2^2))) #Varianza estimada
V1<-E21-(E1^2) #0.01250114

tetai2<-rbeta(N,1,1)
w11<-funcion(tetai2)/dbeta(tetai2,1,1)
E11<-(1/sum(w11))*(sum(w11*tetai2))  #Esperanza estimada
E211<-(1/sum(w11))*(sum(w11*(tetai2^2))) #Varianza estimada

p<-runif(10000,0,1)
A<-funcion(p)
sum(A)/10000
B<-funcion2(p)
sum(B)/10000

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
hexacta<-function(x){(1/2.357695e+28)*((2+x)^y1)*((1-x)^(y2+y3+b-1))*x^(y4+a-1)*(x^2)}
#exacta
integrate(h,0,1)
Espteor<-integrate(hexacta,0,1) #0.6228062
Esp2teor<-integrate(hexacta,0,1) #0.3904824
Varteori<-0.3904824-(0.6228062)^2 #0.002594837

#Función de importancia Beta
p<-rbeta(m,54.723205,32.58249)
w<-h(p)/dbeta(p,54.723205,32.58249)
E<-(1/sum(w))*(sum(w*p)) #0.6230844
EB2<-(1/sum(w))*(sum(w*(p^2))) #0.3908071 #E(x^2)
VB<-EB2-(E^2) #0.00257293

#Función de importancia normal
p1<-rnorm(m,0.6268,sqrt(0.002649))
w1<-h(p1)/dnorm(p1,0.6268,sqrt(0.002649))
E1<-(1/sum(w1))*(sum(w1*p1)) #0.623249
EN2<-(1/sum(w1))*(sum(w1*(p1^2))) #0.3910104 #E(x^2)
VN<-EN2-(E1^2) #0.0025704

x11()
plot(density(p),col="chartreuse4",ylab=expression(paste("h(",theta,"|y)"))
     ,xlab = expression(theta),
     main = "Aproximación distribuciones a posteriori",lty=5,lwd=2)
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
h1exacta<-function(x){(1/41575.13)*((2+x)^y11)*((1-x)^(y22+y33+b-1))*x^(y44+a-1)*(x^2)}
#exacta
integrate(h1,0,1) #Constante de integración
Espteor00<-integrate(h1exacta,0,1) #0.8311239 
Esp2teor00<-integrate(h1exacta,0,1) #0.7024182
Varteori00<-0.7024182-(0.8311239)^2 #0.01165126

#Función de importancia Beta
p00<-rbeta(m,8.165764,0.873160)
w00<-h1(p00)/dbeta(p00,8.165764,0.873160)
E00<-(1/sum(w00))*(sum(w00*p00)) #0.83156
E200<-(1/sum(w00))*(sum(w00*p00^2)) #0.7030455
V00<-E200-(E00^2)  #0.01154752

#Función de importancia normal TRUNCARLA PENDIENTE
p11<-rnorm(m,0.9034,sqrt(0.008693))
w11<-h1(p11)/dnorm(p11,0.9034,sqrt(0.008693))
E11<-(1/sum(w11))*(sum(w11*p11))  #1.224096
E211<-(1/sum(w11))*(sum(w11*p11^2)) #1.494372
V11<-E211-(E11^2) #0.0003833

x11()
plot(density(p00),col="chartreuse4",ylab=expression(paste("h(",theta,"|y)"))
     ,xlab = expression(theta),
     main = "Aproximación distribuciones a posteriori",lty=5,lwd=2)
curve(h1exacta,0,1,10000,add=TRUE,lwd=3)
lines(density(p11),lty=2,lwd=2,col="Red")
legend("topright",legend=c("Exacta","Normal","Beta"),
       col=c("Black","Red","chartreuse4"),lty=c(1,2,5))
#-------------------------------------------------#
