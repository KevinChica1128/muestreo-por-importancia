#Muestreo por importancia Ejemplos:
#Ejemplo 1:
N=10000
X<-rnorm(N)   #Esto se hizo para tener la media y la varianza teorica
xbarra=mean(X) #Media teorica
varianza=1/N   #Varianza teorica
thetai<-rt(N,N)  #Tetas generados a partir de la función de importancia t de student
w<-dnorm(thetai,xbarra,sqrt(varianza))/dt(thetai,N) #Pesos
E<-(1/sum(w))*(sum(w*thetai))  #Esperanza estimada
V<-(1/sum(w))*(sum(w*(thetai^2))) #Varianza estimada

#Ejemplo 2:
funcion<-function(x){(x^9)*((1-x)^3)}
integrate(funcion,0,1)
tetai<-runif(N)
w1<-funcion(tetai)/dunif(tetai)
E1<-(1/sum(w1))*(sum(w1*tetai))  #Esperanza estimada
V1<-(1/sum(w1))*(sum(w1*(tetai^2))) #Varianza estimada

#Ejemplo 3 (INCOMPLETO)
#N=197
m=10000
N=197
y1=125
y2=18
y3=20
y4=34
a=b=1

#Esperanza de la exacta con la regla de Simpson:
I=
  
  #Función de importancia Beta
  p<-rbeta(m,1,1)
h<-function(x){((2+x)^y1)*((1-x)^(y2+y3+b-1))*x^(y4+a-1)}
w<-h(p)/dbeta(p,1,1)
E<-(1/sum(w))*(sum(w*p))
V<-(1/sum(w))*(sum(w*(p^2)))

#Función de importancia normal
p1<-rnorm(m,0.6268,0.002649)
w1<-h(p1)/qnorm(p1,0.6268,0.002649)
E1<-(1/sum(w1))*(sum(w1*p1))
V1<-(1/sum(w1))*(sum(w1*(p1^2)))
#Función de importancia uniforme(0,1):
p2<-runif(m,0,1)
w2<-h(p2)/punif(p2,0,)
E2<-(1/sum(w2))*(sum(w2*p2))


#N=20
y11=14
y22=0
y33=1
y44=5
h1<-function(x){((2+x)^y11)*((1-x)^(y22+y33+b-1))*x^(y44+a-1)}
#Función de importancia Beta
p00<-rbeta(m,1,1)
w00<-h1(p00)/pbeta(p00,1,1)
E00<-(1/sum(w00))*(sum(w00*p00))
V00<-(1/sum(w00))*(sum(w00*p00^2))

#Función de importancia normal
p11<-rnorm(m,0.9034,0.008693)
w11<-h1(p11)/pnorm(p11,0.9034,0.008693)
E11<-(1/sum(w11))*(sum(w11*p11))

#Función de importancia uniforme(0,1):
p22<-runif(m,0,1)
w22<-h1(p22)/punif(p22,0,1)
E22<-(1/sum(w22))*(sum(w22*p22))

x11()
plot(p,h(p))
x11()
plot(density(h(p)),xlim=c(0,50))
x11()
plot(p1,h(p1))
x11()
plot(density(h(p1)))