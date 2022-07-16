#Simular 1000 valores de una variable aleatoria normal (VARIABLE 1)
n<-1000 #genero 1000 valores aleatorios provenientes de una normal (0,1)
set.seed(423435)
va1<- rnorm(n,600,25)
hist(va1, freq = FALSE, col='lightblue',
     main='Histograma de X', xlab='',breaks=18)
curve(dnorm(x,mean=600,sd=25), add=TRUE, col='darkgreen') #Curva teorica


va2<- rgamma(n,6,1/120)
hist(va2, freq = FALSE, col='lightblue',
     main='Histograma de X', xlab='',breaks=20)
curve(dgamma(x,6,1/120), add=TRUE, col='darkgreen') #Curva teorica


#Obtener muestra de 50 poblaciones
#Las principales m�tricas de tendencia central y de dispersi�n, as� como los
#coeficientes de asimetr�a y curtosis.
va3<- rnorm(50,600,25)
va4<- rgamma(50,6,1/120)

sd2<-sqrt(sum((va2-mean(va2))^2)/(length(va2)-1))#desvio
sd2
#momentos muestrales
mean(va3) #Promedio
sum((va3-mean(va3))^2)/(length(va3)-1)  #Momento centrado de segundo orden
#esta ultima intenta parecers a la varianza
sd3<-sqrt(sum((va3-mean(va3))^2)/(length(va3)-1))#desvio
sqrt(sum((va3-mean(va3))^2)/(length(va3)-1))/mean(va3) #coeficiente de variacion
par(mfrow=c(1,2))
hist(va3, freq = FALSE, col='lightblue',
     main='Histograma de X', xlab='',breaks=10)
boxplot(va3, horizontal=TRUE, col='steelblue',
        main='Boxplot Normal 50 ensayos')

#momentos muestrales
mean(va4) #Promedio
sum((va4-mean(va4))^2)/(length(va4)-1) #Momento centrado de segundo orden
#esta ultima intenta parecers a la varianza
sd4<-sqrt(sum((va4-mean(va4))^2)/(length(va4)-1))#desvio
sqrt(sum((va4-mean(va4))^2)/(length(va4)-1))/mean(va4) #coeficiente de variacion
par(mfrow=c(1,2))
hist(va4, freq = FALSE, col='lightblue',
     main='Histograma de X', xlab='',breaks=10)
boxplot(va4, horizontal=TRUE, col='steelblue',
        main='Boxplot Gamma 50 ensayos')

#Obtenga el Q-Q plot de los datos contra una distribuci�n normal.
qqnorm(va3)
qqline(va3,col='darkred')
qqnorm(va4)
qqline(va4,col='darkred')

#Repita el punto anterior, pero eliminando los outliers.
min(va3)
max(va3)

cuantil.muestral<-function(x,omega){
  W<-1
  i<-1
  x<-sort(x)
  while(W==1){
    if(i/(length(x)+1)>omega){
      W<-0} else {i<-i+1}
  }
  return(mean(c(x[i],x[i-1])))
}
cuantil.muestral(va3,0.2) #cuantil 0.2 de los datos
#calculo cuantiles para va3
cuantil.muestral(va3,0.25)
cuantil.muestral(va3,0.5) #mediana
cuantil.muestral(va3,0.75)
RIC_3<-cuantil.muestral(va3,0.75)-cuantil.muestral(va3,0.25)
x2_3<-cuantil.muestral(va3,0.75) + 1.5*RIC_3
x1_3<-cuantil.muestral(va3,0.25) - 1.5*RIC_3
length(va3)
boxplot(va3, horizontal=TRUE, col='steelblue',
        main='Boxplot Normal 50 ensayos')
va3
#calculo cuantiles para va4
cuantil.muestral(va4,0.25)
cuantil.muestral(va4,0.5) #mediana
cuantil.muestral(va4,0.75)
RIC_4<-cuantil.muestral(va4,0.75)-cuantil.muestral(va4,0.25)
x2_4<-cuantil.muestral(va4,0.75) + 1.5*RIC_4
x1_4<-cuantil.muestral(va4,0.25) - 1.5*RIC_4

boxplot(va4, horizontal=TRUE, col='steelblue',
        main='Boxplot Normal 50 ensayos')
va3
#Eliminamos outliers en va3
vaN3<-c()
for (i in va3){
  if (i>=x1_3 & i<=x2_3){
    vaN3<-c(vaN3,i)
  }
}
vaN3
#Eliminamos outliers en va4
vaN4<-c()
for (i in va4){
  if (i>=x1_4 & i<=x2_4){
    vaN4<-c(vaN4,i)
  }
}
vaN4


#comparacion frente a va3 (normal 50 ensayos-normal)
par(mfrow=c(1,2))
boxplot(vaN3, horizontal=TRUE, col='steelblue',
        main='Boxplot Normal 50 ensayos - outliers')
boxplot(va3, horizontal=TRUE, col='steelblue',
        main='Boxplot Normal 50 ensayos')

par(mfrow=c(1,2))
qqnorm(va3,main = 'Q-Q Plot con outliers')
qqline(va3,col='darkred')
qqnorm(vaN3,main='Q-Q Plot sin outliers')
qqline(vaN3,col='darkred')
#comparacion frente a va4(gamma 50 ensayos-normal)
par(mfrow=c(1,2))
boxplot(vaN4, horizontal=TRUE, col='steelblue',
        main='Boxplot GAMMA 50 ensayos - outliers')
boxplot(va4, horizontal=TRUE, col='steelblue',
        main='Boxplot GAMMA 50 ensayos')

par(mfrow=c(1,2))
qqnorm(va4,main = 'Q-Q Plot con outliers')
qqline(va4,col='darkred')
qqnorm(vaN4,main='Q-Q Plot sin outliers')
qqline(vaN4,col='darkred')

#curtosis y asimetra 3-7
#normal n=1000
sk1<-mean(((va1-600)/25)^3) #skewness - momento estandarizado de tercer orden
k1<-mean(((va1-600)/25)^4) #kurtosis - vale 3 si la campana es gaussiana
#gamma n=1000
sk2<-mean(((va2-720)/294)^3) #skewness - momento estandarizado de tercer orden
k2<-mean(((va2-720)/294)^4) #kurtosis - vale 3 si la campana es gaussiana


#normal n=50
sk3<-mean(((va3-mean(va3))/sd3)^3) #skewness - momento estandarizado de tercer orden
k3<-mean(((va3-mean(va3))/sd3)^4) #kurtosis - vale 3 si la campana es gaussiana
#gamma n=50
sk4<-mean(((va4-mean(va4))/sd4)^3) #skewness - momento estandarizado de tercer orden
k4<-mean(((va4-mean(va4))/sd4)^4) #kurtosis - vale 3 si la campana es gaussiana

#comparar sk1 y k1 con sk3 y k3
#comparar sk2 y k2 con sk4 y k4

#punto 4) calculo de intervalos de confianza w<-matrix(sample(va1,500), nrow = 100, ncol = 5)
#NORMAL

matriz<-matrix(nrow = 1,ncol = 9)
for (val in c(1:100)){
  j1<-va1[sample(0:n,size = 5)]
  mean(j1)
  LI<-mean(j1)-qnorm(1-0.05/2)*25/sqrt(5)
  LS<-mean(j1)+qnorm(1-0.05/2)*25/sqrt(5)
  IC<-c(LI,LS)
  E<-qnorm(1-0.05/2)*25/sqrt(5)
  Sobs<-sqrt(sum((j1-mean(j1))^2)/(length(j1)-1)) #Sobs
  LIt<-mean(j1)-qt(1-0.05/2,5)*Sobs/sqrt(5)
  LDt<-mean(j1)+qt(1-0.05/2,5)*Sobs/sqrt(5)
  ICt<-c(LIt,LDt)
  Et<-qt(1-0.05/2,5)*Sobs/sqrt(5)
  aux<-c(val,mean(j1),IC,E,Sobs,ICt,Et)
  matriz<-rbind(matriz,aux)
}
matriz
write.csv(matriz,"matrizTPNormalN5333.csv")

qt(0.95,15)
matriz2<-matrix(nrow = 1,ncol = 9)
for (val in c(1:100)){
  j1<-va1[sample(0:n,size = 30)]
  mean(j1)
  LI<-mean(j1)-qnorm(1-0.05/2)*25/sqrt(30)
  LS<-mean(j1)+qnorm(1-0.05/2)*25/sqrt(30)
  IC<-c(LI,LS)
              ICt<-c(LIt,LDt)
  ICt
  Et<-qt(1-0.05/2,5)*Sobs/sqrt(30)
  aux<-c(val,mean(j1),IC,E,Sobs,ICt,Et)
  matriz2<-rbind(matriz2,aux)
}
matriz2
write.csv(matriz2,"matrizTPNormalN30333.csv")





#GAMMA 

matriz3<-matrix(nrow = 1,ncol = 9)
for (val in c(1:100)){
  j1<-va2[sample(0:n,size = 5)]
  mean(j1)
  LI<-mean(j1)-qnorm(1-0.05/2)*294/sqrt(5)
  LS<-mean(j1)+qnorm(1-0.05/2)*294/sqrt(5)
  IC<-c(LI,LS)
  E<-qnorm(1-0.05/2)*294/sqrt(5)
  Sobs<-sqrt(sum((j1-mean(j1))^2)/(length(j1)-1)) #Sobs
  LIt<-mean(j1)-qt(1-0.05/2,5)*Sobs/sqrt(5)
  LDt<-mean(j1)+qt(1-0.05/2,5)*Sobs/sqrt(5)
  ICt<-c(LIt,LDt)
  Et<-qt(1-0.05/2,5)*Sobs/sqrt(5)
  aux<-c(val,mean(j1),IC,E,Sobs,ICt,Et)
  matriz3<-rbind(matriz3,aux)
}
matriz3
write.csv(matriz3,"matrizTPGammaN5333.csv")



matriz4<-matrix(nrow = 1,ncol = 9)
for (val in c(1:100)){
  j1<-va2[sample(0:n,size = 30)]
  mean(j1)
  LI<-mean(j1)-qnorm(1-0.05/2)*294/sqrt(30)
  LS<-mean(j1)+qnorm(1-0.05/2)*294/sqrt(30)
  IC<-c(LI,LS)
  E<-qnorm(1-0.05/2)*25/sqrt(30)
  Sobs<-sqrt(sum((j1-mean(j1))^2)/(length(j1)-1)) #Sobs
  LIt<-mean(j1)-qt(1-0.05/2,30)*Sobs/sqrt(30)
  LDt<-mean(j1)+qt(1-0.05/2,30)*Sobs/sqrt(30)
  ICt<-c(LIt,LDt)
  Et<-qt(1-0.05/2,5)*Sobs/sqrt(30)
  aux<-c(val,mean(j1),IC,E,Sobs,ICt,Et)
  matriz4<-rbind(matriz4,aux)
}
matriz4
write.csv(matriz4,"matrizTPGammaN303333.csv")

#comparo con intervalo de confianza empirico
alfa=0.05
#desvio desconocido normal
sd3<-sqrt(sum((va3-mean(va3))^2)/(length(va3)-1))
LSdn<- qt(1-alfa/2,5)*sd3/sqrt(n)+mean(va3)
LIdn<- -qt(1-alfa/2,5)*sd3/sqrt(n)+mean(va3)
ICdn=c(LIdn,LSdn)
ICdn
#desvio conocido normal = 25
LScn<- qnorm(1-alfa/2)*25/sqrt(n)+mean(va3)
LIcn<- -qnorm(1-alfa/2)*25/sqrt(n)+mean(va3)
ICcn=c(LIcn,LScn)
ICcn

#desvio desconocido gamma
sd4<-sqrt(sum((va4-mean(va4))^2)/(length(va4)-1))
LSdg<- qt(1-alfa/2,5)*sd4/sqrt(n)+mean(va4)
LIdg<- -qt(1-alfa/2,5)*sd4/sqrt(n)+mean
ICdg=c(LIdg,LSdg)
ICdg
#desvio conocido gamma = 294
LScg<- qnorm(1-alfa/2)*294/sqrt(n)+mean(va4)
LIcg<- -qnorm(1-alfa/2)*294/sqrt(n)+mean(va4)
ICcg=c(LIcg,LScg)
ICcg
