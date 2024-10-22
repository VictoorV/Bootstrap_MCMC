############## Question 1 ##############

X_n_10<-read.table("C:/Users/jmv/Documents/Travail/Master/r/projet_SC/X_n_10.txt",header=TRUE,sep=" ")
X_n_100<-read.table("C:/Users/jmv/Documents/Travail/Master/r/projet_SC/X_n_100.txt",header=TRUE,sep=" ")
theta<-c(20,7,8,2,1.75^2)
Y_10<-rep(0,10)
Y_100<-rep(0,100)

set.seed(2023) # Création des données d'ajustement
err_10<-rnorm(10,0,sqrt(theta[5]))
for(i in 1:10){
  Y_10[i]=theta[1]+theta[2]*X_n_10[i,1]+theta[3]*X_n_10[i,1]**2+theta[4]*X_n_10[i,1]**3+err_10[i]
}

err_100<-rnorm(100,0,sqrt(theta[5]))
for(i in 1:100){
  Y_100[i]=theta[1]+theta[2]*X_n_100[i,1]+theta[3]*X_n_100[i,1]**2+theta[4]*X_n_100[i,1]**3+err_100[i]
}
plot(X_n_10[,1],Y_10,col="blue",pch=19,xlab="X",ylab="Y",main="Y_10 en fonction de X_n_10")
plot(X_n_100[,1],Y_100,col="blue",pch=19,xlab="X",ylab="Y",main="Y_100 en fonction de X_n_100")

############## Question 2 ##############

### n=10
modele1<-lm(Y_10 ~ X_n_10[,1] + I(X_n_10[,1]^2) + I(X_n_10[,1]^3))
summary(modele1)
poly1<-function(x){
  return(modele1[["coefficients"]][1]+modele1[["coefficients"]][2]*x+modele1[["coefficients"]][3]*x^2+modele1[["coefficients"]][4]*x^3)
}

IC1<-confint(modele1,level=0.95) # IC 95% pour theta (n=10)
E1<-summary(modele1)$sigma
lower1<-(6*E1**2)/qchisq(1-0.05/2,6)
upper1<-(6*E1**2)/qchisq(0.05/2,6)
IC1<-rbind(IC1,c(lower1,upper1))
rownames(IC1)<-c("a","b","c","d","sigma^2")
colnames(IC1)<-c("2.5%","97.5%")
head(IC1)


### n=100
modele2<-lm(Y_100 ~ X_n_100[,1] + I(X_n_100[,1]^2) + I(X_n_100[,1]^3))
summary(modele2)
poly2<-function(x){
  return(modele2[["coefficients"]][1]+modele2[["coefficients"]][2]*x+modele2[["coefficients"]][3]*x^2+modele2[["coefficients"]][4]*x^3)
}

IC2<-confint(modele2,level=0.95) # IC 95% pour theta (n=100)
E2<-summary(modele2)$sigma
lower2<-(96*(E2)**2)/qchisq(1-0.05/2,96)
upper2<-(96*(E2)**2)/qchisq(0.05/2,96)
IC2<-rbind(IC2,c(lower2,upper2))
rownames(IC2)<-c("a","b","c","d","sigma^2")
colnames(IC2)<-c("2.5%","97.5%")
head(IC2)

############## Question 3 ##############

X_pred<-seq(-3,1,length.out = 50)
Y1_pred<-poly1(X_pred) # Valeurs prédites avec la fonction lm (n=10)
Y2_pred<-poly2(X_pred) # Valeurs prédites avec la fonction lm (n=100)
err_pred<-rnorm(50,0,sqrt(theta[5]))
Y_pred<-theta[1]+theta[2]*X_pred+theta[3]*X_pred^2+theta[4]*X_pred^3+err_pred # Vraies valeurs

plot(X_pred,Y_pred,col="blue",pch=19,xlab="X",ylab="Y",main="Graphe des vraies valeurs/valeurs prédites pour n=10")
points(X_pred,Y1_pred,pch=19,col="red")
legend("topleft", legend = c("Vraies valeurs","Ŷ_pred"), col = c("blue","red"), pch = c(19, 19))

plot(X_pred,Y_pred,col="blue",pch=19,xlab="X",ylab="Y",main="Graphe des vraies valeurs/valeurs prédites pour n=100")
points(X_pred,Y2_pred,pch=19,col="red")
legend("topleft", legend = c("Vraies valeurs","Ŷ_pred"), col = c("blue","red"), pch = c(19, 19))

############## Question 4 ##############
B<-2000

### n=10
res_boot_10<-data.frame(matrix(nrow = B, ncol = 5)) # Pour stocker les résultats de chaque échantillon bootstrap non paramétrique (n=10)
colnames(res_boot_10)<-c("a","b","c","d","sigma^2")
for(i in 1:B){
  sample<-sample(c(1:10),10,replace=TRUE)
  modele_boot<-lm(Y_10[sample] ~ X_n_10[,1][sample] + I((X_n_10[,1][sample])^2) + I((X_n_10[,1][sample])^3))
  res_boot_10[i,]<-c(modele_boot[["coefficients"]],(summary(modele_boot)$sigma)^2)
}
poly1_boot<-function(x){
  return(mean(res_boot_10[,1][!is.na(res_boot_10[,1])])+mean(res_boot_10[,2][!is.na(res_boot_10[,2])])*x+mean(res_boot_10[,3][!is.na(res_boot_10[,3])])*x^2+mean(res_boot_10[,4][!is.na(res_boot_10[,4])])*x^3)
}
plot(X_pred,Y_pred,col="blue",pch=19,xlab="X",ylab="Y",main="Bootstrap non paramétrique pour n=10")
points(X_pred,poly1_boot(X_pred),pch=19,col="red")
legend("topleft", legend = c("Vraies valeurs", "Ŷ_pred_nonpara"), col = c("blue", "red"), pch = c(19, 19))

IC_boot_10<-data.frame(matrix(nrow = 5, ncol = 2))  # IC 95% bootstrap non paramétrique pour theta (n=10)
IC_boot_10[1,1]<-2*modele1[["coefficients"]][1]-sort(res_boot_10[,1])[ceiling(B*(1-0.05/2))]
IC_boot_10[1,2]<-2*modele1[["coefficients"]][1]-sort(res_boot_10[,1])[ceiling(B*(0.05/2))]
IC_boot_10[2,1]<-2*modele1[["coefficients"]][2]-sort(res_boot_10[,2])[ceiling(B*(1-0.05/2))]
IC_boot_10[2,2]<-2*modele1[["coefficients"]][2]-sort(res_boot_10[,2])[ceiling(B*(0.05/2))]
IC_boot_10[3,1]<-2*modele1[["coefficients"]][3]-sort(res_boot_10[,3])[ceiling(B*(1-0.05/2))]
IC_boot_10[3,2]<-2*modele1[["coefficients"]][3]-sort(res_boot_10[,3])[ceiling(B*(0.05/2))]
IC_boot_10[4,1]<-2*modele1[["coefficients"]][4]-sort(res_boot_10[,4])[ceiling(B*(1-0.05/2))]
IC_boot_10[4,2]<-2*modele1[["coefficients"]][4]-sort(res_boot_10[,4])[ceiling(B*(0.05/2))]
IC_boot_10[5,1]<-2*(summary(modele1)$sigma)^2-sort(res_boot_10[,5])[ceiling(B*(1-0.05/2))]
IC_boot_10[5,2]<-2*(summary(modele1)$sigma)^2-sort(res_boot_10[,5])[ceiling(B*(0.05/2))]
rownames(IC_boot_10)<-c("a","b","c","d","sigma^2")
colnames(IC_boot_10)<-c("2.5%","97.5%")
head(IC_boot_10)

temp<-data.frame(matrix(nrow = B, ncol = 50))
for(i in 1:B){
  temp[i,]<-res_boot_10[i,1]+res_boot_10[i,2]*X_pred+res_boot_10[i,3]*X_pred^2+res_boot_10[i,4]*X_pred^3
}
temp2<-temp
for(i in 1:B){
  temp2[i,]<-temp2[i,]+rnorm(50,0,E1)
}
IC_Ynonpara_10<-data.frame(matrix(nrow = 50, ncol = 2)) # IC 95% bootstrap non paramétrique pour les valeurs prédites (n=10)
for(i in 1:50){
  IC_Ynonpara_10[i,]<-c(quantile(temp[,i],c(0.025,0.975),na.rm=TRUE)[1],quantile(temp[,i],c(0.025,0.975),na.rm=TRUE)[2])
}
IP_Ynonpara_10<-data.frame(matrix(nrow = 50, ncol = 2)) # IP 95% bootstrap non paramétrique (n=10)
for(i in 1:50){
  IP_Ynonpara_10[i,]<-c(quantile(temp2[,i],c(0.025,0.975),na.rm=TRUE)[1],quantile(temp2[,i],c(0.025,0.975),na.rm=TRUE)[2])
}
colnames(IC_Ynonpara_10)<-c("2.5%","97.5%")
head(IC_Ynonpara_10)
colnames(IP_Ynonpara_10)<-c("2.5%","97.5%")
head(IP_Ynonpara_10)


### n=100
res_boot_100<-data.frame(matrix(nrow = B, ncol = 5)) # Pour stocker les résultats de chaque échantillon bootstrap non paramétrique (n=100)
colnames(res_boot_100)<-c("a","b","c","d","sigma^2")
for(i in 1:B){
  sample<-sample(c(1:100),100,replace=TRUE)
  modele_boot<-lm(Y_100[sample] ~ X_n_100[,1][sample] + I((X_n_100[,1][sample])^2) + I((X_n_100[,1][sample])^3))
  res_boot_100[i,]<-c(modele_boot[["coefficients"]],(summary(modele_boot)$sigma)^2)
}
poly2_boot<-function(x){
  return(mean(res_boot_100[,1][!is.na(res_boot_100[,1])])+mean(res_boot_100[,2][!is.na(res_boot_100[,2])])*x+mean(res_boot_100[,3][!is.na(res_boot_100[,3])])*x^2+mean(res_boot_100[,4][!is.na(res_boot_100[,4])])*x^3)
}
plot(X_pred,Y_pred,col="blue",pch=19,xlab="X",ylab="Y",main="Bootstrap non paramétrique pour n=100")
points(X_pred,poly2_boot(X_pred),pch=19,col="red")
legend("topleft", legend = c("Vraies valeurs", "Ŷ_pred_nonpara"), col = c("blue", "red"), pch = c(19, 19))

IC_boot_100<-data.frame(matrix(nrow = 5, ncol = 2)) # IC 95% bootstrap non paramétrique pour theta (n=100)
IC_boot_100[1,1]<-2*modele2[["coefficients"]][1]-sort(res_boot_100[,1])[ceiling(B*(1-0.05/2))]
IC_boot_100[1,2]<-2*modele2[["coefficients"]][1]-sort(res_boot_100[,1])[ceiling(B*(0.05/2))]
IC_boot_100[2,1]<-2*modele2[["coefficients"]][2]-sort(res_boot_100[,2])[ceiling(B*(1-0.05/2))]
IC_boot_100[2,2]<-2*modele2[["coefficients"]][2]-sort(res_boot_100[,2])[ceiling(B*(0.05/2))]
IC_boot_100[3,1]<-2*modele2[["coefficients"]][3]-sort(res_boot_100[,3])[ceiling(B*(1-0.05/2))]
IC_boot_100[3,2]<-2*modele2[["coefficients"]][3]-sort(res_boot_100[,3])[ceiling(B*(0.05/2))]
IC_boot_100[4,1]<-2*modele2[["coefficients"]][4]-sort(res_boot_100[,4])[ceiling(B*(1-0.05/2))]
IC_boot_100[4,2]<-2*modele2[["coefficients"]][4]-sort(res_boot_100[,4])[ceiling(B*(0.05/2))]
IC_boot_100[5,1]<-2*(summary(modele2)$sigma)^2-sort(res_boot_100[,5])[ceiling(B*(1-0.05/2))]
IC_boot_100[5,2]<-2*(summary(modele2)$sigma)^2-sort(res_boot_100[,5])[ceiling(B*(0.05/2))]
rownames(IC_boot_100)<-c("a","b","c","d","sigma^2")
colnames(IC_boot_100)<-c("2.5%","97.5%")
head(IC_boot_100)

for(i in 1:B){
  temp[i,]<-res_boot_100[i,1]+res_boot_100[i,2]*X_pred+res_boot_100[i,3]*X_pred^2+res_boot_100[i,4]*X_pred^3
}
temp2<-temp
for(i in 1:B){
  temp2[i,]<-temp2[i,]+rnorm(50,0,E2)
}
IC_Ynonpara_100<-data.frame(matrix(nrow = 50, ncol = 2)) # IC 95% bootstrap non paramétrique pour les valeurs prédites (n=100)
for(i in 1:50){
  IC_Ynonpara_100[i,]<-c(quantile(temp[,i],c(0.025,0.975),na.rm=TRUE)[1],quantile(temp[,i],c(0.025,0.975),na.rm=TRUE)[2])
}
IP_Ynonpara_100<-data.frame(matrix(nrow = 50, ncol = 2)) # IP 95% bootstrap non paramétrique (n=100)
for(i in 1:50){
  IP_Ynonpara_100[i,]<-c(quantile(temp2[,i],c(0.025,0.975),na.rm=TRUE)[1],quantile(temp2[,i],c(0.025,0.975),na.rm=TRUE)[2])
}
colnames(IC_Ynonpara_100)<-c("2.5%","97.5%")
head(IC_Ynonpara_100)
colnames(IP_Ynonpara_100)<-c("2.5%","97.5%")
head(IP_Ynonpara_100)

############## Question 5 ##############

### n=10
Y_10_para<-rep(0,10)
residus_10<-rep(0,10)
res_para_10<-data.frame(matrix(nrow = B, ncol = 5)) # Pour stocker les résultats de chaque échantillon bootstrap paramétrique (n=10)
for(i in 1:B){
  residus_10<-sample(resid(modele1),10,replace=TRUE)
  for(j in 1:10){
    Y_10_para[j]=modele1[["coefficients"]][1]+modele1[["coefficients"]][2]*X_n_10[j,1]+modele1[["coefficients"]][3]*X_n_10[j,1]^2+modele1[["coefficients"]][4]*X_n_10[j,1]^3+residus_10[j]
  }
  modele_para<-lm(Y_10_para ~ X_n_10[,1] + I(X_n_10[,1]^2) + I(X_n_10[,1]^3))
  res_para_10[i,]<-c(modele_para[["coefficients"]],(summary(modele_para)$sigma)^2)
}
poly1_para<-function(x){
  return(mean(res_para_10[,1][!is.na(res_para_10[,1])])+mean(res_para_10[,2][!is.na(res_para_10[,2])])*x+mean(res_para_10[,3][!is.na(res_para_10[,3])])*x^2+mean(res_para_10[,4][!is.na(res_para_10[,4])])*x^3)
}
plot(X_pred,Y_pred,col="blue",pch=19,xlab="X",ylab="Y",main="Bootstrap paramétrique n=10")
points(X_pred,poly1_para(X_pred),pch=19,col="red")
legend("topleft", legend = c("Vraies valeurs", "Ŷ_pred_para"), col = c("blue", "red"), pch = c(19, 19))

IC_para_10<-data.frame(matrix(nrow = 5, ncol = 2)) # IC 95% bootstrap paramétrique pour theta (n=10)
IC_para_10[1,1]<-2*modele1[["coefficients"]][1]-sort(res_para_10[,1])[ceiling(B*(1-0.05/2))]
IC_para_10[1,2]<-2*modele1[["coefficients"]][1]-sort(res_para_10[,1])[ceiling(B*(0.05/2))]
IC_para_10[2,1]<-2*modele1[["coefficients"]][2]-sort(res_para_10[,2])[ceiling(B*(1-0.05/2))]
IC_para_10[2,2]<-2*modele1[["coefficients"]][2]-sort(res_para_10[,2])[ceiling(B*(0.05/2))]
IC_para_10[3,1]<-2*modele1[["coefficients"]][3]-sort(res_para_10[,3])[ceiling(B*(1-0.05/2))]
IC_para_10[3,2]<-2*modele1[["coefficients"]][3]-sort(res_para_10[,3])[ceiling(B*(0.05/2))]
IC_para_10[4,1]<-2*modele1[["coefficients"]][4]-sort(res_para_10[,4])[ceiling(B*(1-0.05/2))]
IC_para_10[4,2]<-2*modele1[["coefficients"]][4]-sort(res_para_10[,4])[ceiling(B*(0.05/2))]
IC_para_10[5,1]<-2*(summary(modele1)$sigma)^2-sort(res_para_10[,5])[ceiling(B*(1-0.05/2))]
IC_para_10[5,2]<-2*(summary(modele1)$sigma)^2-sort(res_para_10[,5])[ceiling(B*(0.05/2))]
rownames(IC_para_10)<-c("a","b","c","d","sigma^2")
colnames(IC_para_10)<-c("2.5%","97.5%")
head(IC_para_10)

for(i in 1:B){
  temp[i,]<-res_para_10[i,1]+res_para_10[i,2]*X_pred+res_para_10[i,3]*X_pred^2+res_para_10[i,4]*X_pred^3
}
temp2<-temp
for(i in 1:B){
  temp2[i,]<-temp2[i,]+rnorm(50,0,E1)
}
IC_Ypara_10<-data.frame(matrix(nrow = 50, ncol = 2)) # IC 95% bootstrap paramétrique pour les valeurs prédites (n=10)
for(i in 1:50){
  IC_Ypara_10[i,]<-c(quantile(temp[,i],c(0.025,0.975),na.rm=TRUE)[1],quantile(temp[,i],c(0.025,0.975),na.rm=TRUE)[2])
}
IP_Ypara_10<-data.frame(matrix(nrow = 50, ncol = 2)) # IP 95% bootstrap paramétrique (n=10)
for(i in 1:50){
  IP_Ypara_10[i,]<-c(quantile(temp2[,i],c(0.025,0.975),na.rm=TRUE)[1],quantile(temp2[,i],c(0.025,0.975),na.rm=TRUE)[2])
}
colnames(IC_Ypara_10)<-c("2.5%","97.5%")
head(IC_Ypara_10)
colnames(IP_Ypara_10)<-c("2.5%","97.5%")
head(IP_Ypara_10)


### n=100
Y_100_para<-rep(0,100)
residus_100<-rep(0,100)
res_para_100<-data.frame(matrix(nrow = B, ncol = 5)) # Pour stocker les résultats de chaque échantillon bootstrap paramétrique (n=100)
for(i in 1:B){
  residus_100<-sample(resid(modele2),100,replace=TRUE)
  for(j in 1:100){
    Y_100_para[j]=modele2[["coefficients"]][1]+modele2[["coefficients"]][2]*X_n_100[j,1]+modele2[["coefficients"]][3]*X_n_100[j,1]^2+modele2[["coefficients"]][4]*X_n_100[j,1]^3+residus_100[j]
  }
  modele_para<-lm(Y_100_para ~ X_n_100[,1] + I(X_n_100[,1]^2) + I(X_n_100[,1]^3))
  res_para_100[i,]<-c(modele_para[["coefficients"]],(summary(modele_para)$sigma)^2)
}
poly2_para<-function(x){
  return(mean(res_para_100[,1][!is.na(res_para_100[,1])])+mean(res_para_100[,2][!is.na(res_para_100[,2])])*x+mean(res_para_100[,3][!is.na(res_para_100[,3])])*x^2+mean(res_para_100[,4][!is.na(res_para_100[,4])])*x^3)
}
plot(X_pred,Y_pred,col="blue",pch=19,xlab="X",ylab="Y",main="Bootstrap paramétrique pour n=100")
points(X_pred,poly2_para(X_pred),pch=19,col="red")
legend("topleft", legend = c("Vraies valeurs", "Ŷ_pred_para"), col = c("blue", "red"), pch = c(19, 19))

IC_para_100<-data.frame(matrix(nrow = 5, ncol = 2)) # IC 95% bootstrap paramétrique pour theta (n=100)
IC_para_100[1,1]<-2*modele2[["coefficients"]][1]-sort(res_para_100[,1])[ceiling(B*(1-0.05/2))]
IC_para_100[1,2]<-2*modele2[["coefficients"]][1]-sort(res_para_100[,1])[ceiling(B*(0.05/2))]
IC_para_100[2,1]<-2*modele2[["coefficients"]][2]-sort(res_para_100[,2])[ceiling(B*(1-0.05/2))]
IC_para_100[2,2]<-2*modele2[["coefficients"]][2]-sort(res_para_100[,2])[ceiling(B*(0.05/2))]
IC_para_100[3,1]<-2*modele2[["coefficients"]][3]-sort(res_para_100[,3])[ceiling(B*(1-0.05/2))]
IC_para_100[3,2]<-2*modele2[["coefficients"]][3]-sort(res_para_100[,3])[ceiling(B*(0.05/2))]
IC_para_100[4,1]<-2*modele2[["coefficients"]][4]-sort(res_para_100[,4])[ceiling(B*(1-0.05/2))]
IC_para_100[4,2]<-2*modele2[["coefficients"]][4]-sort(res_para_100[,4])[ceiling(B*(0.05/2))]
IC_para_100[5,1]<-2*(summary(modele2)$sigma)^2-sort(res_para_100[,5])[ceiling(B*(1-0.05/2))]
IC_para_100[5,2]<-2*(summary(modele2)$sigma)^2-sort(res_para_100[,5])[ceiling(B*(0.05/2))]
rownames(IC_para_100)<-c("a","b","c","d","sigma^2")
colnames(IC_para_100)<-c("2.5%","97.5%")
head(IC_para_100)

temp<-data.frame(matrix(nrow = B, ncol = 50))
for(i in 1:B){
  temp[i,]<-res_para_100[i,1]+res_para_100[i,2]*X_pred+res_para_100[i,3]*X_pred^2+res_para_100[i,4]*X_pred^3
}
temp2<-temp
for(i in 1:B){
  temp2[i,]<-temp2[i,]+rnorm(50,0,E2)
}
IC_Ypara_100<-data.frame(matrix(nrow = 50, ncol = 2)) # IC 95% bootstrap paramétrique pour les valeurs prédites (n=100)
for(i in 1:50){
  IC_Ypara_100[i,]<-c(quantile(temp[,i],c(0.025,0.975),na.rm=TRUE)[1],quantile(temp[,i],c(0.025,0.975),na.rm=TRUE)[2])
}
IP_Ypara_100<-data.frame(matrix(nrow = 50, ncol = 2)) # IP 95% bootstrap paramétrique (n=100)
for(i in 1:50){
  IP_Ypara_100[i,]<-c(quantile(temp2[,i],c(0.025,0.975),na.rm=TRUE)[1],quantile(temp2[,i],c(0.025,0.975),na.rm=TRUE)[2])
}
colnames(IC_Ypara_100)<-c("2.5%","97.5%")
head(IC_Ypara_100)
colnames(IP_Ypara_100)<-c("2.5%","97.5%")
head(IP_Ypara_100)

############## Question 6 ##############

### n=10
plot(X_pred,Y_pred,col="blue",pch=19,xlab="X",ylab="Y",main="Représentation des IC à 95% des prédictions pour n=10")
points(X_pred,IC_Ynonpara_10[,1],type='l',col="red")
points(X_pred,IC_Ynonpara_10[,2],type='l',col='red')
points(X_pred,IC_Ypara_10[,1],type='l',col="green")
points(X_pred,IC_Ypara_10[,2],type='l',col='green')
legend("topleft", legend = c("Vraies valeurs","IC bootstrap non paramétrique","IC bootstrap paramétrique"), col = c("blue","red","green"), pch = c(19, NA, NA),lty=c(NA,1,1))

### n=100
plot(X_pred,Y_pred,col="blue",pch=19,xlab="X",ylab="Y",main="Représentation des IC à 95% des prédictions pour n=100")
points(X_pred,IC_Ynonpara_100[,1],type='l',col="red")
points(X_pred,IC_Ynonpara_100[,2],type='l',col='red')
points(X_pred,IC_Ypara_100[,1],type='l',col="green")
points(X_pred,IC_Ypara_100[,2],type='l',col='green')
legend("topleft", legend = c("Vraies valeurs","IC bootstrap non paramétrique","IC bootstrap paramétrique"), col = c("blue","red","green"), pch = c(19, NA, NA),lty=c(NA,1,1))

### n=10
plot(X_pred,Y_pred,col="blue",pch=19,xlab="X",ylab="Y",main="Représentation des IP à 95% pour n=10")
points(X_pred,IP_Ynonpara_10[,1],type='l',col="red")
points(X_pred,IP_Ynonpara_10[,2],type='l',col='red')
points(X_pred,IP_Ypara_10[,1],type='l',col="green")
points(X_pred,IP_Ypara_10[,2],type='l',col='green')
legend("topleft", legend = c("Vraies valeurs","IP bootstrap non paramétrique","IP bootstrap paramétrique"), col = c("blue","red","green"), pch = c(19, NA, NA),lty=c(NA,1,1))

### n=100
plot(X_pred,Y_pred,col="blue",pch=19,xlab="X",ylab="Y",main="Représentation des IP à 95% pour n=100")
points(X_pred,IP_Ynonpara_100[,1],type='l',col="red")
points(X_pred,IP_Ynonpara_100[,2],type='l',col='red')
points(X_pred,IP_Ypara_100[,1],type='l',col="green")
points(X_pred,IP_Ypara_100[,2],type='l',col='green')
legend("topleft", legend = c("Vraies valeurs","IP bootstrap non paramétrique","IP bootstrap paramétrique"), col = c("blue","red","green"), pch = c(19, NA, NA),lty=c(NA,1,1))



############## IC bca ##############
X <-X_n_100[,1]
Y <- Y_100

model.boot <- function(data, indices) {
  X <- data$X[indices]
  Y <- data$Y[indices]
  fit <- lm(Y ~ X + I(X^2) + I(X^3))
  return(coef(fit))
}
data <- data.frame(X = X, Y = Y)
results <- boot(data=data,statistic=model.boot,R=2000)

# Intervalle de confiance bootstrap à 95% pour chaque coefficient
boot.ci(results,index=1,type="bca",conf=0.95)
boot.ci(results,index=2,type="bca",conf=0.95)
boot.ci(results,index=3,type="bca",conf=0.95)
boot.ci(results,index=4,type="bca",conf=0.95)

var.boot <- function(data, indices) {
  X <- data$X[indices]
  Y <- data$Y[indices]
  fit <- lm(Y ~ X + I(X^2) + I(X^3))
  return(summary(fit)$sigma^2)
}
results.var <- boot(data=data,statistic=var.boot,R=2000)
boot.ci(results.var,type="bca",conf=0.95)