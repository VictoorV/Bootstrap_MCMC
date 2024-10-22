# Question 1
X_n_10<-read.table("C:/Users/jmv/Documents/Travail/Master/r/projet_SC/X_n_10.txt",header=TRUE,sep=" ")
X_n_100<-read.table("C:/Users/jmv/Documents/Travail/Master/r/projet_SC/X_n_100.txt",header=TRUE,sep=" ")
theta<-c(20,7,8,2,1.75^2)
Y_10<-rep(0,10)
Y_100<-rep(0,100)

set.seed(2023) # Création des données d'ajustement
err_10<-rnorm(10,0,sqrt(theta[5]))
for (i in 1:10){
  Y_10[i] <- theta[1]+theta[2]*X_n_10[i,1]+theta[3]*X_n_10[i,1]^2+theta[4]*X_n_10[i,1]^3+err_10[i]
}

err_100<-rnorm(100,0,sqrt(theta[5]))
for (i in 1:100){
  Y_100[i] <- theta[1]+theta[2]*X_n_100[i,1]+theta[3]*X_n_100[i,1]^2+theta[4]*X_n_100[i,1]^3+err_100[i]
}

plot(X_n_10[,1],Y_10,col="blue",pch=19,xlab="X",ylab="Y",main="Y_10 en fonction de X_n_10")
plot(X_n_100[,1],Y_100,col="blue",pch=19,xlab="X",ylab="Y",main="Y_100 en fonction de X_n_100")

# Question 2
# n=10
model_emv_10 <- lm(Y_10 ~ X_n_10[,1] + I(X_n_10[,1]^2) + I(X_n_10[,1]^3))
summary(model_emv_10)
poly_emv_10 <- function(x){
  coef = model_emv_10$coefficients
  return(coef[1]+coef[2]*x+coef[3]*x^2+coef[4]*x^3)
}
sigma <- summary(model_emv_10)$sigma
lower_10 <- 6*sigma^2 / qchisq(0.975,6) 
upper_10 <- 6*sigma^2 / qchisq(0.025,6)
IC_emv_10 <-confint(model_emv_10,level=0.95)
IC_emv_10 <-rbind(IC_emv_10,c(lower_10,upper_10))
row.names(IC_emv_10) <- c("a", "b", "c", "d", "sigma^2")
head(IC_emv_10)

# n=100
model_emv_100 <- lm(Y_100 ~ X_n_100[,1] + I(X_n_100[,1]^2) + I(X_n_100[,1]^3))
summary(model_emv_100)
poly_emv_100 <- function(x){
  coef = model_emv_100$coefficients
  return(coef[1]+coef[2]*x+coef[3]*x^2+coef[4]*x^3)
}
sigma <- summary(model_emv_100)$sigma
lower_100 <- 96*sigma^2 / qchisq(0.975,96) 
upper_100 <- 96*sigma^2 / qchisq(0.025,96)
IC_emv_100 <-confint(model_emv_100,level=0.95)
IC_emv_100 <-rbind(IC_emv_100,c(lower_100,upper_100))
row.names(IC_emv_100) <- c("a", "b", "c", "d", "sigma^2")
head(IC_emv_100)

# Question 3 
X_pred<-seq(-3,1,length.out = 50)
err_pred <- rnorm(50,0,theta[5])
Y_pred <- theta[1]+theta[2]*X_pred+theta[3]*X_pred^2+theta[4]*X_pred^3+err_pred

Y1_pred <- poly_emv_10(X_pred)
Y2_pred <- poly_emv_100(X_pred)

plot(X_pred,Y_pred,col="blue",pch=19,xlab="X",ylab="Y",main="Graphe des vraies valeurs/valeurs prédites pour n=10")
points(X_pred,Y1_pred,pch=19,col="red")
legend("topleft", legend = c("Vraies valeurs","Ŷ_pred"), col = c("blue","red"), pch = c(19, 19))

plot(X_pred,Y_pred,col="blue",pch=19,xlab="X",ylab="Y",main="Graphe des vraies valeurs/valeurs prédites pour n=100")
points(X_pred,Y2_pred,pch=19,col="red")
legend("topleft", legend = c("Vraies valeurs","Ŷ_pred"), col = c("blue","red"), pch = c(19, 19))

# Question 4
B<-2000
# n=10
res_boot_10<-data.frame(matrix(nrow = B, ncol = 5)) # Pour stocker les résultats de chaque échantillon bootstrap non paramétrique (n=10)
colnames(res_boot_10)<-c("a","b","c","d","sigma^2")
for(i in 1:B){
  sample <- sample(c(1:10),10,replace=TRUE)
  model_boot <- lm(Y_10[sample] ~ X_n_10[,1][sample] + I(X_n_10[,1][sample]^2) + I(X_n_10[,1][sample]^3))
  res_boot_10[i,]<-c(model_boot[["coefficients"]],(summary(model_boot)$sigma)^2)
}
poly_nonpara_10 <- function(x){
  return(mean(res_boot_10[,1][!is.na(res_boot_10[,1])])+mean(res_boot_10[,2][!is.na(res_boot_10[,2])])*x+mean(res_boot_10[,3][!is.na(res_boot_10[,3])])*x^2+mean(res_boot_10[,4][!is.na(res_boot_10[,4])])*x^3)
}

plot(X_pred,Y_pred,col="blue",pch=19,xlab="X",ylab="Y",main="Bootstrap non paramétrique pour n=10")
points(X_pred,poly_nonpara_10(X_pred),pch=19,col="red")
legend("topleft", legend = c("Vraies valeurs", "Ŷ_pred_nonpara"), col = c("blue", "red"), pch = c(19, 19))

IC_boot_10<-data.frame(matrix(nrow = 5, ncol = 2))  # IC 95% bootstrap non paramétrique pour theta (n=10)
IC_boot_10[1,1]<-2*model_emv_10[["coefficients"]][1]-sort(res_boot_10[,1])[ceiling(B*(1-0.05/2))]
IC_boot_10[1,2]<-2*model_emv_10[["coefficients"]][1]-sort(res_boot_10[,1])[ceiling(B*(0.05/2))]
IC_boot_10[2,1]<-2*model_emv_10[["coefficients"]][2]-sort(res_boot_10[,2])[ceiling(B*(1-0.05/2))]
IC_boot_10[2,2]<-2*model_emv_10[["coefficients"]][2]-sort(res_boot_10[,2])[ceiling(B*(0.05/2))]
IC_boot_10[3,1]<-2*model_emv_10[["coefficients"]][3]-sort(res_boot_10[,3])[ceiling(B*(1-0.05/2))]
IC_boot_10[3,2]<-2*model_emv_10[["coefficients"]][3]-sort(res_boot_10[,3])[ceiling(B*(0.05/2))]
IC_boot_10[4,1]<-2*model_emv_10[["coefficients"]][4]-sort(res_boot_10[,4])[ceiling(B*(1-0.05/2))]
IC_boot_10[4,2]<-2*model_emv_10[["coefficients"]][4]-sort(res_boot_10[,4])[ceiling(B*(0.05/2))]
IC_boot_10[5,1]<-2*(summary(model_emv_10)$sigma)^2-sort(res_boot_10[,5])[ceiling(B*(1-0.05/2))]
IC_boot_10[5,2]<-2*(summary(model_emv_10)$sigma)^2-sort(res_boot_10[,5])[ceiling(B*(0.05/2))]
rownames(IC_boot_10)<-c("a","b","c","d","sigma^2")
colnames(IC_boot_10)<-c("2.5%","97.5%")
head(IC_boot_10)










# n=100
res_boot_100<-data.frame(matrix(nrow = B, ncol = 5)) # Pour stocker les résultats de chaque échantillon bootstrap non paramétrique (n=10)
colnames(res_boot_100)<-c("a","b","c","d","sigma^2")
for(i in 1:B){
  sample <- sample(c(1:100),100,replace=TRUE)
  model_boot <- lm(Y_100[sample] ~ X_n_100[,1][sample] + I(X_n_100[,1][sample]^2) + I(X_n_100[,1][sample]^3))
  res_boot_100[i,]<-c(model_boot[["coefficients"]],(summary(model_boot)$sigma)^2)
}
poly_nonpara_100 <- function(x){
  return(mean(res_boot_100[,1][!is.na(res_boot_100[,1])])+mean(res_boot_100[,2][!is.na(res_boot_100[,2])])*x+mean(res_boot_100[,3][!is.na(res_boot_100[,3])])*x^2+mean(res_boot_100[,4][!is.na(res_boot_100[,4])])*x^3)
}

plot(X_pred,Y_pred,col="blue",pch=19,xlab="X",ylab="Y",main="Bootstrap non paramétrique pour n=10")
points(X_pred,poly_nonpara_100(X_pred),pch=19,col="red")
legend("topleft", legend = c("Vraies valeurs", "Ŷ_pred_nonpara"), col = c("blue", "red"), pch = c(19, 19))

IC_boot_100<-data.frame(matrix(nrow = 5, ncol = 2))  # IC 95% bootstrap non paramétrique pour theta (n=10)
IC_boot_100[1,1]<-2*model_emv_100[["coefficients"]][1]-sort(res_boot_100[,1])[ceiling(B*(1-0.05/2))]
IC_boot_100[1,2]<-2*model_emv_100[["coefficients"]][1]-sort(res_boot_100[,1])[ceiling(B*(0.05/2))]
IC_boot_100[2,1]<-2*model_emv_100[["coefficients"]][2]-sort(res_boot_100[,2])[ceiling(B*(1-0.05/2))]
IC_boot_100[2,2]<-2*model_emv_100[["coefficients"]][2]-sort(res_boot_100[,2])[ceiling(B*(0.05/2))]
IC_boot_100[3,1]<-2*model_emv_100[["coefficients"]][3]-sort(res_boot_100[,3])[ceiling(B*(1-0.05/2))]
IC_boot_100[3,2]<-2*model_emv_100[["coefficients"]][3]-sort(res_boot_100[,3])[ceiling(B*(0.05/2))]
IC_boot_100[4,1]<-2*model_emv_100[["coefficients"]][4]-sort(res_boot_100[,4])[ceiling(B*(1-0.05/2))]
IC_boot_100[4,2]<-2*model_emv_100[["coefficients"]][4]-sort(res_boot_100[,4])[ceiling(B*(0.05/2))]
IC_boot_100[5,1]<-2*(summary(model_emv_100)$sigma)^2-sort(res_boot_100[,5])[ceiling(B*(1-0.05/2))]
IC_boot_100[5,2]<-2*(summary(model_emv_100)$sigma)^2-sort(res_boot_100[,5])[ceiling(B*(0.05/2))]
rownames(IC_boot_100)<-c("a","b","c","d","sigma^2")
colnames(IC_boot_100)<-c("2.5%","97.5%")
head(IC_boot_100)
