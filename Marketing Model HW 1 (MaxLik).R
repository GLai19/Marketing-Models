install.packages("evd")
library(evd)
?rgev

#Generate 1050 random draws frim a Gumbel Dis
set.seed(1990)
RandomGumbel <- rgumbel(1050, loc = 1, scale = 1.2)

mean(RandomGumbel) #mean = 1.66
var(RandomGumbel)  #variance = 2.31

#Histogram of RandomGumbel
 hist(RandomGumbel)


#Question 4: Estimating a MNL using maxLik
library(maxLik)
TunaData <- read.csv("/Users/guan-hunglai/Desktop/Marketing Models/HW1/tunacalib-2.csv", header = TRUE)[,-2] #specify -2 to omit 2nd column
summary(TunaData)
counts <- table(TunaData$brand)
NumBrands <- length(counts)
barplot(counts, main = "Brand Distribution", xlab = "Brand", col = "goldenrod")


nobs<-nrow(TunaData)
##Set it equal to 1 when selected brand is chosen, 1 brand will have all 0s because it is the base brand
delta <- matrix(0.0, nrow=nrow(TunaData), ncol=NumBrands)
for (i in 1:nrow(TunaData)) { 
  delta[i, TunaData$brand[i]] <- 1
}

Brand_1_mat <- cbind(rep(0,nobs), rep(0, nobs), rep(0, nobs), rep(0, nobs), 
                     TunaData$p1, TunaData$f1, TunaData$d1)

Brand_2_mat <- cbind(rep(1,nobs), rep(0, nobs), rep(0, nobs), rep(0, nobs),
                     TunaData$p2, TunaData$f2, TunaData$d2)

Brand_3_mat <- cbind(rep(0,nobs), rep(1, nobs), rep(0, nobs), rep(0, nobs),
                     TunaData$p3, TunaData$f3, TunaData$d3)

Brand_4_mat <- cbind(rep(0,nobs), rep(0, nobs), rep(1, nobs), rep(0, nobs),
                     TunaData$p4, TunaData$f4, TunaData$d4)

Brand_5_mat <- cbind(rep(0,nobs), rep(0, nobs), rep(0, nobs), rep(1, nobs),
                     TunaData$p5, TunaData$f5, TunaData$d5)

logLik<-function(parms, delta, Brand_1_mat, Brand_2_mat, Brand_3_mat, Brand_4_mat, Brand_5_mat){
  
  v <-matrix(0, dim(delta)[1], dim(delta)[2])
  
  v[,1] <- Brand_1_mat %*% parms
  v[,2] <- Brand_2_mat %*% parms
  v[,3] <- Brand_3_mat %*% parms
  v[,4] <- Brand_4_mat %*% parms
  v[,5] <- Brand_5_mat %*% parms
  
  lden <- log(exp(v[,1])+exp(v[,2])+exp(v[,3])+exp(v[,4])+exp(v[,5]))
  sum(apply(v*delta,1,sum)-lden)
}


parms <- runif(7)
mlfit<-maxLik(logLik, start=c(Int2=parms[1], Int3=parms[2], Int4=parms[3], Int5=parms[4], 
                              Price=parms[5], Feature=parms[6], Display=parms[7]), 
              delta=delta, Brand_1_mat=Brand_1_mat, Brand_2_mat=Brand_2_mat, Brand_3_mat=Brand_3_mat, Brand_4_mat=Brand_4_mat, Brand_5_mat=Brand_5_mat)

summary(mlfit)

###Estimating own and cross-price elasticity matrix
#install.packages("mlogit")
#library(mlogit)

ml_parms<-mlfit$estimate
ml_parms

true_v<-c(0,0,0,0,0)
true_v[1] <- mean(Brand_1_mat %*% ml_parms)
true_v[2] <- mean(Brand_2_mat %*% ml_parms)
true_v[3] <- mean(Brand_3_mat %*% ml_parms)
true_v[4] <- mean(Brand_4_mat %*% ml_parms)
true_v[5] <- mean(Brand_5_mat %*% ml_parms)


log_prob_choose<-matrix(0.0,nrow=1,ncol=NumBrands)
for (i in 1:length(log_prob_choose)){
  log_prob_choose[i]<-true_v[i]-log(sum(exp(true_v)))
} 

prob_choose<-exp(log_prob_choose)
prob_choose

price=c(mean(TunaData$p1),mean(TunaData$p2),mean(TunaData$p3),mean(TunaData$p4),mean(TunaData$p5))
price

set.seed(1996)
elasticitymatrix<-matrix(0.0,nrow=NumBrands,ncol=NumBrands)
for (i in 1:nrow(elasticitymatrix)){
  for (j in 1:ncol(elasticitymatrix)){
    elasticitymatrix[i,j]<--mlfit$estimate[5]*price[j]*prob_choose[j]
  }}
for (i in 1:nrow(elasticitymatrix)){
  elasticitymatrix[i,i]<-mlfit$estimate[5]*price[i]*(1-prob_choose[i])
}
elasticitymatrix

colnames(elasticitymatrix) = c("Brand 1", "Brand 2","Brand 3","Brand 4", "Brand 5")
rownames(elasticitymatrix) = c("Brand 1", "Brand 2","Brand 3","Brand 4", "Brand 5")


###Getting mean own and cross price elastictiy
avg_own_elasticities<-mean(diag(elasmatrix))
avg_own_elasticities
avg_cross_elasticities<-mean(elasmatrix[2,1],elasmatrix[3,2],elasmatrix[4,3],elasmatrix[5,4],elasmatrix[1,5])
avg_cross_elasticities






#####SOME EXTRA TESTS

mlfit$estimate[5]

Own_Price <- matrix(0.0, nrow(TunaData), ncol=NumBrands)
for (i in 1:nrow(TunaData)) {
  Own_Price[i,1] <- -11.16082 *TunaData$p1[i]*(1-prob_choose[1])
  Own_Price[i,2] <- -11.16082 *TunaData$p2[i]*(1-prob_choose[2])
  Own_Price[i,3] <- -11.16082 *TunaData$p3[i]*(1-prob_choose[3])
  Own_Price[i,4] <- -11.16082 *TunaData$p4[i]*(1-prob_choose[4])
  Own_Price[i,5] <- -11.16082 *TunaData$p5[i]*(1-prob_choose[5])
}
Own_Price_Elasticity = NULL
for(i in 1:5){
  Own_Price_Elasticity[i] = mean(Own_Price[,i])
}

Own_Price_Elasticity[]
for(i in 1:5){
  elasticity_matrix[i,i] = Own_Price_Elasticity
}