CustomerChoiceData <- read.csv("/Users/guan-hunglai/Desktop/Homework2 Instructions/LogisticMixture.csv", header = TRUE)
summary(CustomerChoiceData)
ID <- CustomerChoiceData$id
nind=max(ID)
Xmatrix <- as.matrix((cbind(1, CustomerChoiceData[3:4])))
Y <- as.vector(CustomerChoiceData[,2])
library(Rlab)


###Fitting a two segment binary logit model
loglik2 <- function(parms, Y, Xmatrix, ID){
  
  wt <- exp(parms[7])/(1+exp(parms[7]))
  xbeta1 <- Xmatrix%*%parms[1:3]; 
  xbeta2 <- Xmatrix%*%parms[4:6]
  
  ll <- 0.0
  
  for (i in 1:max(ID)){
    sxbeta1 <- subset(xbeta1, ID==i); 
    sxbeta2 <- subset(xbeta2, ID==i)
    sy <- subset(Y, ID==i)
    
    sum1 <- sum(dbern(sy, prob = (exp(sxbeta1)/(1 + exp(sxbeta1))), log = TRUE))
    sum2 <- sum(dbern(sy, prob = (exp(sxbeta2)/(1 + exp(sxbeta2))), log = TRUE))
    
    ws1 <- sum1 + log(wt); 
    ws2 <- sum2 + log(1 - wt)
    
    maxws = max(ws1, ws2)
    
    ll <- ll + maxws + log(exp(ws1-maxws) + exp(ws2-maxws))
  }
  ll
}

set.seed(1996)
parms <- runif(7)
mlfit <- maxLik(loglik2, start = c("beta11"= parms[1], "beta12" = parms[2], "beta13" = parms[3], 
                                   "beta21" = parms[4], "beta22" = parms[5], "beta23" = parms[6],
                                   "sigma" = parms[7]), Y = Y, Xmatrix = Xmatrix, ID = ID, method = "BFGS")

summary(mlfit)



###Get Negative Loglikelihood (Since regular likelihood doesn't run)
negloglike <- function(parms, Y, Xmatrix, ID){
  -loglik2(parms, Y, Xmatrix, ID)
}

mle <- nlm(negloglike, parms, Y =Y, Xmatrix=Xmatrix, ID=ID, hessian = T)

summary(mle)

mle$hessian
###Getting the table
library(data.table)
library(knitr)
library(kableExtra)

mode = mle$estimate
se = sqrt(diag(solve(mle$hessian)))
tValue = mode/se
ll = mle$minimum
ll
names <- c("b11","b12","b13","b21","b22","b23","sigma")
parTable2 = data.table(Parameter = names, Estimate = mode, Se = se, tValue = tValue)
parTable2 %>% kable(digits = 3) %>% kable_styling(bootstrap_options = "striped", full_width = F)

###Compute BIC Statistics (2 S egments)
### BIC = ln(n)P - 2ln(L)
#P = # of parameters
#n = # of datapoints/observations
#L = the maximized value of the likelihood function of the model 

BIC_2nd_Seg <- log(3765)*7 - 2*-1643.861 
BIC_2nd_Seg
#The BIC Statistics, according to the above formula, is 3353.59

###Posterior Probabilities
PosteriorProb <- function(parms, Y, Xmatrix, ID){
  nind <- max(ID)
  wt <- exp(parms[7])/(1+exp(parms[7]))
  xbeta1 <- Xmatrix%*%parms[1:3]; 
  xbeta2 <- Xmatrix%*%parms[4:6]
  
  p <- matrix(0.0, nrow=nind, 2)
  
  for(i in 1:nind){
    sxbeta1 <- subset(xbeta1, ID==i); 
    sxbeta2 <- subset(xbeta2, ID==i)
    sy <- subset(Y, ID==i)
    sum1 <- sum(dbern(sy, prob = (exp(sxbeta1)/(1+exp(sxbeta1))), log = TRUE))
    sum2 <- sum(dbern(sy, prob = (exp(sxbeta2)/(1+exp(sxbeta2))), log = TRUE))
    
    p[i,1] <- exp(sum1)*wt
    p[i,2] <- exp(sum2)*(1-wt)
    
    den <- p[i,1] + p[i,2]
    
    p[i,1] <- p[i,1]/den
    p[i,2] <- p[i,2]/den
    
  }
  p
}

p <- PosteriorProb(mode, Y, Xmatrix, ID)
p[1,]
p[,1]+p[,2]

###Regression Summary
lsfit <- lm(CustomerChoiceData$id ~ CustomerChoiceData$Price + CustomerChoiceData$Promotion)
summary(lsfit)
