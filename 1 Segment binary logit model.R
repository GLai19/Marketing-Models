CustomerChoiceData <- read.csv("/Users/guan-hunglai/Desktop/Homework2 Instructions/LogisticMixture.csv", header = TRUE)
summary(CustomerChoiceData)
ID <- CustomerChoiceData$id
nind=max(ID)
Xmatrix <- as.matrix((cbind(1, CustomerChoiceData[3:4])))
Y <- as.vector(CustomerChoiceData[,2])
library(Rlab)


##Fitting a single segment binary logit model
library(maxLik)
loglik <- function(parms, Y, Xmatrix, ID){
  
  xbeta1 <- Xmatrix %*% parms[1:3]
  ll <- 0.0
  for (i in 1:max(ID)){
    sxbeta1 <- subset(xbeta1, ID==i)
    sy <- subset(Y, ID==i)
    sum1 <- sum(dbern(sy, prob = (exp(sxbeta1)/(1+exp(sxbeta1))), log = TRUE))
    ll <- ll + sum1
  }
  ll
}

parms <- runif(3)
mlfit <- maxLik(loglik, start = c("b11"= parms[1], "b12" = parms[2], "b13" = parms[3]), 
                Y = Y, Xmatrix = Xmatrix, ID = ID, method = "BFGS")
summary(mlfit)





### A Different method
negloglike <- function(parms, Y, Xmatrix, ID){
  -loglik(parms, Y, Xmatrix, ID)
}

mle1 <- nlm(negloglike, parms, Y = Y, Xmatrix=Xmatrix, ID=ID, hessian = T)

summary(mle1)


### Constructing the parameter table
library(data.table)
library(knitr)
library(kableExtra)

mode = mle1$estimate
se = sqrt(diag(solve(mle1$hessian)))
tValue = mode/se
ll = mle1$minimum
names <- c("b11","b12","b13")
parTable = data.table(Parameter = names, Estimate = mode, Se = se, tValue = tValue)
parTable %>% kable(digits = 3) %>% kable_styling(bootstrap_options = "striped", full_width = F)
                         
###Compute BIC Statistics (1 segment)
### BIC = log(n)P - 2log(L)
#P = # of parameters
#n = # of datapoints/observations
#L = the maximized value of the likelihood function of the model 

BIC_1st_Seg <- log(3765)*3 - 2*-1831.667 
BIC_1st_Seg 
#BIC Statistics for segment 1, according to the above formula is around 3704.502


###Regression Results

lsfit <- lm(CustomerChoiceData$id ~ CustomerChoiceData$Price)
summary(lsfit)
