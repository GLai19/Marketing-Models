library(rstan)
rstan_options(auto_write = TRUE)

MovieRatingData <- read.csv("/Users/guan-hunglai/Desktop/Homework2 Instructions/MovieRatings.csv",header= TRUE)

nRatings <- 10
nobs <- nrow(MovieRatingData)
npar <- 28
nind<-max(MovieRatingData$rater)

Ratings <- MovieRatingData$rating
Ratings
Half_mat <- cbind(rep(0,nobs), rep(0, nobs), rep(0,nobs), rep(0, nobs), rep(0,nobs), rep(0, nobs),rep(0,nobs), rep(0, nobs), rep(0,nobs), MovieRatingData$action, MovieRatingData$adventure, MovieRatingData$animation, MovieRatingData$children_s, MovieRatingData$comedy, MovieRatingData$crime, MovieRatingData$doc, MovieRatingData$drama, MovieRatingData$fantasy, MovieRatingData$filmnoir, MovieRatingData$horror, MovieRatingData$musical, MovieRatingData$mystery, MovieRatingData$romance, MovieRatingData$scifi, MovieRatingData$thriller, MovieRatingData$war, MovieRatingData$western)
One_mat <- cbind(rep(1,nobs), rep(0, nobs), rep(0,nobs), rep(0, nobs), rep(0,nobs), rep(0, nobs),rep(0,nobs), rep(0, nobs), rep(0,nobs), MovieRatingData$action, MovieRatingData$adventure, MovieRatingData$animation, MovieRatingData$children_s, MovieRatingData$comedy, MovieRatingData$crime, MovieRatingData$doc, MovieRatingData$drama, MovieRatingData$fantasy, MovieRatingData$filmnoir, MovieRatingData$horror, MovieRatingData$musical, MovieRatingData$mystery, MovieRatingData$romance, MovieRatingData$scifi, MovieRatingData$thriller, MovieRatingData$war, MovieRatingData$western)
OneHalf_mat <- cbind(rep(0,nobs), rep(1, nobs), rep(0,nobs), rep(0, nobs), rep(0,nobs), rep(0, nobs),rep(0,nobs), rep(0, nobs), rep(0,nobs), MovieRatingData$action, MovieRatingData$adventure, MovieRatingData$animation, MovieRatingData$children_s, MovieRatingData$comedy, MovieRatingData$crime, MovieRatingData$doc, MovieRatingData$drama, MovieRatingData$fantasy, MovieRatingData$filmnoir, MovieRatingData$horror, MovieRatingData$musical, MovieRatingData$mystery, MovieRatingData$romance, MovieRatingData$scifi, MovieRatingData$thriller, MovieRatingData$war, MovieRatingData$western)
Two_mat <- cbind(rep(0,nobs), rep(0, nobs), rep(1,nobs), rep(0, nobs), rep(0,nobs), rep(0, nobs),rep(0,nobs), rep(0, nobs), rep(0,nobs), MovieRatingData$action, MovieRatingData$adventure, MovieRatingData$animation, MovieRatingData$children_s, MovieRatingData$comedy, MovieRatingData$crime, MovieRatingData$doc, MovieRatingData$drama, MovieRatingData$fantasy, MovieRatingData$filmnoir, MovieRatingData$horror, MovieRatingData$musical, MovieRatingData$mystery, MovieRatingData$romance, MovieRatingData$scifi, MovieRatingData$thriller, MovieRatingData$war, MovieRatingData$western)
TwoHalf_mat <- cbind(rep(0,nobs), rep(0, nobs), rep(0,nobs), rep(1, nobs), rep(0,nobs), rep(0, nobs),rep(0,nobs), rep(0, nobs), rep(0,nobs), MovieRatingData$action, MovieRatingData$adventure, MovieRatingData$animation, MovieRatingData$children_s, MovieRatingData$comedy, MovieRatingData$crime, MovieRatingData$doc, MovieRatingData$drama, MovieRatingData$fantasy, MovieRatingData$filmnoir, MovieRatingData$horror, MovieRatingData$musical, MovieRatingData$mystery, MovieRatingData$romance, MovieRatingData$scifi, MovieRatingData$thriller, MovieRatingData$war, MovieRatingData$western)
Three_mat <- cbind(rep(0,nobs), rep(0, nobs), rep(0,nobs), rep(0, nobs), rep(1,nobs), rep(0, nobs),rep(0,nobs), rep(0, nobs), rep(0,nobs), MovieRatingData$action, MovieRatingData$adventure, MovieRatingData$animation, MovieRatingData$children_s, MovieRatingData$comedy, MovieRatingData$crime, MovieRatingData$doc, MovieRatingData$drama, MovieRatingData$fantasy, MovieRatingData$filmnoir, MovieRatingData$horror, MovieRatingData$musical, MovieRatingData$mystery, MovieRatingData$romance, MovieRatingData$scifi, MovieRatingData$thriller, MovieRatingData$war, MovieRatingData$western)
ThreeHalf_mat <- cbind(rep(0,nobs), rep(0, nobs), rep(0,nobs), rep(0, nobs), rep(0,nobs), rep(1, nobs),rep(0,nobs), rep(0, nobs), rep(0,nobs), MovieRatingData$action, MovieRatingData$adventure, MovieRatingData$animation, MovieRatingData$children_s, MovieRatingData$comedy, MovieRatingData$crime, MovieRatingData$doc, MovieRatingData$drama, MovieRatingData$fantasy, MovieRatingData$filmnoir, MovieRatingData$horror, MovieRatingData$musical, MovieRatingData$mystery, MovieRatingData$romance, MovieRatingData$scifi, MovieRatingData$thriller, MovieRatingData$war, MovieRatingData$western)
Four_mat <- cbind(rep(0,nobs), rep(0, nobs), rep(0,nobs), rep(0, nobs), rep(0,nobs), rep(0, nobs),rep(1,nobs), rep(0, nobs), rep(0,nobs), MovieRatingData$action, MovieRatingData$adventure, MovieRatingData$animation, MovieRatingData$children_s, MovieRatingData$comedy, MovieRatingData$crime, MovieRatingData$doc, MovieRatingData$drama, MovieRatingData$fantasy, MovieRatingData$filmnoir, MovieRatingData$horror, MovieRatingData$musical, MovieRatingData$mystery, MovieRatingData$romance, MovieRatingData$scifi, MovieRatingData$thriller, MovieRatingData$war, MovieRatingData$western)
FourHalf_mat <- cbind(rep(0,nobs), rep(0, nobs), rep(0,nobs), rep(0, nobs), rep(0,nobs), rep(0, nobs),rep(0,nobs), rep(1, nobs), rep(0,nobs), MovieRatingData$action, MovieRatingData$adventure, MovieRatingData$animation, MovieRatingData$children_s, MovieRatingData$comedy, MovieRatingData$crime, MovieRatingData$doc, MovieRatingData$drama, MovieRatingData$fantasy, MovieRatingData$filmnoir, MovieRatingData$horror, MovieRatingData$musical, MovieRatingData$mystery, MovieRatingData$romance, MovieRatingData$scifi, MovieRatingData$thriller, MovieRatingData$war, MovieRatingData$western)
Five_mat <- cbind(rep(0,nobs), rep(0, nobs), rep(0,nobs), rep(0, nobs), rep(0,nobs), rep(0, nobs),rep(0,nobs), rep(0, nobs), rep(1,nobs), MovieRatingData$action, MovieRatingData$adventure, MovieRatingData$animation, MovieRatingData$children_s, MovieRatingData$comedy, MovieRatingData$crime, MovieRatingData$doc, MovieRatingData$drama, MovieRatingData$fantasy, MovieRatingData$filmnoir, MovieRatingData$horror, MovieRatingData$musical, MovieRatingData$mystery, MovieRatingData$romance, MovieRatingData$scifi, MovieRatingData$thriller, MovieRatingData$war, MovieRatingData$western)

library(parallel)
options(mc.cores=detectCores()-2)
data_list <- list (NOBS = nobs, NPAR = npar, NBRANDS=nRatings,
                   NIND = nind, 
                   x1mat = Half_mat, x2mat= One_mat, x3mat = OneHalf_mat, x4mat = Two_mat, x5mat = TwoHalf_mat,
                   x6mat = Three_mat, x7mat = ThreeHalf_mat, x8mat = Four_mat, x9mat = FourHalf_mat, x10mat = Five_mat,
                   choice = Ratings, customer = MovieRatingData$rater
                   )





fit <- stan(file = "/Users/guan-hunglai/Desktop/Marketing Models HW 2/mixedLogitDiscrete.stan", data = data_list, warmup = 500, iter = 1000, chains = 2)
