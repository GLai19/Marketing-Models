
data{
 
  int<lower=0> NOBS; // number of observations
  int<lower=0> NPAR; // number of parameters
  int<lower=0> NBRANDS; // number of brands
  int<lower=0> NIND; // number of individuals
 
  matrix[NOBS, NPAR] x1mat;
  matrix[NOBS, NPAR] x2mat;
  matrix[NOBS, NPAR] x3mat;
  matrix[NOBS, NPAR] x4mat;
  matrix[NOBS, NPAR] x5mat;
  matrix[NOBS, NPAR] x6mat;
  matrix[NOBS, NPAR] x7mat;
  matrix[NOBS, NPAR] x8mat;
  matrix[NOBS, NPAR] x9mat;
  matrix[NOBS, NPAR] x10mat;
  
 
  int<lower=0> choice[NOBS];
  int<lower=1> customer[NOBS];
}

parameters{
 
  matrix[NPAR, NIND] z;
  vector[NPAR] mu;
  
  cholesky_factor_corr[NPAR] L_Omega;
  
  vector<lower=0>[NPAR] tau;
  
}

transformed parameters {
 matrix[NIND, NPAR] lambda;
 lambda=(diag_pre_multiply(tau, L_Omega)*z)'; 
}

model{
  vector[NBRANDS] utils;
  vector[NBRANDS] probs;
  vector[NPAR] beta_cust;

  to_vector(z) ~ normal(0,1);
  L_Omega ~ lkj_corr_cholesky(2);

  mu ~ normal(0,10);

  tau ~ normal(0,10);
  
  for(i in 1:NOBS){

    beta_cust=mu+lambda[customer[i]]';

    utils[1] = dot_product(x1mat[i], beta_cust);
    utils[2] = dot_product(x2mat[i], beta_cust);
    utils[3] = dot_product(x3mat[i], beta_cust);
    utils[4] = dot_product(x4mat[i], beta_cust);
    utils[5] = dot_product(x5mat[i], beta_cust);
    utils[6] = dot_product(x6mat[i], beta_cust);
    utils[7] = dot_product(x7mat[i], beta_cust);
    utils[8] = dot_product(x8mat[i], beta_cust);
    utils[9] = dot_product(x9mat[i], beta_cust);
    utils[10] = dot_product(x10mat[i], beta_cust);
    
    
    choice[i] ~ categorical(softmax(utils));
  }
}




 
