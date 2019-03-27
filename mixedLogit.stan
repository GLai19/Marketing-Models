
data{
 
  int<lower=0> NOBS; // number of observations
  int<lower=0> NPAR; // number of parameters
  int<lower=0> NBRANDS; // number of brands
  int<lower=0> NIND; // number of individuals
 
  matrix[NOBS, NPAR] x1mat;
  matrix[NOBS, NPAR] x2mat;
  matrix[NOBS, NPAR] x3mat;
 
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
    
    choice[i] ~ categorical(softmax(utils));
  }
}




 
