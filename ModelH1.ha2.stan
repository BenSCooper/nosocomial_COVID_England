//  Model H1.ha2: covariates but no new variant or vaccine  
  data {
    int<lower=1> N;   //number of observations
    int<lower=1> H;   //number of hospitals
    int<lower=1> W;   //number of weeks
    int<lower=1,upper=H> trustindex[N];    //hospital index, starting from 1 
    int<lower=1,upper=W> weekindex[N];    ///week number
    int<lower=0> ha1[N];    // noscomial cases 3 day definition 
    int<lower=0> ha2[N];    // noscomial cases 8 day definition 
    int<lower=0> ha3[N];    // noscomial cases 15 day definition 

    int<lower=0> lag1ha1[N];    // noscomial cases in previous week -  3 day definition 
    int<lower=0> lag1ha2[N];    // noscomial cases in previous week -  8 day definition 
    int<lower=0> lag1ha3[N];    // noscomial cases in previous week -  15 day definition 

    int<lower=0> lag2ha1[N];    // noscomial cases two weeks ago -  3 day definition 
    int<lower=0> lag2ha2[N];    // noscomial cases two weeks ago -  8 day definition 
    int<lower=0> lag2ha3[N];    // noscomial cases two weeks ago -  15 day definition 

    int<lower=0> lag1_ca[N];    //community acquried admission in previous week 
    int<lower=0> lag2_ca[N];    //community acquried admission two weeks ago
    
    int<lower=0> lag1hcw[N];    // hcws infected (imputed) in previous week 

    
    int<lower=0> hcw[N];    //imputed number of new hcw covid cases (imputed from absence data)
    real<lower=0, upper=1> prop_nv[N];    //proportion detected with new variant in the region
    real<lower=0, upper=1> prop_nv_permuted[N];    //proportion detected with new variant but based on permuted regions (for model checking)
    
  }
  
transformed data{
  
  real<lower=0> lag1hcw_primed[N];
  real<lower=0> lag1ha2_primed[N];
  real<lower=0> lag1_ca_primed[N];

  for (n in 1:N){
   lag1hcw_primed[n] = lag1hcw[n]/124.3; 
   lag1ha2_primed[n]  = lag1ha2[n]/15.1;
   lag1_ca_primed[n] = lag1_ca[n]/76.1;
  }
}  

parameters {
//  real<lower=0> a[H]; //intercepts
  real<lower=0> a_raw[H]; //intercepts
  real<lower=0> k_raw[H]; //k terms for neg bin

  real<lower=0> a0; //mean of as 
  real<lower=0> k0; //mean of ks 

  real b0; //  coefficient for lagged community acquired cases 
  real c0; //  coefficient for lagged hospital acquired cases 
  real d0; //  coefficient for imputed lagged HCW cases
  
  // real<lower=0>  phi0;  //overdispersion parameter 
  real<lower=0>  phi0primed;  //1/sqrt(phi), the overdispersion parameter (see https://statmodeling.stat.columbia.edu/2018/04/03/justify-my-love/)
  real<lower=0> sigmasq_a; //intercept variance 
  real<lower=0> sigmasq_k; //k variance 

  
}

transformed parameters {
  real<lower=0> a[H];
  real<lower=0> k[H];

  real<lower=0>  mu[N];  //observation mean
  //real sigma_a = sqrt(sigmasq_a);

  real<lower=0>  phi0; 

  real<lower=0>  phi[N];  //term for modelling overdisperion in neg bin for each observation
  
  
  
  phi0 = 1/phi0primed^2; 
  
  for (i in 1:H){
    a[i] = a0 + a_raw[i]*sigmasq_a;
    k[i] = k0 + k_raw[i]*sigmasq_k;
  }
  
  for (n in 1:N){
   //  mu[n] = a[trustindex[n]] + b0*lag1_ca[n] + c0*lag1ha1[n] +d0*lag1hcw[n] ;
   //this has no mixing problems  mu[n] = a0 + b0*lag1_ca[n] + c0*lag1ha1[n] +d0*lag1hcw[n] ;
   mu[n] = a[trustindex[n]] + b0*lag1_ca_primed[n] + c0*lag1ha2_primed[n] +d0*lag1hcw_primed[n] ;
   phi[n] = lag1hcw[n];
  }
  
}

model { 
  
  a0 ~ normal(0, 1);
  b0 ~ normal(0, 1);
  c0 ~ normal(0, 1);
  d0 ~ normal(0, 1);
  k0 ~ normal(0, 1);
  
  //a ~ normal(a0, sigmasq_a );
  a_raw ~ normal(0, 1);   //for non-centred parameterisation
  k_raw ~ normal(0, 1);   //for non-centred parameterisation

  sigmasq_a ~ normal(0, 1); 
  sigmasq_k ~ normal(0, 1); 
  
  
 // sigmasq_a ~ student_t(3,0,1); //slightly heavier tailed prior 
  phi0primed  ~  normal(0, 0.5);
  for (i in 1:N){
    hcw[i] ~ neg_binomial_2(mu[i], phi0 + k[trustindex[i]]* phi[i]);
  }  
}

generated quantities {
 real ca_coeff;
 real ha_coeff;
 real hcw_coeff;

 vector[N] log_lik;

 for (n in 1:N) log_lik[n] = neg_binomial_2_lpmf( hcw[n]| mu[n],  phi0 + k[trustindex[n]]*phi[n]); 
 
 ca_coeff = b0/76.;
 ha_coeff = c0/15.1;
 hcw_coeff = d0/124.3;

}


  
