//  Model P3_tv1:  trust covariates and new variant plus vaccine - but with time varying intercepts
//  implemented using a spline

functions {
  vector build_b_spline(real[] t, real[] ext_knots, int ind, int order);
  vector build_b_spline(real[] t, real[] ext_knots, int ind, int order) {
    // INPUTS:
      //    t:          the points at which the b_spline is calculated
    //    ext_knots:  the set of extended knots
    //    ind:        the index of the b_spline
    //    order:      the order of the b-spline
    vector[size(t)] b_spline;
    vector[size(t)] w1 = rep_vector(0, size(t));
    vector[size(t)] w2 = rep_vector(0, size(t));
    if (order==1)
      for (i in 1:size(t)) // B-splines of order 1 are piece-wise constant
    b_spline[i] = (ext_knots[ind] <= t[i]) && (t[i] < ext_knots[ind+1]);
    else {
      if (ext_knots[ind] != ext_knots[ind+order-1])
        w1 = (to_vector(t) - rep_vector(ext_knots[ind], size(t))) /
          (ext_knots[ind+order-1] - ext_knots[ind]);
      if (ext_knots[ind+1] != ext_knots[ind+order])
        w2 = 1 - (to_vector(t) - rep_vector(ext_knots[ind+1], size(t))) /
          (ext_knots[ind+order] - ext_knots[ind+1]);
      // Calculating the B-spline recursively as linear interpolation of two lower-order splines
      b_spline = w1 .* build_b_spline(t, ext_knots, ind, order-1) +
        w2 .* build_b_spline(t, ext_knots, ind+1, order-1);
    }
    return b_spline;
  }
}
 
data {
    int num_knots;            // num of knots
    vector[num_knots] knots;  // the sequence of knots
    int spline_degree;        // the degree of spline (is equal to order - 1)
    int<lower=1> N;   //number of observations
    int<lower=1> H;   //number of hospitals
    int<lower=1> W;   //number of weeks
    real<lower=1> week[W];  // week numbers (needed for splines)

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
    int<lower=0> lag1hcw_s[N];    // smoothed  hcws infected (imputed) in previous week 

    
    int<lower=0> hcw[N];    //imputed number of new hcw covid cases (imputed from absence data)
    real<lower=0, upper=1> prop_nv[N];    //proportion detected with new variant in the region
    real<lower=0, upper=1> prop_nv_permuted[N];    //proportion detected with new variant but based on permuted regions (for model checking)
    real<lower=0, upper=1> hcw_vax_plus3[N];  //proportion of HCWs in the trust who had been vaccinated three weeks ago

    real proportion_single_rooms_std[N]; //standardized proportion of gen and acute beds in trust that are single room
    real occupancy_std[N]; //standardized bed occupancy
    real percent_pre1965_std[N];
    real trustsize_std[N];
    real trustvolperbed_std[N]; ////standardized heated volume per bed

  }

  
transformed data{
  
  real<lower=0> lag1hcw_primed[N];
  real<lower=0> lag1ha2_primed[N];
  real<lower=0> lag1_ca_primed[N];
  
  // spline stuff
  int num_basis = num_knots + spline_degree - 1; // total number of B-splines
  matrix[num_basis, W] B;  // matrix of B-splines
  vector[spline_degree + num_knots] ext_knots_temp;
  vector[2*spline_degree + num_knots] ext_knots; // set of extended knots
  ext_knots_temp = append_row(rep_vector(knots[1], spline_degree), knots);
  ext_knots = append_row(ext_knots_temp, rep_vector(knots[num_knots], spline_degree));
  for (ind in 1:num_basis)  
  B[ind,:] = to_row_vector(build_b_spline(week, to_array_1d(ext_knots), ind, spline_degree + 1));
  B[num_knots + spline_degree - 1, W] = 1;
  

  for (n in 1:N){
   lag1hcw_primed[n] = lag1hcw_s[n]/124.3; 
   lag1ha2_primed[n]  = lag1ha2[n]/15.1;
   lag1_ca_primed[n] = lag1_ca[n]/76.1;
  }
}  

parameters {
  row_vector[num_basis] a_spl_raw;
  real<lower=0> tau;

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
  real v0;
  real nv0;
  real sr0 ;
  real sizetrust0;
  real occ0;
  real agetrust0;
  real vol;
}

transformed parameters {
  row_vector[num_basis] spl_coeffs; // spline coefficients
  vector[W] spline_component;
  
  real<lower=0> a[H];
  real<lower=0> k[H];

  real<lower=0>  mu[N];  //observation mean
  //real sigma_a = sqrt(sigmasq_a);

  real<lower=0>  phi0; 

  real<lower=0>  phi[N];  //term for modelling overdisperion in neg bin for each observation
  
  spl_coeffs = a_spl_raw*tau;
  spline_component =  to_vector(spl_coeffs*B);
  
  
  phi0 = 1/phi0primed^2; 
  
  for (i in 1:H){
    a[i] = a0 + a_raw[i]*sigmasq_a;
    k[i] = k0 + k_raw[i]*sigmasq_k;
  }
  
  for (n in 1:N){
   //  mu[n] = a[trustindex[n]] + b0*lag1_ca[n] + c0*lag1ha1[n] +d0*lag1hcw[n] ;
   //this has no mixing problems  mu[n] = a0 + b0*lag1_ca[n] + c0*lag1ha1[n] +d0*lag1hcw[n] ;
   //all transitions are divergent with line below
  // mu[n] = (a[trustindex[n]]*(1 +  spline_component[weekindex[n]]) + b0*lag1_ca_primed[n] + c0*lag1ha2_primed[n] +d0*lag1hcw_primed[n]*exp(v0*hcw_vax_plus3[n]))*exp(sr0*proportion_single_rooms_std[n]+occ0*occupancy_std[n]+agetrust0*percent_pre1965_std[n]+ sizetrust0*trustsize_std[n]+ vol*trustvolperbed_std[n]+ nv0*prop_nv[n]) ;
     //all transitions are divergent with line below
   //mu[n] = (a[trustindex[n]]*(spline_component[weekindex[n]]) + b0*lag1_ca_primed[n] + c0*lag1ha2_primed[n] +d0*lag1hcw_primed[n]*exp(v0*hcw_vax_plus3[n]))*exp(sr0*proportion_single_rooms_std[n]+occ0*occupancy_std[n]+agetrust0*percent_pre1965_std[n]+ sizetrust0*trustsize_std[n]+ vol*trustvolperbed_std[n]+ nv0*prop_nv[n]) ;
   mu[n] = (1 +  spline_component[weekindex[n]])*(a[trustindex[n]] + b0*lag1_ca_primed[n] + c0*lag1ha2_primed[n] +d0*lag1hcw_primed[n]*exp(v0*hcw_vax_plus3[n]))*exp(sr0*proportion_single_rooms_std[n]+occ0*occupancy_std[n]+agetrust0*percent_pre1965_std[n]+ sizetrust0*trustsize_std[n]+ vol*trustvolperbed_std[n]+ nv0*prop_nv[n]) ;

   phi[n] = lag1ha2[n];
  }
}

model { 
 // a_spl_raw ~ normal(0, 0.5);
  a_spl_raw ~ normal(0, 1);
  tau ~ normal(0, 1);

  a0 ~ normal(0, 1);
  b0 ~ normal(0, 1);
  c0 ~ normal(0, 1);
  d0 ~ normal(0, 1);
  k0 ~ normal(0, 1);
  
  sr0 ~ normal(0, 1);
  sizetrust0 ~ normal(0, 1);
  occ0 ~ normal(0, 1);
  agetrust0 ~ normal(0, 1);
  v0 ~ normal(0, 1);
  vol ~ normal(0, 1);

  nv0 ~ normal(0, 1);
  
  
  //a ~ normal(a0, sigmasq_a );
  a_raw ~ normal(0, 1);   //for non-centred parameterisation
  k_raw ~ normal(0, 1);   //for non-centred parameterisation

   sigmasq_a ~ cauchy(0, 1); 
   sigmasq_k ~ cauchy(0, 1); 
  
//  sigmasq_a ~ normal(0, 1); 
//  sigmasq_k ~ normal(0, 1); 
  
//  sigmasq_a ~  student_t(3,0,1); 
//  sigmasq_k ~  student_t(3,0,1); 
  
  
 // sigmasq_a ~ student_t(3,0,1); //slightly heavier tailed prior 
  phi0primed  ~  normal(0, 1);
  for (i in 1:N){
    ha2[i] ~ neg_binomial_2(mu[i], phi0 + k[trustindex[i]]* phi[i]);
  }  
}

generated quantities {
 real ca_coeff;
 real ha_coeff;
 real hcw_coeff;
 
 vector[N] y_rep;
 vector[N] ca_totals; 
 vector[N] ha_totals; 
 vector[N] hcw_totals; 


 vector[N] log_lik;
 
 //posterior predictive distribution
 for (n in 1:N) y_rep[n]= neg_binomial_2_rng(mu[n], phi0 + k[trustindex[n]]* phi[n]);

 // contribution to mean from community acquired cases
  for (n in 1:N) ca_totals[n] = b0*lag1_ca_primed[n]*exp(sr0*proportion_single_rooms_std[n]+occ0*occupancy_std[n]+agetrust0*percent_pre1965_std[n]+ sizetrust0*trustsize_std[n]+ vol*trustvolperbed_std[n]+ nv0*prop_nv[n]); 
  
 // contribution to mean from hospital acquired cases
  for (n in 1:N) ha_totals[n] = c0*lag1ha2_primed[n]*exp(sr0*proportion_single_rooms_std[n]+occ0*occupancy_std[n]+agetrust0*percent_pre1965_std[n]+ sizetrust0*trustsize_std[n]+ vol*trustvolperbed_std[n]+ nv0*prop_nv[n]) ;
  
   // contribution to mean from hcws
  for (n in 1:N) hcw_totals[n] = d0*lag1hcw_primed[n]*exp(v0*hcw_vax_plus3[n])*exp(sr0*proportion_single_rooms_std[n]+occ0*occupancy_std[n]+agetrust0*percent_pre1965_std[n]+ sizetrust0*trustsize_std[n]+ vol*trustvolperbed_std[n]+ nv0*prop_nv[n]) ; 
  

 for (n in 1:N) log_lik[n] = neg_binomial_2_lpmf( ha3[n]| mu[n],  phi0 + k[trustindex[n]]*phi[n]); 
 
 ca_coeff = b0/76.;
 ha_coeff = c0/7.6;
 hcw_coeff = d0/124.3;

}


  