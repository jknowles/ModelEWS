# Packages Investigated

# install.packages(c("BMA","rjags"))

# load("wkce1.rda")
# wkce1<-na.omit(wkce1)
# samp<-sample(unique(wkce1$stuid),10000)
# dat<-subset(wkce1,stuid %in% samp)
# save(dat,file="mydat.rda",compress="xz")

load("mydat.rda")


# Set up STAN
# install.packages(c("inline","Rcpp","RcppEigen"))

# Test
library(inline) 
library(Rcpp)
src <- ' 
  std::vector<std::string> s; 
  s.push_back("hello");
  s.push_back("world");
  return Rcpp::wrap(s);
'
hellofun <- cxxfunction(body = src, includes = '', plugin = 'Rcpp', verbose = FALSE)
cat(hellofun(), '\n') 

remove.packages('rstan') ### if older version of rstan is installed
## add current repository of rstan
options(repos = c(getOption("repos"), rstan = "http://wiki.stan.googlecode.com/git/R"))
install.packages('rstan', type = 'source')

library(rstan) 

schools_code <- '
data {
int<lower=0> J; // number of schools 
real y[J]; // estimated treatment effects
real<lower=0> sigma[J]; // s.e. of effect estimates 
}
parameters {
real mu; 
real<lower=0> tau;
real eta[J];
}
transformed parameters {
real theta[J];
for (j in 1:J)
theta[j] <- mu + tau * eta[j];
}
model {
eta ~ normal(0, 1);
y ~ normal(theta, sigma);
}
'

schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <- stan(model_code = schools_code, data = schools_dat, 
            iter = 1000, chains = 4)

print(fit)
plot(fit)
