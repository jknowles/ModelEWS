################################################################################
# Bayesian Prediction Module
# DEVELOPMENT
#
################################################################################

# Cut the data down for now
dat <- subset(studata, transfer_out < 1)

dat <- subset(dat, select = c("lds_student_key", "math7", "read7", 
                              "ontime_grad", "gender", "disab_code", 
                              "race", "frpl2", "ell_comp_med2", "schg7",
                              "attrate_yr1", "mobility_dist_yr1"))


dat <- na.omit(dat)
rm(studata)

################################################################################
# BMA
################################################################################
library(BMA)

mymod<-model.matrix(ontime_grad ~ math7 + read7 + I(math7 ^ 2) + 
                      I(read7 ^ 2) + I(math7 ^ 3) + I(read7 ^ 3) +
                      attrate_yr1 + mobility_dist_yr1 + race * frpl2 + 
                      gender + disab_code + ell_comp_med2,
                    data=dat)



system.time(
  small <- bic.glm(dat[ , c(2:3, 5:9, 11:12)], dat$ontime_grad, glm.family="binomial",
                   factor.type = TRUE)
)

names(mymod[1,]) <- c("Intercept", small$namesx)

dat$yhat <- predict(small, newdata = dat)


# test predictions


system.time(
  big <- bic.glm(dat[ , c(2:3, 5:12)], dat$ontime_grad, glm.family="binomial",
                   factor.type = TRUE)
)

summary(big)


fullmodel <- formula(ontime_grad ~ math7 + read7 +
                       attrate_yr1 + mobility_dist_yr1 + race * frpl2 + 
                       gender + disab_code + ell_comp_med2 + schg7)

system.time(
big_model <- bic.glm(f = fullmodel, 
                   data = dat, factor.type = TRUE,
                     glm.family="binomial")
)

################################################################################
# STAN
################################################################################
library(rstan)



ML<-'
data{

int<lower=0> N; //obs
real y[N]; // dv
real x[N]; // iv

}

parameters {

real alpha;
real beta;
real<lower=0> sigma; 

}

model {

alpha ~ normal(50,100);
beta ~ normal(0.9,2);
sigma ~ uniform(0, 500) T[0,];
for(n in 1:N)
y[n] ~ normal(alpha+beta*x[n],sigma);

}
'

moddat<-list(x=dat$readSS,y=dat$mathSS,N=nrow(dat))
# set_cppo("fast") 
# get_cppo() 
fit1<-stan(model_code=ML,data=moddat,iter=500,chains=4)
fit2<-stan(fit=fit,data=moddat,iter=5000,chains=2)
print(fit2)
plot(fit2)
traceplot(fit2)
summary(lm(mathSS~readSS,data=dat))

example(sflist2stanfit)


