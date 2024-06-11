

library(data.table)
library(rjags)
library(reshape2)
library(readr)
library(tidyverse)

#blood_child_who_stan_long <- blood_child_who_stan_long[blood_child_who_stan_long$bw_sex == 2,]

Y <- dcast(blood_child_who_stan_long, Subject_ID2 ~ visit, value.var = "zbmi") %>% 
  select(del, wk3, wk6, m3, m6, m12, m18, m24, m36, y4)                                            # !!!


### be careful!!!! sequence
X = blood_child_who_stan_long[,c("PFHxS_quar_num", "PFOS_Total_quar_num", "PFOA_Linear_quar_num", 
                                 "PFNA_quar_num", "PFHpA_quar_num", "PFHpS_quar_num", 
                                 "PFDA_quar_num")][seq(1, nrow(blood_child_who_stan_long), 10),]

Years_since_birth <- dcast(blood_child_who_stan_long, Subject_ID2 ~ visit, value.var = "Years_since_birth")%>% 
  select(del, wk3, wk6, m3, m6, m12, m18, m24, m36, y4)                           # !!!





blood_child_who_stan_long$pcv1_smoker<- factor(blood_child_who_stan_long$pcv1_smoker,
                                               levels = 0:1)
blood_child_who_stan_long <- DataExplorer::dummify(blood_child_who_stan_long)

C <- blood_child_who_stan_long[,c("Analysis_Batch_recat_B2","Analysis_Batch_recat_B3", "Analysis_Batch_recat_B4","Analysis_Batch_recat_B5", "age_at_recruitment", "pcv1_highest_education_completed_recat_Primary.Secondary.Post_secondary", 
                                  "ethnicity_specified_recat_Chinese", "ethnicity_specified_recat_Malay", "pcv1_parity_recat_...1", "pcv1_smoker_1", 
                                  "pcv1_bmi", "Subject_ID2")][seq(1, nrow(blood_child_who_stan_long), 10),]


N <- nrow(X)

Q <- ncol(C)

P <- ncol(X)

alpha_w <- rep(1, P)

timepoints <- ncol(Years_since_birth)

n_adapt <- 5000

n_iter1 <- 5000

n_iter2 <- 30000

n_thin <- 30

mix_bwqs_model = 
  "model {
for(i in 1:N) {
  for(t in 1:timepoints) {
    Y[i,t] ~ dnorm(mu[i,t], Y.tau[t])
    mu[i,t] <- a[i] + c[i]*Years_since_birth[i,t] 
  }
  c[i] ~ dnorm(c.mean[i], c.tau)
  a[i] ~ dnorm(a.mean[i], a.tau)
  
  c.mean[i] <- gamma1 + gamma3*(X[i,1:P] %*% What[1:P])
  a.mean[i] <- gamma0 + gamma2*(X[i,1:P] %*% Wtilde[1:P]) + (C[i,1:Q] %*% delta[1:Q])     
}

for(t in 1:timepoints){
Y.tau.s[t] ~ dunif(0,3)
Y.tau[t] <- 1/(Y.tau.s[t]*Y.tau.s[t])
}

a.tau <- 1/(a.sigma*a.sigma)
a.sigma ~ dunif(0,5)
c.tau <- 1/(c.sigma*c.sigma)
c.sigma ~ dunif(0,5)
gamma0 ~ dnorm(0, 1.0E-04)
gamma1 ~ dnorm(0, 1.0E-04)
gamma2 ~ dnorm(0, 1.0E-04)
gamma3 ~ dnorm(0, 1.0E-04)
Wtilde ~  ddirich(alpha_w)
What ~  ddirich(alpha_w)
for(q in 1:Q) { delta[q] ~ dnorm(0, 1.0E-06) }
}"

jdata <- list(Y=Y, X=X, N=N, alpha_w = alpha_w, Years_since_birth=Years_since_birth, C=C, P=P, Q=Q, timepoints=timepoints)
var.s <- c("Wtilde","What","gamma0","gamma1","gamma2","gamma3","a.tau","c.tau")


model.fit <- jags.model(file=textConnection(mix_bwqs_model), data=jdata, n.chains=4, inits = list(.RNG.name = "base::Wichmann-Hill",
                                                                                                  .RNG.seed = 46377662),
                        n.adapt=n_adapt, quiet=T)

update(model.fit, n.iter = n_iter1, progress.bar="none")

model.fit <- coda.samples(model=model.fit, variable.names=var.s, n.iter=n_iter2, thin=n_thin, progress.bar="none")
sm = summary(model.fit)



