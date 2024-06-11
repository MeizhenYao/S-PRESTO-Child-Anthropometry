library(data.table)
library(rjags)
library(reshape2)
library(readr)

set.seed(46377662)

blood_child_who_stan_long<- read_csv("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/input/for paper/anthropometry_paper/blood_child_who_stan_long.csv")[,-1]

Y <- dcast(blood_child_who_stan_long, Subject_ID2 ~ visit, value.var = "zwfl_internal")

X = blood_child_who_stan_long[,c("PFHxS_quar_num", "PFOS_Total_quar_num", "PFOA_Linear_quar_num", 
                                 "PFNA_quar_num", "PFHpA_quar_num", "PFHpS_quar_num", 
                                 "PFDA_quar_num")][seq(1, nrow(blood_child_who_stan_long), 7),]

#### TIMEPOINT

visitWk3 <- data.frame(del_merge=rep(0, nrow(Y)), wk3=rep(1, nrow(Y)), wk6=rep(0, nrow(Y)), 
                       m3=rep(0, nrow(Y)), m6=rep(0, nrow(Y)), m12=rep(0, nrow(Y)), m18=rep(0, nrow(Y)))

visitWk6 <- data.frame(del_merge=rep(0, nrow(Y)), wk3=rep(0, nrow(Y)), wk6=rep(1, nrow(Y)), 
                       m3=rep(0, nrow(Y)), m6=rep(0, nrow(Y)), m12=rep(0, nrow(Y)), m18=rep(0, nrow(Y)))

visitM3 <- data.frame(del_merge=rep(0, nrow(Y)), wk3=rep(0, nrow(Y)), wk6=rep(0, nrow(Y)), 
                      m3=rep(1, nrow(Y)), m6=rep(0, nrow(Y)), m12=rep(0, nrow(Y)), m18=rep(0, nrow(Y)))

visitM6 <- data.frame(del_merge=rep(0, nrow(Y)), wk3=rep(0, nrow(Y)), wk6=rep(0, nrow(Y)), 
                      m3=rep(0, nrow(Y)), m6=rep(1, nrow(Y)), m12=rep(0, nrow(Y)), m18=rep(0, nrow(Y)))

visitM12 <- data.frame(del_merge=rep(0, nrow(Y)), wk3=rep(0, nrow(Y)), wk6=rep(0, nrow(Y)), 
                       m3=rep(0, nrow(Y)), m6=rep(0, nrow(Y)), m12=rep(1, nrow(Y)), m18=rep(0, nrow(Y)))

visitM18 <- data.frame(del_merge=rep(0, nrow(Y)), wk3=rep(0, nrow(Y)), wk6=rep(0, nrow(Y)), 
                       m3=rep(0, nrow(Y)), m6=rep(0, nrow(Y)), m12=rep(0, nrow(Y)), m18=rep(1, nrow(Y)))


visit<- list(visitWk3, visitWk6, visitM3, visitM6, visitM12, visitM18)

blood_child_who_stan_long$pcv1_smoker<- factor(blood_child_who_stan_long$pcv1_smoker,
                                               levels = 0:1)
blood_child_who_stan_long <- DataExplorer::dummify(blood_child_who_stan_long)

C <- blood_child_who_stan_long[,c("Analysis_Batch_recat_B2","Analysis_Batch_recat_B3", "Analysis_Batch_recat_B4","Analysis_Batch_recat_B5", "age_at_recruitment", "pcv1_highest_education_completed_recat_Primary.Secondary.Post_secondary", 
                                  "ethnicity_specified_recat_Chinese", "ethnicity_specified_recat_Malay", "pcv1_parity_recat_...1", "pcv1_smoker_1", 
                                  "pcv1_bmi", "Subject_ID2")][seq(1, nrow(blood_child_who_stan_long), 7),]


N <- nrow(X)

Q <- ncol(C)

P <- ncol(X)

alpha_w <- rep(1, P)

timepoints <- ncol(visitWk3)


n_adapt <- 5000

n_iter1 <- 5000

n_iter2 <- 30000

n_thin <- 30

sw = blood_child_who_stan_long$ipw[seq(1, nrow(blood_child_who_stan_long), 7)]



mix_bwqs_model = 
  "model {
for(i in 1:N) {
  for(t in 1:timepoints) {
    Y[i,t] ~ dnorm(mu[i,t], Y.tau[t])
    mu[i,t] <- a[i] + c[i,1] + c[i,2]*visitWk3[i,t] + c[i,3]*visitWk6[i,t] + c[i,4]*visitM3[i,t] + c[i,5]*visitM6[i,t] + c[i,6]*visitM12[i,t] + c[i,7]*visitM18[i,t]
    
  }
  c[i,1] ~ dnorm(c.mean[i,1], c.tau)
  c[i,2] ~ dnorm(c.mean[i,2], c.tau)
  c[i,3] ~ dnorm(c.mean[i,3], c.tau)
  c[i,4] ~ dnorm(c.mean[i,4], c.tau)
  c[i,5] ~ dnorm(c.mean[i,5], c.tau)
  c[i,6] ~ dnorm(c.mean[i,6], c.tau)
  c[i,7] ~ dnorm(c.mean[i,7], c.tau)
  a[i] ~ dnorm(a.mean[i], a.tau)
  
  c.mean[i,1] <- gamma1*sw[i] 
  c.mean[i,2] <- (gamma1 + gamma3*(X[i,1:P] %*% What[1:P]))*sw[i] 
  c.mean[i,3] <- (gamma1 + gamma4*(X[i,1:P] %*% What[1:P]))*sw[i] 
  c.mean[i,4] <- (gamma1 + gamma5*(X[i,1:P] %*% What[1:P]))*sw[i] 
  c.mean[i,5] <- (gamma1 + gamma6*(X[i,1:P] %*% What[1:P]))*sw[i] 
  c.mean[i,6] <- (gamma1 + gamma7*(X[i,1:P] %*% What[1:P]))*sw[i] 
  c.mean[i,7] <- (gamma1 + gamma8*(X[i,1:P] %*% What[1:P]))*sw[i] 
  a.mean[i] <- (gamma0 + gamma2*(X[i,1:P] %*% Wtilde[1:P]) + (C[i,1:Q] %*% delta[1:Q]))*sw[i]
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
gamma4 ~ dnorm(0, 1.0E-04)
gamma5 ~ dnorm(0, 1.0E-04)
gamma6 ~ dnorm(0, 1.0E-04)
gamma7 ~ dnorm(0, 1.0E-04)
gamma8 ~ dnorm(0, 1.0E-04)
Wtilde ~  ddirich(alpha_w)
What ~  ddirich(alpha_w)
for(q in 1:Q) { delta[q] ~ dnorm(0, 1.0E-06) }
}"

jdata <- list(Y=Y, X=X, N=N, alpha_w = alpha_w, sw=sw, visitWk3=visitWk3, visitWk6=visitWk6, visitM3=visitM3, 
              visitM6=visitM6, visitM12=visitM12, visitM18=visitM18, C=C, P=P, Q=Q, timepoints=timepoints)
var.s <- c("Wtilde","What","gamma0","gamma1","gamma2","gamma3","gamma4","gamma5","gamma6","gamma7","gamma8","a.tau","c.tau")

model.fit <- jags.model(file=textConnection(mix_bwqs_model), data=jdata, n.chains=2, 
                        n.adapt=n_adapt, quiet=T)

update(model.fit, n.iter = n_iter1, progress.bar="none")
model.fit <- coda.samples(model=model.fit, variable.names=var.s, n.iter=n_iter2, thin=n_thin, progress.bar="none")
sm = summary(model.fit)

p_table = data.table(Parameters  = c(paste0("Wh_",c("PFHxS_quar_num", "PFOS_Total_quar_num", "PFOA_Linear_quar_num", 
                                                    "PFNA_quar_num", "PFHpA_quar_num", "PFHpS_quar_num", 
                                                    "PFDA_quar_num")),
                                     paste0("Wt_",c("PFHxS_quar_num", "PFOS_Total_quar_num", "PFOA_Linear_quar_num", 
                                                    "PFNA_quar_num", "PFHpA_quar_num", "PFHpS_quar_num", 
                                                    "PFDA_quar_num")),
                                     "sigma_a","sigma_c",
                                     "gamma0","gamma1","gamma2","gamma3","gamma4","gamma5","gamma6","gamma7","gamma8"),
                     Mean        = format(round(sm$statistics[,1],3), digits=3, nsmall=3), 
                     `2.5% CrI`  = format(round(sm$quantiles[,1],3), digits=3, nsmall=3),
                     `97.5% CrI` = format(round(sm$quantiles[,5],3), digits=3, nsmall=3))

p_table

