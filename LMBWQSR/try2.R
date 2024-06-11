library(data.table)
library(rjags)
library(reshape2)
library(readr)
library(tidyverse)
blood_child_who_stan_long<- read_csv("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/input/for paper/anthropometry_paper/blood_child_who_stan_long.csv")[,-1]

Y <- dcast(blood_child_who_stan_long, Subject_ID2 ~ visit, value.var = "zwfl_internal") %>% 
  select(del_merge, wk3, wk6, m3, m6, m12, m18)

X = blood_child_who_stan_long[,c("PFHxS_quar_num", "PFOS_Total_quar_num", "PFOA_Linear_quar_num", 
                                 "PFNA_quar_num", "PFHpA_quar_num", "PFHpS_quar_num", 
                                 "PFDA_quar_num")][seq(1, nrow(blood_child_who_stan_long), 7),]

# Months_since_birth <- dcast(blood_child_who_stan_long, Subject_ID2 ~ visit, value.var = "Months_since_birth")


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

timepoints <- 7

n_adapt <- 500

n_iter1 <- 500

n_iter2 <- 3000

n_thin <- 30

seed = 51346

sw = blood_child_who_stan_long$ipw[seq(1, nrow(blood_child_who_stan_long), 7)]

mix_bwqs_model = 
  "model {
for(i in 1:N) {
   Y[i,1] ~ dnorm(mu[i,1], Y.tau[1])
   Y[i,2] ~ dnorm(mu[i,2], Y.tau[2])
   Y[i,3] ~ dnorm(mu[i,3], Y.tau[3])
   Y[i,4] ~ dnorm(mu[i,4], Y.tau[4])
   Y[i,5] ~ dnorm(mu[i,5], Y.tau[5])
   Y[i,6] ~ dnorm(mu[i,6], Y.tau[6])
   Y[i,7] ~ dnorm(mu[i,7], Y.tau[7])
   
   
   mu[i,1] <- a[i] 
   mu[i,2] <- a[i] + c[i,2]
   mu[i,3] <- a[i] + c[i,3]
   mu[i,4] <- a[i] + c[i,4]
   mu[i,5] <- a[i] + c[i,5]
   mu[i,6] <- a[i] + c[i,6]
   mu[i,7] <- a[i] + c[i,7]
   

  c[i,2] ~ dnorm(c.mean[i,2], c.tau)
  c[i,3] ~ dnorm(c.mean[i,3], c.tau)
  c[i,4] ~ dnorm(c.mean[i,4], c.tau)
  c[i,5] ~ dnorm(c.mean[i,5], c.tau)
  c[i,6] ~ dnorm(c.mean[i,6], c.tau)
  c[i,7] ~ dnorm(c.mean[i,7], c.tau)
  a[i] ~ dnorm(a.mean[i], a.tau)
  
  c.mean[i,2] <- inter2 + (gamma2*(X[i,1:P] %*% What[1:P]))*sw[i] 
  c.mean[i,3] <- inter3 + (gamma3*(X[i,1:P] %*% What[1:P]))*sw[i] 
  c.mean[i,4] <- inter4 + (gamma4*(X[i,1:P] %*% What[1:P]))*sw[i] 
  c.mean[i,5] <- inter5 + (gamma5*(X[i,1:P] %*% What[1:P]))*sw[i] 
  c.mean[i,6] <- inter6 + (gamma6*(X[i,1:P] %*% What[1:P]))*sw[i] 
  c.mean[i,7] <- inter7 + (gamma7*(X[i,1:P] %*% What[1:P]))*sw[i] 
  
  a.mean[i] <- inter1  + (gamma1*(X[i,1:P] %*% Wtilde[1:P]))*sw[i]  + (C[i,1:Q] %*% delta[1:Q]) 
}

for(t in 1:7){
  Y.tau.s[t] ~ dunif(0,3)
  Y.tau[t] <- 1/(Y.tau.s[t]*Y.tau.s[t])
}

a.tau <- 1/(a.sigma*a.sigma)
a.sigma ~ dunif(0,5)
c.tau <- 1/(c.sigma*c.sigma)
c.sigma ~ dunif(0,5)

inter1 ~ dnorm(0, 1.0E-04)
inter2 ~ dnorm(0, 1.0E-04)
inter3 ~ dnorm(0, 1.0E-04)
inter4 ~ dnorm(0, 1.0E-04)
inter5 ~ dnorm(0, 1.0E-04)
inter6 ~ dnorm(0, 1.0E-04)
inter7 ~ dnorm(0, 1.0E-04)

gamma1 ~ dnorm(0, 1.0E-04)
gamma2 ~ dnorm(0, 1.0E-04)
gamma3 ~ dnorm(0, 1.0E-04)
gamma4 ~ dnorm(0, 1.0E-04)
gamma5 ~ dnorm(0, 1.0E-04)
gamma6 ~ dnorm(0, 1.0E-04)
gamma7 ~ dnorm(0, 1.0E-04)

Wtilde ~  ddirich(alpha_w)
What ~  ddirich(alpha_w)

for(q in 1:Q) { delta[q] ~ dnorm(0, 1.0E-06) }
}"

jdata <- list(Y=Y, X=X, N=N, alpha_w = alpha_w, sw=sw,C=C, P=P, Q=Q)
var.s <- c("Wtilde","What",
           "gamma1","gamma2","gamma3","gamma4","gamma5","gamma6","gamma7",
           "inter","inter1","inter2","inter3","inter4","inter5","inter6","inter7",
           "a.tau","c.tau")

model.fit <- jags.model(file=textConnection(mix_bwqs_model), data=jdata, n.chains=2, 
                        n.adapt=n_adapt, quiet=T)

update(model.fit, n.iter = n_iter1, progress.bar="none")
model.fit <- coda.samples(model=model.fit, variable.names=var.s, n.iter=n_iter2, thin=n_thin, progress.bar="none")
sm2 = summary(model.fit)












p_table = data.table(Parameters  = c(paste0("weight_visit1_",c("PFHxS_quar_num", "PFOS_Total_quar_num", "PFOA_Linear_quar_num", 
                                                               "PFNA_quar_num", "PFHpA_quar_num", "PFHpS_quar_num", 
                                                               "PFDA_quar_num")),
                                     paste0("Wt_",c("PFHxS_quar_num", "PFOS_Total_quar_num", "PFOA_Linear_quar_num", 
                                                    "PFNA_quar_num", "PFHpA_quar_num", "PFHpS_quar_num", 
                                                    "PFDA_quar_num")),
                                     "sigma_a","sigma_c",
                                     "gamma0","gamma1","gamma2","gamma3"),
                     Mean        = format(round(sm$statistics[,1],3), digits=3, nsmall=3), 
                     `2.5% CrI`  = format(round(sm$quantiles[,1],3), digits=3, nsmall=3),
                     `97.5% CrI` = format(round(sm$quantiles[,5],3), digits=3, nsmall=3))




PFAS = c("PFHxS", "PFOS_Total", "PFOA_Linear", 
         "PFNA", "PFHpA", "PFHpS", 
         "PFDA")



par(mar=c(5, 4, 4, 5))
ylim 		= c(-0.2, 0.9)
plot(1:8, p_table[c(19,1:7),Mean], xaxt="n",
     ylim=ylim, cex.axis=1, col = c("black",rep("grey51",7)),
     pch=15, xlab="PFAS", ylab="Main effect",
     main="Mixture baseline"
)

arrows(1:8, as.numeric(p_table[c(19,1:7),`2.5% CrI`]), 
       1:8, as.numeric(p_table[c(19,1:7),`97.5% CrI`]), 
       col = c("black",rep("grey51",7)),
       length=0.05, angle=90, code=3)
axis(1, at=1, labels=c("Overall\nmixture"), cex.axis = 0.75, 
     col.axis = c("black"))
axis(1, at=2:8, labels=c(PFAS), cex.axis = 0.75, 
     col.axis = c("grey51"))
axis(4, ylim=ylim, col="black",col.axis="grey51")
mtext("PFAS Weights",side=4,col="grey51",line = 3)
abline(h=0)
abline(v=1.5,col="red",lty=2)

par(mar=c(5, 4, 4, 5))
ylim 		= c(0, 0.55)
plot(1:8, p_table[c(20,8:14),Mean], xaxt="n",
     ylim=ylim, cex.axis=1, col = c("black",rep("grey51",7)),
     pch=15, xlab="PFAS", ylab="Main effect",
     main="Mixture interaction with age")
arrows(1:8, as.numeric(p_table[c(20,8:14),`2.5% CrI`]), 
       1:8, as.numeric(p_table[c(20,8:14),`97.5% CrI`]), 
       col = c("black",rep("grey51",7)),
       length=0.05, angle=90, code=3)
axis(1, at=1, labels=c("Overall\nmixture"), cex.axis = 0.75, 
     col.axis = c("black"))
axis(1, at=2:8, labels=c(PFAS), cex.axis = 0.75, 
     col.axis = c("grey51"))
axis(4, ylim=ylim, col="black",col.axis="grey51")
mtext("PFAS Weights",side=4,col="grey51",line = 3)
abline(h=0)
abline(v=1.5,col="red",lty=2)








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

