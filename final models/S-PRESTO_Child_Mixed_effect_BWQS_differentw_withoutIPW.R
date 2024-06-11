
library(data.table)
library(rjags)
library(reshape2)
library(readr)
library(tidyverse)
library(ggalluvial)

#blood_child_who_stan_long <- blood_child_who_stan_long[blood_child_who_stan_long$bw_sex == 2,]

Y <- dcast(blood_child_who_stan_long, Subject_ID2 ~ visit, value.var = "zbmi") %>% 
  select(del, wk3, wk6, m3, m6, m12, m18, m24, m36, y4)

X = blood_child_who_stan_long[,c("PFHxS_quar_num", "PFOS_Total_quar_num", "PFOA_Linear_quar_num", 
                                 "PFNA_quar_num", "PFHpA_quar_num", "PFHpS_quar_num", 
                                 "PFDA_quar_num","Subject_ID2")][seq(1, nrow(blood_child_who_stan_long), 10),]

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

timepoints <- 10

n_adapt <- 5000

n_iter1 <- 5000

n_iter2 <- 30000

n_thin <- 30

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
   Y[i,8] ~ dnorm(mu[i,8], Y.tau[8])
   Y[i,9] ~ dnorm(mu[i,9], Y.tau[9])
   Y[i,10] ~ dnorm(mu[i,10], Y.tau[10])
   
   
   mu[i,1] <- a[i] + c[i,1]
   mu[i,2] <- a[i] + c[i,2]
   mu[i,3] <- a[i] + c[i,3]
   mu[i,4] <- a[i] + c[i,4]
   mu[i,5] <- a[i] + c[i,5]
   mu[i,6] <- a[i] + c[i,6]
   mu[i,7] <- a[i] + c[i,7]
   mu[i,8] <- a[i] + c[i,8]
   mu[i,9] <- a[i] + c[i,9]
   mu[i,10] <- a[i] + c[i,10]
   
  c[i,1] ~ dnorm(c.mean[i,1], c.tau)
  c[i,2] ~ dnorm(c.mean[i,2], c.tau)
  c[i,3] ~ dnorm(c.mean[i,3], c.tau)
  c[i,4] ~ dnorm(c.mean[i,4], c.tau)
  c[i,5] ~ dnorm(c.mean[i,5], c.tau)
  c[i,6] ~ dnorm(c.mean[i,6], c.tau)
  c[i,7] ~ dnorm(c.mean[i,7], c.tau)
  c[i,8] ~ dnorm(c.mean[i,8], c.tau)
  c[i,9] ~ dnorm(c.mean[i,9], c.tau)
  c[i,10] ~ dnorm(c.mean[i,10], c.tau)
  a[i] ~ dnorm(a.mean[i], a.tau)
  
  c.mean[i,1] <- inter1 + (gamma1*(X[i,1:P] %*% What1[1:P]))
  c.mean[i,2] <- inter2 + (gamma2*(X[i,1:P] %*% What2[1:P]))
  c.mean[i,3] <- inter3 + (gamma3*(X[i,1:P] %*% What3[1:P]))
  c.mean[i,4] <- inter4 + (gamma4*(X[i,1:P] %*% What4[1:P]))
  c.mean[i,5] <- inter5 + (gamma5*(X[i,1:P] %*% What5[1:P]))
  c.mean[i,6] <- inter6 + (gamma6*(X[i,1:P] %*% What6[1:P]))
  c.mean[i,7] <- inter7 + (gamma7*(X[i,1:P] %*% What7[1:P]))
  c.mean[i,8] <- inter8 + (gamma8*(X[i,1:P] %*% What8[1:P]))
  c.mean[i,9] <- inter9 + (gamma9*(X[i,1:P] %*% What9[1:P]))
  c.mean[i,10] <- inter99910 + (gamma99910*(X[i,1:P] %*% What99910[1:P]))

  
  a.mean[i] <- inter  + (C[i,1:Q] %*% delta[1:Q]) 
}

for(t in 1:10){
  Y.tau.s[t] ~ dunif(0,3)
  Y.tau[t] <- 1/(Y.tau.s[t]*Y.tau.s[t])
}

a.tau <- 1/(a.sigma*a.sigma)
a.sigma ~ dunif(0,5)
c.tau <- 1/(c.sigma*c.sigma)
c.sigma ~ dunif(0,5)

inter ~ dnorm(0, 1.0E-04)
inter1 ~ dnorm(0, 1.0E-04)
inter2 ~ dnorm(0, 1.0E-04)
inter3 ~ dnorm(0, 1.0E-04)
inter4 ~ dnorm(0, 1.0E-04)
inter5 ~ dnorm(0, 1.0E-04)
inter6 ~ dnorm(0, 1.0E-04)
inter7 ~ dnorm(0, 1.0E-04)
inter8 ~ dnorm(0, 1.0E-04)
inter9 ~ dnorm(0, 1.0E-04)
inter99910 ~ dnorm(0, 1.0E-04)

gamma1 ~ dnorm(0, 1.0E-04)
gamma2 ~ dnorm(0, 1.0E-04)
gamma3 ~ dnorm(0, 1.0E-04)
gamma4 ~ dnorm(0, 1.0E-04)
gamma5 ~ dnorm(0, 1.0E-04)
gamma6 ~ dnorm(0, 1.0E-04)
gamma7 ~ dnorm(0, 1.0E-04)
gamma8 ~ dnorm(0, 1.0E-04)
gamma9 ~ dnorm(0, 1.0E-04)
gamma99910 ~ dnorm(0, 1.0E-04)


What1 ~  ddirich(alpha_w)
What2 ~  ddirich(alpha_w)
What3 ~  ddirich(alpha_w)
What4 ~  ddirich(alpha_w)
What5 ~  ddirich(alpha_w)
What6 ~  ddirich(alpha_w)
What7 ~  ddirich(alpha_w)
What8 ~  ddirich(alpha_w)
What9 ~  ddirich(alpha_w)
What99910 ~  ddirich(alpha_w)

for(q in 1:Q) { delta[q] ~ dnorm(0, 1.0E-06) }
}"

jdata <- list(Y=Y, X=X, N=N, alpha_w = alpha_w, C=C, P=P, Q=Q)
var.s <- c("What1","What2","What3","What4","What5","What6","What7","What8","What9","What99910",
           "gamma1","gamma2","gamma3","gamma4","gamma5","gamma6","gamma7","gamma8","gamma9","gamma99910",
           "inter","inter1","inter2","inter3","inter4","inter5","inter6","inter7","inter8","inter9","inter99910",
           "a.tau","c.tau")

model.fit <- jags.model(file=textConnection(mix_bwqs_model), data=jdata, n.chains=4, inits = list(.RNG.name = "base::Wichmann-Hill",
                                                                                                  .RNG.seed = 46377662),
                        n.adapt=n_adapt, quiet=T)

update(model.fit, n.iter = n_iter1, progress.bar="none")
model.fit <- coda.samples(model=model.fit, variable.names=var.s, n.iter=n_iter2, thin=n_thin, progress.bar="none")
sm1 = summary(model.fit)

weight_means <- as.data.frame(sm1$statistics)

weight_means$timepoint <- c(rep(c("Delivery"), 7), rep(c("Week 3"), 7), rep(c("Week 6"), 7), rep(c("Month 3"), 7), rep(c("Month 6"), 7),
                            rep(c("Month 12"), 7), rep(c("Month 18"), 7), rep(c("Month 24"), 7), rep(c("Month 36"), 7), 
                            rep(c("Month 48"), 7), c(NA, NA), 
                            rep(c("Delivery", "Week 3", "Week 6", "Month 3", "Month 6", "Month 12", "Month 18", "Month 24", 
                                  "Month 36", "Month 48"), 1), c(NA), 
                            rep(c("Delivery", "Week 3", "Week 6", "Month 3", "Month 6", "Month 12", "Month 18", "Month 24", 
                                  "Month 36", "Month 48"), 1))

weight_means$timepoint <- factor(weight_means$timepoint, 
                                 levels=c("Delivery", "Week 3", "Week 6", "Month 3", "Month 6", "Month 12", "Month 18", "Month 24", 
                                          "Month 36", "Month 48"))

weight_means$PFAS <- c(rep(c("PFHxS", "PFOS", "PFOA", "PFNA", "PFHpA", "PFHpS", "PFDA"), 10), c(NA, NA), rep(c("Mixture"), 10), c(NA), 
                       rep(c("Mixture"), 10))

weight_means$PFAS <- factor(weight_means$PFAS, 
                            levels=c("PFHxS", "PFOS", "PFOA", "PFNA", "PFHpA", "PFHpS", "PFDA", "Mixture"))

ggplot(weight_means[1:70,],
       aes(y = 10000*weight_means$Mean[1:70], axis1 = timepoint, axis2 = PFAS)) +
  geom_alluvium(aes(fill = PFAS), width = 1/12) +
  ylab("") +
  theme(legend.position = "none") + 
  scale_y_continuous(breaks = NULL, minor_breaks = NULL) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Time Point", "PFAS"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1")




