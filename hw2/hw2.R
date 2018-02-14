# Work Directory ----------------------------------------------------------

setwd("/Users/Lyf/OneDrive/study/WISC/2017_spring/Stat_679/hw2")


# Load Package ------------------------------------------------------------

library(tidyverse)
library(lme4)
library(gridExtra)
library(grid)
library(rstanarm)
library(rstan)
rstan_options(auto_write = TRUE)

# Load Data ---------------------------------------------------------------

da = read_csv("hw02.csv")

# Q1 ----------------------------------------------------------------------

answer1 = da %>%
  summarise(
    mean = mean(Score),
    median = median(Score),
    sd = sd(Score),
    min = min(Score),
    max = max(Score)
  )

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 
ggplot(da, aes(Score)) +
  geom_histogram(aes(Score, ..density..), bins = 20, color = "white") +
  geom_density(color = "#56B4E9") 
  # scale_colour_manual(values = cbPalette) +
  # scale_fill_manual(values = cbPalette) 



# Q2 ----------------------------------------------------------------------

answer2 = da %>%
  summarise(
    mean = mean(SES),
    median = median(SES),
    sd = sd(SES),
    min = min(SES),
    max = max(SES)
  )

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 
ggplot(da, aes(SES)) +
  geom_histogram(aes(SES, ..density..), bins = 20, color = "white") +
  geom_density(color = "#56B4E9") 
# scale_colour_manual(values = cbPalette) +
# scale_fill_manual(values = cbPalette) 


# Q3 ----------------------------------------------------------------------

ggplot(da, aes(SES, Score)) +
  geom_hex() +
  geom_smooth() +
  scale_fill_gradientn(colours=c("yellow","blue"))


# Q4 ----------------------------------------------------------------------

answer4 = da %>%
  group_by(School) %>%
  summarise(
    SES = mean(SES),
    Score = mean(Score),
    Number = n()
  )
J = nrow(answer4)
N = nrow(da)

# Q5 ----------------------------------------------------------------------

ggplot(answer4, aes(SES, Score)) +
  geom_point() +
  geom_smooth()

# Q6 ----------------------------------------------------------------------

plot1 = ggplot(answer4) +
  geom_point(aes(School, Number))
plot2 = ggplot(answer4) +
  geom_histogram(aes(Number), bins = 13, color = "white")
grid.arrange(plot1, plot2, ncol=2)

# Q7 ----------------------------------------------------------------------

schoolA = answer4 %>%
  filter(.$Number == min(.$Number)) %>%
  filter(.$SES == min(.$SES))
schoolB = answer4 %>%
  filter(.$Number == min(.$Number)) %>%
  filter(.$SES == max(.$SES))
schoolC = answer4 %>%
  filter(.$Number == max(.$Number)) %>%
  filter(.$SES == min(.$SES))
schoolD = answer4 %>%
  filter(.$Number == max(.$Number)) %>%
  filter(.$SES == max(.$SES))
schoolE = answer4 %>%
  filter(.$Number == median(.$Number)) %>%
  filter(.$SES == median(.$SES))
school_label = tibble(
  index = c("A", "B", "C", "D", "E"),
  label = c(schoolA$School, 
            schoolB$School, 
            schoolC$School, 
            schoolD$School, 
            schoolE$School)
)

# Q8 ----------------------------------------------------------------------

model7 = lmer(Score ~ SES + (1|School), da)
mysum = summary(model7)


answer8 = tibble(
  parameter = c("sigmaA", "sigma", "beta0", "beta1"),
  Estimate = c(attr(mysum$varcor$School,"stddev"), 
               attr(mysum$varcor,"sc"),
               mysum$coefficients[,1]),
  sd = c(NA, NA, mysum$coefficients[,2])
)


# Q9 ----------------------------------------------------------------------

set.seed(12345)
sigmaA = 2
alpha = rnorm(J,0,sigmaA)
x = da$SES
group_ori = sort(unique(da$School), decreasing = F)
group1 = as.factor(da$School)
group_order = as.numeric(levels(group1))
group = as.numeric(group1)
y = da$Score
hw02_data = list(N, J, y, x, group)
require(rstan)
fit = stan(file="hw02.stan",seed=23456)
fit.extract = rstan::extract(fit)
beta = summary(fit, pars="beta")$summary
alpha = summary(fit, pars="alpha")$summary
sigma = summary(fit, pars="sigma")$summary
sigmaA = summary(fit, pars="sigmaA")$summary
answer9 = tibble(
  parameter = c("sigmaA", "sigma", "beta0", "beta1"),
  Estimate = c(sigmaA[1], sigma[1], beta[1,1], beta[2,1]),
  sd = c(sigmaA[3], sigma[3], beta[1,3], beta[2,3])
)


# Q10 ---------------------------------------------------------------------

aa = confint(model7)
quantile(beta[1,],probs=c(0.025, 0.975)) 
answer10 = tibble(
  parameter = c("lmer_beta0", "lmer_beta1", "stan_beta0", "stan_beta1"),
  "2.5%" = c(aa[3:4,1], 
             quantile(beta[,1],probs=c(0.025)), 
             quantile(beta[,2],probs=c(0.025))),
  "97.5%" = c(aa[3:4,2], 
              quantile(beta[,1],probs=c(0.975)), 
              quantile(beta[,2],probs=c(0.975)))
)


# Q11 ---------------------------------------------------------------------

set.seed(12345)
sigmaA = 2
alpha = rnorm(J,0,sigmaA)
x = da$SES
group_ori = sort(unique(da$School), decreasing = F)
group1 = as.factor(da$School)
group_order = as.numeric(levels(group1))
group = as.numeric(group1)
y = da$Score
hw02_data2 = list(
  N,
  J,
  y,
  x,
  group
)

require(rstan)
fit2 = stan(file="hw02.stan",seed=615, data = hw02_data2)
# extract posterior sample from fitted stan object
fit2.extract = rstan::extract(fit2)
# calculate quantiles of the population slope 
## true value was 2
beta2 = fit2.extract$beta 
sigma2 = fit2.extract$sigma
sigmaA2 = fit2.extract$sigmaA


answer11 = tibble(
  parameter = c("stan1_beta0", "stan1_beta1", "stan2_beta0", "stan2_beta1"),
  "2.5%" = c(quantile(beta[,1],probs=c(0.025)), 
             quantile(beta[,2],probs=c(0.025)),
             quantile(beta2[,1],probs=c(0.025)), 
             quantile(beta2[,2],probs=c(0.025))),
  "97.5%" = c(quantile(beta[,1],probs=c(0.975)), 
              quantile(beta[,2],probs=c(0.975)), 
              quantile(beta2[,1],probs=c(0.975)), 
              quantile(beta2[,2],probs=c(0.975)))
)

# Q12 ---------------------------------------------------------------------

set.seed(12345)
sigmaA = 2
alpha = rnorm(J,0,sigmaA)
x = da$SES
group_ori = sort(unique(da$School), decreasing = F)
group1 = as.factor(da$School)
group_order = as.numeric(levels(group1))
group = as.numeric(group1)
y = da$Score
hw02_data3 = list(
  N,
  J,
  y,
  x,
  group
)

require(rstan)
fit3 = stan(file="hw02_2.stan",seed=23456, data = hw02_data3)
# extract posterior sample from fitted stan object
fit3.extract = rstan::extract(fit3)
# calculate quantiles of the population slope 
## true value was 2
beta3 = fit3.extract$beta 
sigma3 = fit3.extract$sigma
sigmaA3 = fit3.extract$sigmaA


answer12 = tibble(
  parameter = c("stan1_beta0", "stan1_beta1", "stan3_beta0", "stan3_beta1"),
  "2.5%" = c(quantile(beta[,1],probs=c(0.025)), 
             quantile(beta[,2],probs=c(0.025)),
             quantile(beta3[,1],probs=c(0.025)), 
             quantile(beta3[,2],probs=c(0.025))),
  "97.5%" = c(quantile(beta[,1],probs=c(0.975)), 
              quantile(beta[,2],probs=c(0.975)), 
              quantile(beta3[,1],probs=c(0.975)), 
              quantile(beta3[,2],probs=c(0.975)))
)

# Q13 ---------------------------------------------------------------------

alpha = fit.extract$alpha
stan = apply(alpha, 2, mean)

lmer = coef(model7)$School[,1] - mysum$coefficients[1][1]
y13 =  tibble(
  n = c(1:length(stan), 1:length(stan)),
  index = c(rep("sten",length(stan)), rep("lmer",length(stan))),
  y = c(stan, lmer)
)
ggplot(y13) +
  geom_point(aes(n, y, color = index))


# Q14 ---------------------------------------------------------------------

da_sub = da[which(da$School %in% school_label$label),] %>%
  arrange(School)
da_sub2 = da_sub 
da_sub2$SES = 7

fun_lmer1 = function(x, data = da_sub, conf = TRUE){
  sigma = stats::sigma(x)
  n = nrow(data)
  fit = predict(x, newdata=data)
  if( ! conf ){
    fit = fit + rnorm(n, 0, sigma)
  }
  return(fit)
}

fun.conf_lmer1 = function(x){
  return( fun_lmer1(x) )
}
fun.pred_lmer1 = function(x){
  return( fun_lmer1(x, conf = FALSE, data = da_sub2) )
}
conf.lmer = bootMer(model7,FUN=fun.conf_lmer1,nsim=1000,use.u=TRUE)
pred.lmer = bootMer(model7,FUN=fun.pred_lmer1,nsim=1000,use.u=TRUE) 
conf.lmer.muB = conf.lmer$t[,1]
conf.lmer.muC = apply(conf.lmer$t[,2:14], 1, mean)
conf.lmer.muA = conf.lmer$t[,15]
conf.lmer.muE = apply(conf.lmer$t[,16:21], 1, mean)
conf.lmer.muD = apply(conf.lmer$t[,22:34], 1, mean)
conf.lmer = tibble(
  SchoolA = conf.lmer.muA,
  schoolB = conf.lmer.muB,
  schoolC = conf.lmer.muC,
  schoolD = conf.lmer.muD,
  SchoolE = conf.lmer.muE
)
conf.out = apply(conf.lmer,2,quantile,probs=c(0.5,0.025,0.975))
# da_sub%>%group_by(School)%>%summarise(a = mean(Score))
pred.lmer.muB = pred.lmer$t[,1]
pred.lmer.muC = apply(pred.lmer$t[,2:14], 1, mean)
pred.lmer.muA = pred.lmer$t[,15]
pred.lmer.muE = apply(pred.lmer$t[,16:21], 1, mean)
pred.lmer.muD = apply(pred.lmer$t[,22:34], 1, mean)
pred.lmer = tibble(
  SchoolA = pred.lmer.muA,
  schoolB = pred.lmer.muB,
  schoolC = pred.lmer.muC,
  schoolD = pred.lmer.muD,
  SchoolE = pred.lmer.muE
)
pred.out = apply(pred.lmer,2,quantile,probs=c(0.5,0.025,0.975))

## stan
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

index = which(da$School %in% school_label$label)
index2 = da$School[index]
indexA = which(index2 == school_label$label[1])
indexB = which(index2 == school_label$label[2])
indexC = which(index2 == school_label$label[3])
indexD = which(index2 == school_label$label[4])
indexE = which(index2 == school_label$label[5])
da_stan = list(
  N = nrow(da),
  J = with(da,length(unique(School))),
  y = with(da, Score),
  x = with(da, SES),
  school = with(da,as.integer(as.factor(School)))
)
stan.2 = stan(file="hw02_pred.stan",data=da_stan)

e.conf = extract(stan.2, pars=c("mu2"))
conf = e.conf$mu2[,index]
conf.stan.muA = conf[,indexA]
conf.stan.muB = conf[,indexB]
conf.stan.muC = apply(conf[,indexC], 1, mean)
conf.stan.muD = apply(conf[,indexD], 1, mean)
conf.stan.muE = apply(conf[,indexE], 1, mean)
conf.stan.mu.Total = tibble(
  schoolA = conf.stan.muA,
  SchoolB = conf.stan.muB,
  schoolC = conf.stan.muC,
  SchoolD = conf.stan.muD,
  schoolE = conf.stan.muE
)
conf.stan = apply(conf.stan.mu.Total,2,quantile,probs=c(0.5,0.025,0.975))
rownames(conf.stan) = c("50%","2.5%","97.5%")

e.pred = extract(stan.2, pars=c("pred2"))
pred = e.pred$pred2[,index]
pred.stan.predA = pred[,indexA]
pred.stan.predB = pred[,indexB]
pred.stan.predC = apply(pred[,indexC], 1, mean)
pred.stan.predD = apply(pred[,indexD], 1, mean)
pred.stan.predE = apply(pred[,indexE], 1, mean)
pred.stan.pred.Total = tibble(
  SchoolA = pred.stan.predA,
  schoolB = pred.stan.predB,
  schoolC = pred.stan.predC,
  schoolD = pred.stan.predD,
  SchoolE = pred.stan.predE
)
pred.stan = apply(pred.stan.pred.Total,2,quantile,probs=c(0.5,0.025,0.975))
rownames(pred.stan) = c("50%","2.5%","97.5%")

answer14 = tibble(
  index = c(rep('lmer', 5), rep('stan', 5)),
  School = rep(c("A", "B", "C", "D", "E"), 2),
  median_conf = c(conf.out[1,], conf.stan[1,]),
  lower_conf = c(conf.out[2,], conf.stan[2,]),
  upper_conf = c(conf.out[3,], conf.stan[3,]),
  median_pred = c(pred.out[1,], pred.stan[1,]),
  lower_pred = c(pred.out[2,], pred.stan[2,]),
  upper_pred = c(pred.out[3,], pred.stan[3,])
)
ggplot(answer14, aes(School, median_conf, color = index)) +
  geom_errorbar(aes(ymin = lower_conf, ymax = upper_conf)) +
  geom_point(aes(School, median_conf, color = index))
ggplot(answer14, aes(School, median_pred, color = index)) +
  geom_errorbar(aes(ymin = lower_pred, ymax = upper_pred)) +
  geom_point(aes(School, median_pred, color = index))

# Q15 ---------------------------------------------------------------------


da_sub = da[which(da$School %in% school_label$label),] %>%
  arrange(School) 
da_sub3 = da_sub %>%
  group_by(School)%>%
  summarise(SES = 7)
da_sub3 = da_sub3[c(3,1,2,4,5),]


fun.pred_lmer1_2 = function(x){
  return( fun_lmer1(x, conf = FALSE, data = da_sub3) )
}
pred.lmer = bootMer(model7,FUN=fun.pred_lmer1_2,nsim=1000,use.u=TRUE) 

pred.out3 = apply(pred.lmer$t,2,quantile,probs=c(0.5,0.025,0.975))
rownames(pred.out3) = c("Median","Lower","Upper")
colnames(pred.out3) = c("SchoolA", "SchoolB", "SchoolC", "SchoolD", "SchoolE")

## stan

conf.stan.predA2 = conf[,indexA]
conf.stan.predB2 = conf[,indexB]
conf.stan.predC2 = conf[,indexC]
conf.stan.predD2 = conf[,indexD]
conf.stan.predE2 = conf[,indexE]
quantileB = quantile(conf.stan.predB2, probs=c(0.5,0.025,0.975))
quantileC = quantile(conf.stan.predC2, probs=c(0.5,0.025,0.975))
quantileA = quantile(conf.stan.predA2, probs=c(0.5,0.025,0.975))
quantileE = quantile(conf.stan.predE2, probs=c(0.5,0.025,0.975))
quantileD = quantile(conf.stan.predD2, probs=c(0.5,0.025,0.975))

pred.stan3 = as.matrix(data.frame(
  quantileA,quantileB,quantileC,quantileD,quantileE
))
rownames(pred.stan3) = c("Median","Lower","Upper")
colnames(pred.stan3) = c("SchoolA", "SchoolB", "SchoolC", "SchoolD", "SchoolE")

answer15 = tibble(
  index = c(rep('lmer', 5), rep('stan', 5)),
  School = rep(c("A", "B", "C", "D", "E"), 2),
  median_pred = c(pred.out3[1,], pred.stan3[1,]),
  lower_pred = c(pred.out3[2,], pred.stan3[2,]),
  upper_pred = c(pred.out3[3,], pred.stan3[3,])
)
ggplot(answer15, aes(School, median_pred, color = index)) +
  geom_errorbar(aes(ymin = lower_pred, ymax = upper_pred)) +
  geom_point(aes(School, median_pred, color = index))

# Q16 ---------------------------------------------------------------------

pred_lmer_16 = predict(model7, da)
residual_lmer = pred_lmer_16- da$Score

da_stan = list(
  N = nrow(da),
  J = with(da,length(unique(School))),
  y = with(da, Score),
  x = with(da, SES),
  school = with(da,as.integer(as.factor(School)))
)
e.conf = extract(stan.2, pars=c("mu2"))

pred_stan_16 = apply(e.conf$mu2, 2, mean)
residual_stan = pred_stan_16- da$Score

answer16 = tibble(
  Score = rep(da$Score, 2),
  SES = rep(da$SES, 2),
  School = rep(da$School, 2),
  number = c(c(1:length(residual_lmer), 1:length(residual_stan))),
  residual = c(residual_lmer, residual_stan),
  index = c(rep('lmer', length(residual_lmer)), rep('stan', length(residual_stan)))
)
ggplot(answer16) +
  geom_point(aes(number, residual, color = index))
ggplot(answer16) +
  geom_point(aes(Score, residual, color = index))
ggplot(answer16) +
  geom_point(aes(SES, residual, color = index))
ggplot(answer16) +
  geom_point(aes(School, residual, color = index))
