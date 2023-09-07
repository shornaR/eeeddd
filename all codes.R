####mid2022#####
install.packages("MASS")
library(magrittr)
library(ggplot2)

require(foreign)
require(ggplot2)
require(MASS)
install.packages("readxl")
library(readxl)
data=read.csv(file.choose());data
data=read_excel(file.choose())
data
View(data)
head(data)
attach(data)
summary(data)
View(data)
dat <- within(data, {
  Sites <- factor(Sites, levels = 1:2, labels = c("Urban", "Rural"))
  Dengueif <- factor(Dengueif, levels = 0:1, labels = c("No", "Yes"))
  H.N. <- factor(H.N.)
  Dengueif=factor(Dengueif)
})
dat
model = glm.nb(AEF~SES+HCI+PCI+HT+BF+WS+k_cc+at_cc+p_cc+TL+k_d+at_d+p_d);model
summary(model)
#####logistic#####
library(aod)
library(ggplot2)
mydata=read.csv(file.choose(),header=T);mydata
View(mydata)
head(mydata)
summary(mydata)
sapply(mydata,sd)
xtabs(~admit+rank,data=mydata)
xtabs(~admit+gre,data=mydata)
xtabs(~admit+gpa,data=mydata)

mydata$rank=factor(mydata$rank)
mylogit=glm(admit~gre+gpa+rank,data=mydata,family="binomial");mylogit
summary(mylogit)
confint(mylogit)
confint.default(mylogit)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)
l <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), L = l)
## Wald test:
## ----------
## 
## Chi-squared test:
## X2 = 5.5, df = 1, P(> X2) = 0.019
## odds ratios only
exp(coef(mylogit))
## (Intercept) gre gpa rank2 rank3 rank4 
## 0.0185 1.0023 2.2345 0.5089 0.2618 0.2119
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))

newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
## view data frame
newdata1
newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
newdata1
newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100),
                                              4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))

newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
                                    se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
## view first few rows of final dataset
head(newdata3)
ggplot(newdata3, aes(x = gre, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
                                                                    ymax = UL, fill = rank), alpha = 0.2) + geom_line(aes(colour = rank),
                                                                                                                      size = 1)

with(mylogit, null.deviance - deviance)
with(mylogit, df.null - df.residual)
with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
logLik(mylogit)

library(sandwich)
library(msm)

p<-read.csv(file.choose(),header=T)
p 
View(P) 
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", 
                                            "Vocational"))
  id <- factor(id)
})
summary(p)
with(p, tapply(num_awards, prog, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
ggplot(p, aes(num_awards, fill = prog)) +
  geom_histogram(binwidth=.5, position="dodge")
summary(m1 <- glm(num_awards ~ prog + math, family="poisson", data=p))
summary(m1 <- glm(num_awards ~ prog + math, 
                  family="poisson", data=p))
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)
r.est
## update m1 model dropping prog
m2 <- update(m1, . ~ . - prog)
## test model differences with chi square test
anova(m2, m1, test="Chisq")
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4)), 
                 coef(m1), cov.m1)
## exponentiate old estimates dropping the p values
rexp.est <- exp(r.est[, -3])
## replace SEs with estimates for exponentiated coefficients
rexp.est[, "Robust SE"] <- s
rexp.est
(s1 <- data.frame(math = mean(p$math),
                  prog = factor(1:3, levels = 1:3, labels = 
                                  levels(p$prog))))
predict(m1, s1, type="response", se.fit=TRUE)
## calculate and store predicted values
p$phat <- predict(m1, type="response")
## order by program and then by math
p <- p[with(p, order(prog, math)), ]
## create the plot
ggplot(p, aes(x = math, y = phat, colour = prog)) +
  geom_point(aes(y = num_awards), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(x = "Math Score", y = "Expected number of awards")
library(foreign)
library(ggplot2)
library(MASS)
dat <- read.csv(file.choose(),header=T)
dat <- within(dat, {
  prog <- factor(prog, levels = 1:3, labels = c("General", "Academic", "Vocational"))
  id <- factor(id)
})
summary(dat)
ggplot(dat, aes(daysabs, fill = prog)) + geom_histogram(binwidth = 1) + facet_grid(prog ~ 
                                                                                     ., margins = TRUE, scales = "free")

with(dat, tapply(daysabs, prog, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
summary(m1 <- glm.nb(daysabs ~ math + prog, data = dat))
m2 <- update(m1, . ~ . - prog)
anova(m1, m2)

m3 <- glm(daysabs ~ math + prog, family = "poisson", data = dat)
pchisq(2 * (logLik(m1) - logLik(m3)), df = 1, lower.tail = FALSE)


zinb <- read.csv (file.choose(),header=T)
zinb <- within(zinb, {
  nofish <- factor(nofish)
  livebait <- factor(livebait)
  camper <- factor(camper)
})
summary(zinb)
## histogram with x axis in log10 scale
ggplot(zinb, aes(count)) + geom_histogram() + scale_x_log10()
mnull <- update(m1, . ~ 1)
pchisq(2 * (logLik(m1) - logLik(mnull)), df = 3, lower.tail = FALSE)

summary(p1 <- glm(count ~ child + camper, family = poisson, data = zinb))
vuong(p1, m1)

dput(coef(m1, "count"))
dput(coef(m1, "zero"))
## structure(c(1.29744027908309, -0.564347365357873), .Names = c("(Intercept)", 
## "persons"))
f <- function(data, i) {
  require(pscl)
  m <- zeroinfl(count ~ child + camper | persons, data = data[i, ],
    start = list(count = c(1.598, -1.0428, 0.834), zero = c(1.297, -0.564)
                 #-----------Analysis of Negative Binomial Regression with GLM------------
                 rm(list = ls())
                 
                  library(foreign)
                 data = read.dta(file.choose());head(data)#nb_data.dta
                 View(data)
                 data$prog = factor(data$prog, levels = 1:3, labels = c("General", "Academic", "Vocational"))
                 data$id = factor(data$id)
                 summary(data)
                 
                 library(ggplot2)
                 ggplot(data, aes(daysabs, fill = prog))+
                   geom_histogram(binwidth = 1) +
                   facet_grid(prog~., margins = T, scales = 'free')
                 
                 with(data, tapply(daysabs, prog, function(x){
                   sprintf('Mean (SD) = %1.2f  (%1.2f)', mean(x), sd(x))
                 }))
                 
                 library(MASS)
                 summary(m1 <- glm.nb(daysabs ~ math + prog, data = data))
                 
                 
                 
  #=======Final 2021=======
                 
dd = read.csv(file.choose());head(dd)     #Dengue Data.csv
  dd = within(dd, {
                   H.N. = factor(H.N.)
                   Sites = factor(Sites, levels = 0:1, labels = c('Rural', 'Urban'))
                   Dengueif = factor(Dengueif, levels = 0:1, labels = c('No', 'Yes'))
                   SES = factor(SES, levels = 1:3, labels = c('Poor', 'Intermediate', 'Wealthy'))
                   HCI = factor(HCI, levels = 1:3, labels = c('Not crowded', 'Medium crowded', 'Crowded'))
                   PCI = factor(PCI, levels = 5:10, labels = c('Low', 'Low', 'Medium', 'Medium', 'High', 'High'))
                   HT = factor(HT, levels = 1:2, labels = c('Single house, one family, two floors','Single house, one family, one floor'))
                   BF = factor(BF, levels = 1:2, labels = c('Cement','Tiles'))
                   WS = factor(WS, levels = 1:2, labels = c('Unscreened','Screened'))
                 })
    summary(dd)
                 
                 library(MASS)
                 g1 = glm.nb(AEF ~ SES + HCI + PCI + HT + TL + BF + WS + k_cc + at_cc + p_cc + 
                               k_d + at_d + p_d, data = dd)
                 summary(g1)
                 
                 
                 
                 #-------------END--------------#
                
                