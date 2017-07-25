require(rms)
require(Hmisc)
# 建立数据集（使用rms包example的代码，未改动）
n <- 1000
set.seed(731)
age <- 50 + 12*rnorm(n)
label(age) <- "Age"
sex <- factor(sample(c('Male','Female'), n, rep=TRUE, prob=c(.6, .4)))
cens <- 15*runif(n)
h <- .02*exp(.04*(age-50)+.8*(sex=='Female'))
dt <- -log(runif(n))/h
label(dt) <- 'Follow-up Time'
e <- ifelse(dt <= cens,1,0)
dt <- pmin(dt, cens)
units(dt) <- "Year"
# 设定nomogram的参数
ddist <- datadist(age, sex)
options(datadist='ddist')
# Cox回归
S <- Surv(dt,e)
# f <- cph(S ~ rcs(age,4) + sex, x=TRUE, y=TRUE)
f <- cph(S ~ rcs(age,4) + sex, x=TRUE, y=TRUE,surv = TRUE)
med <- Quantile(f)
# nomogram
nom <- nomogram(f, fun=function(x) med(x),
                fun.at=c(13,12,11,9,8,7,6,5),
                lp=F, funlabel="Median Survival Time")
plot(nom)
