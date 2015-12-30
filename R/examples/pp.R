## Create a dataset
set.seed(42)
x.big = rnorm(12, mean=1e4, sd=1e3)
x.small = rnorm(12, mean=1e-3, sd=1e-3)
name = sample(levels(iris$Species), 12, replace=TRUE)
dd = data.frame(name, x.big, x.small)

pp(dd)	# typical usage
pp(dd, b=TRUE)	# show original and formatted

flowers = sample(levels(iris$Species), 1e4, replace=TRUE)
pp(table(flowers))	# table

pp(dd, s=NA)	# format only, don't round
pp(dd$x.big, r=2)	# show 2 decimal places
# different signif.digits and round.digits for each column
pp(dd, s=c(NA, NA, 3), r=c(NA, 2, NA))

mm = tcrossprod(x.big, x.small)
pp(mm)	# matrix
pp(qr(mm))	# list

## Linear Models -- example from lm
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)

summary(lm.D9)
pp(lm.D9)	# cleaner output

## Logit Regression -- example from http://statistics.ats.ucla.edu/stat/r/dae/logit.htm
mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
mydata$rank <- factor(mydata$rank)
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")

summary(mylogit)
pp(mylogit)	# Odds ratios, 95% CI's
