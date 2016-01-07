## Create a dataset
set.seed(42)
x.big = rnorm(12, mean=1e4, sd=1e3)
x.small = rnorm(12, mean=0, sd=0.5)
name = sample(levels(iris$Species), 12, replace=TRUE)
dd = data.frame(name, x.big, x.small)

pp(dd)			# typical usage
pp(dd, b=TRUE)	# show original and formatted

pp(dd, s=2)		# 2 significant digits
pp(dd, r=2)		# 2 decimal places
# 2 decimal places for small numbers (-1 to 1), 2 significant digits otherwise
pp(dd, d=2)	

pp(dd, s=NA)	# format only, don't round
pp(dd, r=-3)	# round to the nearest thousand

# different signif.digits and round.digits for each column
pp(dd, s=c(NA, NA, 3), r=c(NA, 2, NA))

mm = tcrossprod(x.big, x.small)
pp(mm)	# matrix
pp(qr(mm))	# list

flowers = sample(levels(iris$Species), 1e4, replace=TRUE)
pp(table(flowers))	# table

## Linear Models -- example from lm
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)

summary(lm.D9)
pp(lm.D9)	# cleaner output

## Logistic regression
## Create a dataset
set.seed(42)
n = 100
dd = data.frame(x1 = runif(n), x2 = runif(n))
dd = within(dd, {
		x3 = 2 * x1 + x2 + rnorm(n)
		A = factor(cut(x1, 3, FALSE))
		B = factor(cut(x2, 3, FALSE))
		y = factor(cut(x3, 2, FALSE))
	})

gg = glm(y ~ A + B, data = dd, family = "binomial")
summary(gg)	
pp(gg)	# Odds ratios, 95% CI's
