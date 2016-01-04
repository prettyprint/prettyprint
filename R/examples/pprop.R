# Basic usage
## one proportion
pprop(0.618034)	# 3 / 5
## descriptive prefix is added when necessary
pprop(0.1234)	# about 1 / 10
## default method ("fixed") does not produce
## a satisfactory answer -- switching to 
## another method
pprop(0.01234)	# about 1 / 50

## many proportions
set.seed(42)
x = data.frame(prop = runif(100))
x$desc = pprop(x$prop)
pp(x)

# Show details: char=FALSE or options(pprop.char = FALSE)
options(pprop.char = FALSE)

pprop(0.618034)	# 3 / 5 or 37 / 60
## Not as pretty
MASS::fractions(0.618034)	# 55/89

## "fixed" method does not produce a satisfactory answer
pprop(2 / 123)
## Can use the "simple" method, OR allow more divisors for the "fixed" method.
pprop(2 / 123, 
	fixed.divisors = c(getOption("pprop.fixed.divisors"), 20))

## Increasing simple.signif.digits makes results of the 
## "simple" method less pretty but more exact.
pprop(0.618034, simple.signif.digits = 2)

# compare "fixed" method, "simple" method, and MASS::fractions
options(pprop.char = TRUE)	# reset this option
x = data.frame(prop = runif(100))
x$prop.fixed = pprop(x$prop)
x$prop.simple = pprop(x$prop, char.method = "simple")
x$prop.MASS = as.character(MASS::fractions(x$prop))
pp(x)

## Above, pprop() runs twice -- once for each method.
## With a lot of data, you might want to do this, 
## which only runs pprop() once. 
x = data.frame(prop = runif(100))
pprop. = pprop(x$prop, char=FALSE)
x$prop.fixed = sapply(pprop., as.character, "fixed")
x$prop.simple = sapply(pprop., as.character, "simple")
x$prop.MASS = as.character(MASS::fractions(x$prop))
pp(x)

## All proportions from 0.0% to 100.0%.
n = 1000
x = data.frame(x = (0:n), n)
x$prop = x$x / x$n
pprop. = pprop(x$prop, char=FALSE)
x$prop.fixed = sapply(pprop., as.character, "fixed")
x$prop.simple = sapply(pprop., as.character, "simple")
x$prop.MASS = as.character(MASS::fractions(x$prop))
pp(x)
