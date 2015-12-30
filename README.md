# prettyprint
A suite of R functions that print values in a way that humans can easily understand.

## Installation
```{r}
install.packages("devtools")
devtools::install_github("prettyprint/prettyprint")
```

## Basic usage
```{r}
library(prettyprint)
pp(x)
```
where `x` is any object. For example: 

```{r}
set.seed(42)
x.big = rnorm(12, mean=1e4, sd=1e3)
x.small = rnorm(12, mean=1e-3, sd=1e-3)
name = sample(levels(iris$Species), 12, replace=TRUE)
dd = data.frame(name, x.big, x.small)

pp(dd)
```
