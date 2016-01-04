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

```r
## Create a dataset
set.seed(42)
x.big = rnorm(12, mean=1e4, sd=1e3)
x.small = rnorm(12, mean=1e-3, sd=1e-3)
name = sample(levels(iris$Species), 12, replace=TRUE)
dd = data.frame(name, x.big, x.small)

pp(dd)	# typical usage
```

```
         name  x.big    x.small
1   virginica 11,400 -0.00038'9
2  versicolor  9,440  0.00072'1
3  versicolor 10,400  0.00086'7
4  versicolor 10,600    0.00164
5  versicolor 10,400  0.00071'6
6   virginica  9,890   -0.00166
7      setosa 11,500   -0.00144
8   virginica  9,910    0.00232
9   virginica 12,000  0.00069'3
10     setosa  9,940 -0.00078'1
11     setosa 11,300  0.00082'8
12 versicolor 12,300    0.00221
```

## User friendly descriptions of proportions

```r
set.seed(42)
x = data.frame(prop = runif(20))
x$desc = pprop(x$prop)	# pretty proportions
pp(x)
```

```
    prop             desc
1  0.915     about 9 / 10
2  0.937 more than 9 / 10
3  0.286           3 / 10
4   0.83      about 4 / 5
5  0.642      about 2 / 3
6  0.519            1 / 2
7  0.737            3 / 4
8  0.135 more than 1 / 10
9  0.657            2 / 3
10 0.705           7 / 10
11 0.458      about 1 / 2
12 0.719           7 / 10
13 0.935 more than 9 / 10
14 0.255            1 / 4
15 0.462      about 1 / 2
16  0.94 more than 9 / 10
17 0.978          49 / 50
18 0.117     about 1 / 10
19 0.475      about 1 / 2
20  0.56      about 3 / 5
```
