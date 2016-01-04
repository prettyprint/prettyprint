#' @title Approximations of proportions that look pretty and are easy to describe in words. 
#'
#' @description User friendly descriptions of proportions. 
#'
#' @details
#' Generally, you just need to supply a proportion, or a vector of proportions, which must be between 0 and 1. The approximate proportions may contain a descriptive prefix. This prefix indicates the distance between the proportion and the approximate proportion. 
#' 
#' When \code{char = NA}, then consider the length of the input (and the output). If length > 1, then return just the character representations of the proportions. This allows, for example, adding a column to a data.frame with these descriptions. When length == 1, return an object of class \code{pprop}, which contains more information. 
#' 
#' When returning a character, use the \code{char.method} proportion approximation method. However, if that method gives an answer that is not satisfactory -- the distance between the proportion and its approximation is too high -- then use the method with the lowest distance. 
#' 
#' The two methods are "fixed" and "simple". In "fixed", only the denominators specified in \code{fixed.divisors} are allowed. This is the more user-friendly method, though it might not give a good approximation when the proportion is close to 0 or to 1. This can be remedied by allowing more denominators, by changing \code{fixed.divisors}.
#' 
#' In the "simple" method, numerators between 0 and approximately \code{simple.max.num} are allowed. Denominators have \code{simple.signif.digits} significant digits. This method is more exact but less user friendly. 
#'
#' With default parameter values, the "fixed" method is used. When it does not perform well, which is near 0 and 1, the "simple" method is used automatically. 
#' 
#' The distance between a proportion and its approximation is measured using Cohen's h effect size. \code{h} is a list that must contain 3 elements: \code{eq}, \code{about}, \code{ok}. If h between a proportion and its approximation is less than \code{eq}, then they are so close that no descriptive prefix is used. If h is between \code{eq} and \code{about}, then the "about" prefix is used. If h is between \code{about} and \code{ok} then the "more than" / "less than" language is used. If h is greater than \code{ok}, then the approximation is considered to be too far away from the proportion. According to Cohen's rule of thumb, h < 0.20 indicates that the difference between two proportions is trivial. Therefore, by default, \code{h$ok = 0.20}. 
#' 
#' @param prop A proportion or vector of proportions. Must be between 0 and 1.
#' @param char TRUE (default) = return a character vector. FALSE = return objects of class \code{pprop}, which contain more information. NA = FALSE if \code{prop} is a singleton, TRUE if \code{prop} has more than one element.
#' @param char.method Which proportion approximation method to default to when \code{char = TRUE}? Possibilities are "fixed" (default) and "simple". "fixed" gives more user-friendly results, "simple" gives more exact results.
#' @param fixed.divisors For the "fixed" proportion approximation method, a vector of possible denominators. Only these denominators can be used in the approximation. 
#' @param simple.signif.digits For the "simple" proportion approximation method, the number of significant digits in the denominator. Default = 1. 
#' @param simple.max.num For the "simple" proportion approximation method, a value used in calculating the numerator. The maximum value of the numerator is approximately this, though it could be a little higher. 
#' @param h A list of cutoffs to determine how close the proportion is to the approximate proportion. Must have the following elements: \code{eq}, \code{about}, \code{ok}. See Details for an explanation. 
#' 
#' @return If \code{char = TRUE}, return a character vector. If \code{char = FALSE}, return objects of class \code{pprop}, which contain more information. Specifically, \code{pprop} has the following columns: 
#' \itemize{
#' 	\item method: The name of the approximation method.
#' 	\item prop: The proportion that was approximated.
#' 	\item num, denom: the numerator and denominator of the approximation.
#' 	\item e.prop: Decimal representation of the approximate proportion -- \code{num / denom}.
#' 	\item es.h: Distance between the proportion and its approximation, measured using effect size h. 
#' 	\item diff: Difference between the proportion and its approximation.
#' 	\item ok: Is \code{es.h < h$ok}?
#' 	\item prop.char: Approximation of the proportion, as a character. Possibly includes a descriptive prefix. When \code{char = TRUE}, this is what's returned. 
#' }
#'
#' @section References:
#' Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale,NJ: Lawrence Erlbaum.
#'
#' @seealso \code{\link[pwr]{ES.h}}, \code{\link[MASS]{fractions}}.
#' @keywords print utilities
#' 
#' @example R/examples/pprop.R
#' @export
pprop = function(prop, 
	char = getOption("pprop.char"), 
	char.method = getOption("pprop.char.method"), 
	fixed.divisors = getOption("pprop.fixed.divisors"), 
	simple.signif.digits = getOption("pprop.simple.signif.digits"), 
	simple.max.num = getOption("pprop.simple.max.num"), 
	h = getOption("pprop.h")) {
	
	char.method = match.arg(char.method, c("fixed", "simple"))
	
	stopifnot(
		(lp <- length(prop)) > 0, prop <= 1, prop >= 0, 
		is.na(char) || is.logical(char), 
		abs(fixed.divisors - round(fixed.divisors)) < .001, 
		fixed.divisors > 0, 
		simple.signif.digits > 0, 
		simple.max.num >= 1, 
		c("eq", "about", "ok") %in% names(h), 
		h$eq > 0, h$about >= h$eq, h$ok >= h$about)

	if ( lp == 1 ) {
		if (is.na(char)) {
			char = FALSE
		}
		ret = .do1(prop, fixed.divisors, simple.signif.digits, simple.max.num, h)
		
		if (char) {
			return(as.character(ret, char.method))			
		} else {
			return(ret)
		}		
	} else {
		if (is.na(char)) {
			char = TRUE
		}	
		ret = lapply(prop, .do1, 
			fixed.divisors=fixed.divisors, 
			simple.signif.digits=simple.signif.digits, 
			simple.max.num=simple.max.num, 
			h=h)
		if (char) {
			return(sapply(ret, as.character, char.method))
		} else {
			return(ret)
		}
	}
}

.do1 = function(prop, 
	fixed.divisors, simple.signif.digits, simple.max.num, h) {
	
	ret = rbind(.fixed1(prop, fixed.divisors), 
			.simple1(prop, simple.signif.digits, simple.max.num))

	ret$diff = abs(ret$e.prop - prop)
	ret$ok = (ret$es.h <= h$ok)
	ret$prop.char = ""
	for (ii in 1:nrow(ret)) {
		ret1 = ret[ii, ]
		denom1 = (abs(ret1$denom - 1) < 0.0001)
		
		prefix = NULL
		if (ret1$es.h <= h$eq) {
			prefix = NULL
			if (denom1 && prop > 0.0001 && prop < 0.9999) {
				prefix = "virtually"
			}
		} else if (ret1$es.h <= h$about) {
			prefix = "about"
		} else if (ret1$e.prop > ret1$prop && !denom1) {
			prefix = "less than"
		} else if (ret1$e.prop < ret1$prop && !denom1) {
			prefix = "more than"
		} else if (denom1) {
			prefix = "close to"
		} else {
			prefix = "ERROR"
		}
		
		if (!ret1$ok) {
			prefix = paste(c("BAD", prefix), collapse=" ")
		}
		
		if (denom1) {
			prop.char = paste( c(prefix, 
				.pp1(ret1$num, signif.digits=NA, round.digits=NA, format.params=getOption("pp.format.params"))), 
				collapse=" ")
		} else {
			prop.char = paste( c(prefix, 
				.pp1(ret1$num, signif.digits=NA, round.digits=NA, format.params=getOption("pp.format.params")), 
				"/", 
				.pp1(ret1$denom, signif.digits=NA, round.digits=NA, format.params=getOption("pp.format.params"))), 
				collapse=" ")			
		}
		ret$prop.char[ii] = prop.char
	}
	
	class(ret) = c("pprop")
	ret
}

.fixed1 = function(prop, divisors) {
	ret = data.frame(method="fixed", 
		prop = prop)

	dividends = (0:max(divisors))
	eprop = outer(dividends, divisors, FUN = "/")
	eprop[eprop > 1] = NA
	es.h = effect.size.h(eprop, prop)
	
	idx = arrayInd(which.min(es.h), dim(es.h))
	ret$num = dividends[idx[1]]
	ret$denom = divisors[idx[2]]
	ret$e.prop = eprop[idx[1], idx[2]]
	ret$es.h = es.h[idx[1], idx[2]]
	ret
}

.simple1 = function(prop, signif.digits, max.num) {
	if (prop < 0.5) {
		ret = .simple1(1 - prop, signif.digits, max.num)
		ret$prop = prop
		ret$num = ret$denom - ret$num
		ret$e.prop = 1 - ret$e.prop
		return(ret)
	}
	
	ret = data.frame(method="simple", 
		prop = prop)

	# prop >= 0.5
	inv = 1 / prop
	divisors = unique( signif(round((0:max.num) * inv, 0), signif.digits) )
	divisors = divisors[divisors > 0]

	dividends = (0:max(divisors))
	eprop = outer(dividends, divisors, FUN = "/")
	eprop[eprop > 1] = NA
	es.h = effect.size.h(eprop, prop)
	
	idx = arrayInd(which.min(es.h), dim(es.h))
	ret$num = dividends[idx[1]]
	ret$denom = divisors[idx[2]]
	ret$e.prop = eprop[idx[1], idx[2]]
	ret$es.h = es.h[idx[1], idx[2]]
	ret
}

###
#' Pretty print a \code{pprop} object. 
#'
#' @param x Output of \code{link{pprop}} when \code{char = FALSE}.
#' @param signif.digits See \code{link{pp}}.
#' @param round.digits See \code{link{pp}}.
#' @param format.params See \code{link{pp}}.
#' @param ... Ignored.
#'
#' @return \code{pp} prints \code{x}. Do not rely on the value, if any, that \code{pp} might return. 
#' @family pretty-print functions
#' @seealso \code{\link{pp}}, \code{\link[base]{print}}.
#' @keywords print utilities
#' @export
pp.pprop = function(x, 
	signif.digits=getOption("pp.signif.digits"), 
	round.digits=getOption("pp.round.digits"), format.params=getOption("pp.format.params"), 
	...) {
		
	class(x) = "data.frame"
	pp(x, 
		signif.digits = c(NA, rep(signif.digits, 4), NA, signif.digits, NA, NA), 
		round.digits = c(rep(NA, 5), 2, rep(NA, 3)), 
		format.params = format.params)
}

#' @describeIn pp.pprop Alias for \code{pp.pprop}.
#' @export
print.pprop = pp.pprop

###
#' @describeIn pprop Convert \code{pprop} object to character. For a list of \code{pprop} objects, use \code{sapply(x, as.character, char.method)}.
#'
#' @param x Output of \code{pprop} when \code{char = FALSE}.
#' @param ... Ignored.
#'
#' @export
as.character.pprop = function(x, char.method = getOption("pprop.char.method"), ...) {
	if (isTRUE(x$ok[idx <- (x$method == char.method)])) {
		return(x$prop.char[idx])
	} else {
		return(x$prop.char[which.min(x$es.h)])
	}
}

#' @describeIn pprop Convert \code{pprop} object to numeric. Simply returns the original proportion. 
#' @export
as.double.pprop = function(x, ...) {
	x$prop[1]
}

effect.size.h = function (p1, p2) 
{
    abs(2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2)))
}
