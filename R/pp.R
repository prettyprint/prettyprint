## Main call
#' Pretty print any object. 
#'
#' @param x An object to print.
#' @param both Show both, the \code{print} (unformatted) version and the formatted version of \code{x}? FALSE (default) only shows the formatted version.
#' @param signif.digits Integer indicating the number of significant digits to be used, or NA. Ignored if round.digits is not NA. See \code{\link[base]{signif}}. 
#' @param round.digits Integer indicating the number of decimal places to be used, or NA. NA (default) means that \code{signif.digits} is used. See \code{\link[base]{round}}. 
#' @param format.params Parameters passed to \code{\link[base]{format}}. 
#' 
#' @section Details:
#' At its core, \code{pp} is a wrapper around \code{\link[base]{signif}}, \code{\link[base]{round}}, and \code{\link[base]{format}}. Currently, methods are defined for the following classes: 
#' \itemize{
#' 	\item integer, numeric
#'	\item matrix, array
#' 	\item data.frame (optionally, \code{signif.digits} and \code{round.digits} parameters can be vectors, in which case a different value applies to each column of the data.frame)
#' 	\item table (output from \code{\link[base]{table}} and \code{\link[stats]{xtabs}})
#' 	\item lm, glm (prints odds ratios for logistic regression)
#' 	\item AsIs (output from \code{\link[base]{I}})
#' 	\item list
#' }
#'
#' @return \code{pp} prints \code{x}. Do not rely on the value, if any, that \code{pp} might return. 
#' 
#' @seealso \code{\link[base]{print}}, \code{\link[base]{signif}}, \code{\link[base]{round}}, \code{\link[base]{format}}, \code{\link[base]{summary}}. 
#' @keywords print utilities
#' 
#' @example R/examples/pp.R
#' @import MASS
#' @export
pp = function(x=NA, 
	both=FALSE, 
	signif.digits=getOption("pp.signif.digits"), 
	round.digits=getOption("pp.round.digits"), format.params=getOption("pp.format.params")) {	
	
	.pp(x=x, both=both, signif.digits=signif.digits, round.digits=round.digits, format.params=format.params)
}

.pp = function(x, both, signif.digits, round.digits, format.params) {
	UseMethod("pp")
}

.pp1 = function(x, signif.digits, round.digits, format.params) {
	# round or signif, not both
	if (!is.na(round.digits)) {
		x = round(x, round.digits)
		format.params$digits = format.params$nsmall = round.digits
	} else if (!is.na(signif.digits)) {
		x = signif(x, signif.digits)
	}
	format.params$x = x
	do.call(format, format.params)
}

### Call other pp methods
pp.glm = function(x, ...) {
	sx = summary(x)
	### print(sx)
	
	cf = as.data.frame(sx$coefficients)
	names(cf)[names(cf) %in% c("Pr(>|t|)", "Pr(>|z|)")] = "p-value"
	cf[, "_"] = " "
	cf$`_`[cf$`p-value` <= 0.1] = "."
	cf$`_`[cf$`p-value` <= 0.05] = "*"
	cf$`_`[cf$`p-value` <= 0.01] = "**"
	cf$`_`[cf$`p-value` <= 0.001] = "***"	
	if (isTRUE(x$family$link == "logit")) {	
		or = as.data.frame(exp(cbind(`Odds Ratio` = coef(x), confint(x))))
		c.copy = c("p-value", "_")
		or[, c.copy] = cf[, c.copy]
		or = or[rownames(or) != "(Intercept)", ]
		
		dots = list(...)
		dots$both = FALSE
		dots$round.digits = c(2, 2, 2, 3, NA)
		dots$x = or
		do.call(pp, dots)
	} else {
		dots = list(...)
		dots$both = FALSE
		dots$x = cf
		dots$round.digits = c(dots$round.digits, dots$round.digits, 2, 3, NA)
		do.call(pp, dots)
	}
}
pp.lm = pp.glm

pp.table = function(x, ...) {
	x = unclass(x)
	attr(x, "call") = NULL
	pp.array(x, ...)
}

### Basic pp methods
pp.AsIs = function(x, ...) {
	print(x)
}

pp.numeric = function(x, both, signif.digits, round.digits, format.params) {
	if (length(x) > 0) {
		.ppb(x, both)
		x = sapply(x, .pp1, signif.digits, round.digits, format.params)
		print(x, quote=FALSE, right=TRUE)
	} else {
		print(x)
	}
}
pp.integer = pp.numeric

pp.matrix = function(x, both, signif.digits, round.digits, format.params) {
	if (is.numeric(x) && all(dim(x) > 0)) {
		.ppb(x, both)
		x[] = sapply(x, .pp1, signif.digits, round.digits, format.params)
		print(x, quote=FALSE, right=TRUE)
	} else {
		print(x)
	}
}
pp.array = pp.matrix

pp.data.frame = function(x, both, signif.digits, round.digits, format.params) {
	changed = FALSE
	if ((nc <- ncol(x)) > 0 && nrow(x) > 0) {
		ii = which(sapply(x, is.numeric))
		if (length(ii) > 0) {
			changed = TRUE
			.ppb(x, both)
			
			if (length(signif.digits) == 1 &&  length(round.digits) == 1) {
				for (jj in ii) {
					x[, jj] = sapply(x[, jj], .pp1, signif.digits, round.digits, format.params)
				}
			} else {
				signif.digits = signif.digits + rep(0, nc)
				round.digits = round.digits + rep(0, nc)
				for (jj in ii) {
					x[, jj] = sapply(x[, jj], .pp1, signif.digits[jj], round.digits[jj], format.params)
				}				
			}
			print(x, quote=FALSE, right=TRUE)
		}
	}
	if (!changed) {
		print(x)
	}
}

### Default
pp.list = function(x, ...) {
	if (length(x) > 0) {
		nm = names(x)
		for (ii in 1:length(x)) {
			cat("***", nm[ii], "(Begins) ***\n")
			pp(x[[ii]], ...)
			cat("***", nm[ii], "(Ends) ***\n")
		}
	} else {
		print(x)
	}
}

pp.default = function(x, ...) {
	if (is.list(x)) {
		pp.list(x, ...)
	} else {
		print(x)
	}
}

###
.ppb = function(x, both) {
	if (isTRUE(both)) {
		cat("Raw:\n")
		print(x)
		cat("\nFormatted:\n")
	}
}
