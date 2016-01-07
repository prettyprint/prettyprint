## Main call
#' @title Pretty print any object. 
#' 
#' @description
#' Print values in a way that humans can easily understand.
#'
#' @details
#' At its core, \code{pp} is a wrapper around \code{\link[base]{signif}}, \code{\link[base]{round}}, and \code{\link[base]{format}}. Currently, methods are defined for the following classes: 
#' \itemize{
#' 	\item integer, numeric
#'	\item matrix, array
#' 	\item data.frame (optionally, the ``digits'' parameters can be vectors, in which case a different value applies to each column of the data.frame)
#' 	\item table (output from \code{\link[base]{table}} and \code{\link[stats]{xtabs}})
#' 	\item lm, glm (prints odds ratios for logistic regression)
#' 	\item AsIs (output from \code{\link[base]{I}})
#' 	\item list
#' 	\item pprop (output from \code{\link{pprop}} -- ``pretty proportion'')
#' }
#'
#' If a ``digits'' parameter is \code{NA}, it is ignored. At most, only one non-NA ``digits'' parameter is used. Their order of precedence is: \code{digits}, \code{round.digits}, \code{signif.digits}. By default, \code{digits} and \code{round.digits} are \code{NA}. 
#'
#' @param x An object to print.
#' @param both Show both, the \code{print} (unformatted) version and the formatted version of \code{x}? \code{FALSE} (default) only shows the formatted version.
#' @param digits For \code{x} between -1 and 1, this is the same as \code{round.digits}. For \code{x} outside of the -1 to 1 range, same as \code{signif.digits}. \code{NA} (default) = ignore.
#' @param signif.digits Integer indicating the number of significant digits to be used, or \code{NA}. Ignored if \code{digits} or \code{round.digits} is not \code{NA}. See \code{\link[base]{signif}}. 
#' @param round.digits Integer indicating the number of decimal places to be used, or \code{NA}. \code{NA} (default) = ignore. Ignored if \code{digits} is not \code{NA}. See \code{\link[base]{round}}. 
#' @param format.params Parameters passed to \code{\link[base]{format}}. 
#' 
#' @return \code{pp} prints \code{x}. Do not rely on the value, if any, that \code{pp} might return. 
#' 
#' @family pretty-print functions
#' @seealso \code{\link[base]{print}}, \code{\link[base]{signif}}, \code{\link[base]{round}}, \code{\link[base]{format}}, \code{\link[base]{summary}}. 
#' @keywords print utilities
#' 
#' @example R/examples/pp.R
#' @import MASS
#' @export
pp = function(x=NA, 
	both=FALSE, 
	digits = getOption("pp.digits"),
	signif.digits = getOption("pp.signif.digits"), 
	round.digits = getOption("pp.round.digits"), format.params = getOption("pp.format.params")) {	
	
	.pp(x=x, both=both, digits=digits, signif.digits=signif.digits, 
		round.digits=round.digits, format.params=format.params)
}

.pp = function(x, both, digits, signif.digits, round.digits, format.params) {
	UseMethod("pp")
}

.pp1 = function(x, digits, signif.digits, round.digits, format.params) {
	# at most do one of these, in this order: digits, round, signif
	if (!is.na(digits)) {
		if (isTRUE(x < 1) && isTRUE(x > -1)) {	# round
			x = round(x, digits)
			format.params$digits = format.params$nsmall = max(0, digits)
		} else {	# signif
			x = signif(x, digits)			
		}
	} else if (!is.na(round.digits)) {
		x = round(x, round.digits)
		format.params$digits = format.params$nsmall = max(0, round.digits)
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

pp.numeric = function(x, both, digits, signif.digits, round.digits, format.params) {
	if (length(x) > 0) {
		.ppb(x, both)
		x = sapply(x, .pp1, digits, signif.digits, round.digits, format.params)
		print(x, quote=FALSE, right=TRUE)
	} else {
		print(x)
	}
}
pp.integer = pp.numeric

pp.matrix = function(x, both, digits, signif.digits, round.digits, format.params) {
	if (is.numeric(x) && all(dim(x) > 0)) {
		.ppb(x, both)
		x[] = sapply(x, .pp1, digits, signif.digits, round.digits, format.params)
		print(x, quote=FALSE, right=TRUE)
	} else {
		print(x)
	}
}
pp.array = pp.matrix

pp.data.frame = function(x, both, digits, signif.digits, round.digits, format.params) {
	changed = FALSE
	if ((nc <- ncol(x)) > 0 && nrow(x) > 0) {
		ii = which(sapply(x, is.numeric))
		if (length(ii) > 0) {
			changed = TRUE
			.ppb(x, both)
			
			lmd = length(digits)
			lsd = length(signif.digits)
			lrd = length(round.digits)
			if (lmd == 1L && lsd == 1L && lrd == 1L) {
				for (jj in ii) {
					x[, jj] = sapply(x[, jj], .pp1, digits, signif.digits, round.digits, format.params)
				}
			} else {
				stopifnot(lmd == 1L | lmd == nc, lsd == 1L | lsd == nc, lrd == 1L | lrd == nc)
				if (lmd == 1L) digits = rep(digits, nc)
				if (lsd == 1L) signif.digits = rep(signif.digits, nc)
				if (lrd == 1L) round.digits = rep(round.digits, nc)
				for (jj in ii) {
					x[, jj] = sapply(x[, jj], .pp1, 
						digits[jj], signif.digits[jj], round.digits[jj], format.params)
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
