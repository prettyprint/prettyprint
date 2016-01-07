# http://r-pkgs.had.co.nz/r.html
.onLoad <- function(libname, pkgname) {
	op <- options()
	op.my <- list(
		## pp
		pp.digits = NA, 
		pp.signif.digits = 3,
		pp.round.digits = NA,
		pp.format.params = list(trim=TRUE, scientific=7, big.mark=",", small.mark="'"), 
		
		## pprop
		pprop.char = TRUE, 
		pprop.char.method = "fixed", 
		pprop.fixed.divisors = as.integer(c(1, 2, 3, 4, 5, 10)), 
		pprop.simple.signif.digits = 1, 
		pprop.simple.max.num = 50, 
		pprop.h = list(eq=0.05, about=0.1, ok=0.2)		
	)
	toset <- !(names(op.my) %in% names(op))
	if(any(toset)) options(op.my[toset])

	invisible()
}
