# http://r-pkgs.had.co.nz/r.html
.onLoad <- function(libname, pkgname) {
	op <- options()
	op.my <- list(
		pp.signif.digits = 3,
		pp.round.digits = NA,
		pp.format.params = list(trim=TRUE, scientific=7, big.mark=",", small.mark="'")
	)
	toset <- !(names(op.my) %in% names(op))
	if(any(toset)) options(op.my[toset])

	invisible()
}
