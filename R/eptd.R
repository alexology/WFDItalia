#' EPTD
#'
#' This funcion calculates the log10(Sel_EPTD+1) metric.
#' @param x a data.prep object.
#' @keywords EPTD
#' @details This metric is calculated according to the following formula:
#' log10(Sel_EPTD+1)
#' where log10 is the base-10 lagarithm and SEL_EPTD the abundances of selected families of Ephemeroptera, Plecoptera, Trichoptera and Diptera plus 1.
#' @export
#' @examples
#' data(oglio)
#' oglio.prep <- data.prep(oglio)
#' EPTD(oglio.prep)
#' # list of EPTD families
#' rownames(aspt_v[aspt_v$EPTD==1,])

EPTD <- function(x){
		EPTD_ul <- t(x[[1]])*x[[2]]$EPTD
		return(round(log(apply(EPTD_ul,2,sum)+1,10),3))
}
