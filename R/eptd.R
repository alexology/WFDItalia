#' EPTD
#'
#' Questa funzione permette il calcolo del logartimo degli EPTD selezionati (Efemerotteri, Plecotteri, Tricotteri ed Efemerotteri)
#' @param un oggetto di classe STAR o ISA
#' @keywords EPTD
#' @details La metrica EPTD viene calcolata utilizzando la formula
#' log10(Sel_EPTD+1)
#' dove log10 rappresenta il logaritmo in base 10 e SEL_EPTD l'abbondanza degli EPTD selezionati.
#' @export
#' @examples
#' load(esempio_su)
#' surber <- data.prep(esempio_sub)
#' EPTD(surber)
#' # lista degli EPTD
#' rownames(aspt_v[aspt_v$EPTD==1,])

EPTD <- function(x){
		EPTD_ul <- t(x[[1]])*x[[2]]$EPTD
		return(round(log(apply(EPTD_ul,2,sum)+1,10),3))
}
