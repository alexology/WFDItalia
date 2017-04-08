#' 1-GOLD
#'
#' Calculate 1-GOLD.
#' @param x un oggetto di classe ISA O STAR.
#' @keywords preparazione dei dati
#' @export
#' @examples
#' 1-GOLD()

gold <- function(x){
		gold_row <- row.names(subset(x[[2]], gruppo=="Gastropoda"|gruppo=="Oligochaeta"|gruppo=="Diptera"))
		x_g <- x[[1]][,gold_row,drop=FALSE]
            if(nrow(x[[1]])==1){
            abu_tot <- sum(x[[1]])
		abu_gold <- sum(x_g)
		}
		else{
		abu_tot <- apply(x[[1]], 1, sum)
		abu_gold <- apply(x_g, 1, sum)}
		gold_c <- round(1-abu_gold/abu_tot,3)
		return(gold_c)
}

