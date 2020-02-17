#' ept
#'
#' This function calculates the EPT metric (number of Plecoptera, Eèhemeroptera and Trichoptera families)
#' @param x a data.prep object.
#' @keywords EPT
#' @export
#' @examples
#' data(oglio)
#' oglio.prep <- data.prep(oglio)
#' ept(oglio)


ept <- function (x){
    				x_ept <- x[[1]]
				ept_taxa <- aspt_v[which(aspt_v$gruppo == "Plecoptera" |
        			aspt_v$gruppo == "Ephemeroptera" | aspt_v$gruppo == "Trichoptera"),]
				ept_m <- match(colnames(x_ept), rownames(ept_taxa))
    				if(sum(as.numeric(is.na(ept_m)))==ncol(x_ept)){
    					return(rep(0,nrow(x_ept)))
    				}
				else{
    					aspt_v1 <- x[[2]]
    					ept_row <- row.names(subset(aspt_v1, gruppo == "Plecoptera" |
        				gruppo == "Ephemeroptera" | gruppo == "Trichoptera"))
    					x_ept <- x_ept[, ept_row, drop = FALSE]
    					return(specnumber(x_ept))
				}
}

