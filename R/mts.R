#' mts
#'
#' Questa funzione permette il calcolo del Mayfly Total Score (Buffagni et al. 1997).
#' @param data.frame un oggetto di classe STAR o ISA
#' @details Se Siphlonurus risulta essere l'unico efemerottero nella comunità allora il suo punteggio passa da 3 a 1.
#' @keywords MTS
#' @references Buffagni A., 1997 Mayfly community composition and the biological quality of streams. In Landolt P. & Sartori M. (eds), Ephemeroptera & Plecoptera: Biology-Ecology-Systematics. MTL, Fribourg: 235-246
#' @export
#' @examples
#' macropen()


mts <- function (x)
{
    if (class(x) == "ISA") {
       if(length(x[[3]])==1&sum(x[[3]])==0){
    		return(rep(0, nrow(x[[1]])))}
    	if(length(x[[3]])==1&sum(x[[3]])!=0|length(x[[3]])!=1&sum(x[[3]])!=0){
        temp <- x[[4]]
        punt <- temp[, 1]
        data <- temp[, -1]
        data[data > 0] <- 1
        if (("SIPHLONURUS" %in% rownames(temp)) == FALSE) {
            data <- data * punt
            if (nrow(x[[3]]) == 1) {
                mts_v <- sum(data)
            }
            if (nrow(x[[3]]) > 1) {
                mts_v <- apply(data, 2, sum)
            }
            return(mts_v)
        }
        if (("SIPHLONURUS" %in% rownames(temp)) == TRUE) {
           bin <- data
           bin[bin>0] <- 1
if (nrow(x[[3]]) == 1){
bin <- sum(bin)
}
if (nrow(x[[3]]) > 1) {
bin <- apply(bin,2,sum)
}
if (nrow(x[[3]]) == 1) {
                siph <- data[which(rownames(temp) == "SIPHLONURUS")]
            }
            else {
                siph <- data["SIPHLONURUS", ]
            }
            con.siph <- which(bin == 1 & siph == 1)
            if (length(con.siph) == 0) {
		data <- data * punt
                if (nrow(x[[3]]) == 1) {
                  mts_v <- sum(data)
                }
                if (nrow(x[[3]]) > 1) {
                  mts_v <- apply(data, 2, sum)
                }
                return(mts_v)
            }
            else {
                temp.siph <- temp
                temp.siph["SIPHLONURUS", 1] <- 1
                punt.ship <- temp.siph[, 1]
                data[, con.siph] <- data[, con.siph] * punt.ship
                data[, -con.siph] <- data[, -con.siph] * punt
                if (nrow(x[[3]]) == 1) {
                  mts_v <- sum(data)
                }
                if (nrow(x[[3]]) > 1) {
                  mts_v <- apply(data, 2, sum)
                }
                return(mts_v)
            }
        }
    }
    else {
        "FORMATO DEI DATI SBAGLIATO, CONTROLLA PARAMETRO MTS"
    }
}
}

