#' aspt
#'
#' Questa funzione permette il calcolo dell'Average Score Per Taxon (ASPT).
#' @param x data.frame as specified in details.
#' @keywords ASPT
#' @details Il calcolo viene effettuato utilizzando i punteggi definiti da Davy-Bowker et al. 2008. ? possibile visionare i punteggi digitando aspt_v e premendo invio.
#' @references Davy-Bowker J., Clarke R., Corbin T., Vincent H, Pretty J., Hawczak A., Blackburn J., Murphy J., Jones I., 2008. River Invertebrate Classification Tool. Final report. WFD72C. SNIFFER. 276 pp
#' @export
#' @examples
#' data(esempio_su)
#' surber <- data.prep(esempio_su)
#' aspt(surber)

aspt <- function(x){
		b <- as.data.frame(matrix(0, nrow=nrow(x[[1]]),ncol=nrow(aspt_acc)))
		colnames(b) <- aspt_acc[,1]
		rownames(b) <- rownames(x[[1]])
		rr <- cbind(x[[1]],b)
		t_rr <- t(rr)
		t <- sapply(by(t_rr,rownames(t_rr),colSums),identity)
            if(nrow(x[[1]])==1){
		t <- as.data.frame(t(as.matrix(t)))}
		rnt <- which(colnames(t) %in% aspt_acc[,1])
		a1 <- data.frame(lapply(aspt_acc, as.character), stringsAsFactors=FALSE)
		for(i in 1:nrow(aspt_acc)){
		colnames(t)[which(colnames(t)==a1[i,1])] <- a1[i,2]
		}
		t_rr2 <- t(t)
		t <- sapply(by(t_rr2,rownames(t_rr2),colSums),identity)
		if(nrow(x[[1]])==1){
		t <- as.data.frame(t(as.matrix(t)))}
		r_in1 <- which(rownames(aspt_v) %in% colnames(t))
		aspt_v2 <- na.omit(aspt_v[r_in1,])
		x_bin <- t
		x_bin[x_bin>0] <- 1
		r_in2 <- which(colnames(x_bin) %in% rownames(aspt_v2))
		x_bin <- x_bin[,r_in2]
		bmwp_sam <-  apply(t(x_bin)*aspt_v2$punteggio,2,sum)
		aspt_sam <- bmwp_sam/specnumber(x_bin)
		aspt_sam[is.nan(aspt_sam)] <- 0
		return(round(aspt_sam,3))
		}
