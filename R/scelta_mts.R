scelta_mts <- function(a=NULL){
if(missing(a)){
a <- select.list(levels(classificazione_mts[,1]), title="MACROTIPO")
if(nchar(a)==0){stop("STOPPATO DALL'UTENTE")}
sel_df <- classificazione_mts[which(classificazione_mts[,1]==a), 2:13]
return(sel_df)}
}
