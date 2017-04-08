scelta <- function(a=NULL){
if(missing(a)){
a <- select.list(levels(classificazione[,1]), title="IDROECOREGIONE")
if(nchar(a)==0){stop("STOPPATO DALL'UTENTE")}else{
b <- select.list(levels(droplevels(classificazione[which(classificazione[,1]==a),2])),title="REGIONE")}
if(nchar(b)==0){stop("STOPPATO DALL'UTENTE")}else{
d <- select.list(levels(droplevels(classificazione[which(classificazione[,2]==b&classificazione[,1]==a),3])), title="Classe di distanza dalla sorgente/Altro")}
if(nchar(d)==0){stop("STOPPATO DALL'UTENTE")}else{
e <- select.list(levels(droplevels(classificazione[which(classificazione[,3]==d&classificazione[,2]==b&classificazione[,1]==a),4])), title="Codice tipo")}
if(nchar(e)==0){stop("STOPPATO DALL'UTENTE")}else{
f <- select.list(levels(droplevels(classificazione[which(classificazione[,4]==e&classificazione[,3]==d&classificazione[,2]==b&classificazione[,1]==a),5])), title="mesohabitat")}
if(nchar(f)==0){stop("STOPPATO DALL'UTENTE")}else{
sel_df <- classificazione[which(classificazione[,1]==a&classificazione[,2]==b&classificazione[,3]==d&classificazione[,4]==e&classificazione[,5]==f), 6:16]
return(sel_df)}}
else {
if(length(a)==11){return(a)}else{"C'E' QUALCOSA CHE NON VA"}}
}
