stato_eco <- function(x, rif1){
temp <- data.frame(matrix(NA, nrow=nrow(x), ncol=2))
names(temp) <- c("Stato Ecologico", "Classe")
x <- data.frame(x,temp)
if(length(which(x[,7]<rif1[,11]))>0){
x[which(x[,7]<rif1[,11]),8] <- "CATTIVO"
x[which(x[,7]<rif1[,11]),9] <- 5}

if(length(which(x[,7]>=rif1[,11]&x[,7]<rif1[,10]))>0){
x[which(x[,7]>=rif1[,11]&x[,7]<rif1[,10]),8] <- "SCARSO"
x[which(x[,7]>=rif1[,11]&x[,7]<rif1[,10]),9] <- 4}

if(length(which(x[,7]>=rif1[,10]&x[,7]<rif1[,9]))>0){
x[which(x[,7]>=rif1[,10]&x[,7]<rif1[,9]),8] <- "SUFFICIENTE"
x[which(x[,7]>=rif1[,10]&x[,7]<rif1[,9]),9] <- 3}

if(length(which(x[,7]>=rif1[,9]&x[,7]<rif1[,8]))>0){
x[which(x[,7]>=rif1[,9]&x[,7]<rif1[,8]),8] <- "BUONO"
x[which(x[,7]>=rif1[,9]&x[,7]<rif1[,8]),9] <- 2}

if(length(which(x[,7]>rif1[,8]))>0){
x[which(x[,7]>rif1[,8]),8] <- "ELEVATO"
x[which(x[,7]>rif1[,8]),9] <- 1}
return(x)
}

