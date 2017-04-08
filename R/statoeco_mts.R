stato_eco_mts <- function(x, rif1){
temp <- data.frame(matrix(NA, nrow=nrow(x), ncol=2))
names(temp) <- c("Stato Ecologico", "Classe")
x <- data.frame(x,temp)
if(length(which(x[,9]<rif1[,12]))>0){
x[which(x[,9]<rif1[,12]),10] <- "CATTIVO"
x[which(x[,9]<rif1[,12]),11] <- 5}

if(length(which(x[,9]>=rif1[,12]&x[,9]<rif1[,11]))>0){
x[which(x[,9]>=rif1[,12]&x[,9]<rif1[,11]),10] <- "SCARSO"
x[which(x[,9]>=rif1[,12]&x[,9]<rif1[,11]),11] <- 4}

if(length(which(x[,9]>=rif1[,11]&x[,9]<rif1[,10]))>0){
x[which(x[,9]>=rif1[,11]&x[,9]<rif1[,10]),10] <- "SUFFICIENTE"
x[which(x[,9]>=rif1[,11]&x[,9]<rif1[,10]),11] <- 3}

if(length(which(x[,9]>=rif1[,10]&x[,9]<rif1[,9]))>0){
x[which(x[,9]>=rif1[,10]&x[,9]<rif1[,9]),10] <- "BUONO"
x[which(x[,9]>=rif1[,10]&x[,9]<rif1[,9]),11] <- 2}

if(length(which(x[,9]>rif1[,9]))>0){
x[which(x[,9]>=rif1[,9]),10] <- "ELEVATO"
x[which(x[,9]>=rif1[,9]),11] <- 1}
return(x)
}
