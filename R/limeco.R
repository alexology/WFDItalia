#' LIMeco
#'
#' This function calculates LIMeco.
#' @param X data.frame as specified in details.
#' @param STA column number of site names.
#' @param O2 column number of oxygen data.
#' @param NO3 column number of nitrate data.
#' @param NH4 column number of ammonium data.
#' @param TP column number of total phosporus data.
#' @keywords limECO
#' @export
#' @examples
#' year <- c(rep(2013,5), rep(2014,5))
#' STA <- rep(letters[1:2],5)
#' O2 <- rpois(10, 100)
#' NO3 <- abs(rnorm(10, 1.3, 0.5))
#' NH4 <- abs(rnorm(10, 0.5, 0.2))
#' TP <- abs(rnorm(10, 2.5, 1))
#' ex <- data.frame(year,STA,O2,NO3,NH4,TP)
#' ex
#' data <- LIMeco(ex,2,3,4,5,6)
#' # save in xlsx format, data will be stored in working directory (not run)
#' # write.xlsx(data, "limeco.xlsx")
#' tapply(data$LIMeco, list(ex$STA, ex$year), mean)

LIMeco <- function(X, STA=NULL, O2=NULL, NH4=NULL, NO3=NULL, TP=NULL, writexlsx=F){
if(missing(STA)|missing(O2)|missing(NH4)|missing(NO3)|missing(TP)){"FORNISCI NUMERO COLONNE"}
else{
li <- as.data.frame(matrix(NA, nrow=nrow(X), ncol=7))
if(length(li[which(abs(X[,O2]-100)<=10),1])>0) {li[which(abs(X[,O2]-100)<=10),1]<- 1}
if(length(li[intersect(which(abs(X[,O2]-100)>10),which(abs(X[,O2]-100)<=20)),1])>0) {li[intersect(which(abs(X[,O2]-100)>10),which(abs(X[,O2]-100)<=20)),1] <- 0.5}
if(length(li[intersect(which(abs(X[,O2]-100)>20),which(abs(X[,O2]-100)<=40)),1])>0) {li[intersect(which(abs(X[,O2]-100)>20),which(abs(X[,O2]-100)<=40)),1] <- 0.25}
if(length(li[intersect(which(abs(X[,O2]-100)>40),which(abs(X[,O2]-100)<=80)),1])>0) {li[intersect(which(abs(X[,O2]-100)>40),which(abs(X[,O2]-100)<=80)),1] <- 0.125}
if(length(li[which(abs(X[,O2]-100)>80),1])>0) {li[which(abs(X[,O2]-100)>80),1]<- 0}

if(length(li[which(X[,NH4]<0.03),2])>0) {li[which(X[,NH4]<0.03),2]<- 1}
if(length(li[intersect(which(X[,NH4]>=0.03),which(X[,NH4]<=0.06)),2])>0) {li[intersect(which(X[,NH4]>=0.03),which(X[,NH4]<=0.06)),2] <- 0.5}
if(length(li[intersect(which(X[,NH4]>0.06),which(X[,NH4]<=0.12)),2])>0) {li[intersect(which(X[,NH4]>0.06),which(X[,NH4]<=0.12)),2] <- 0.25}
if(length(li[intersect(which(X[,NH4]>0.12),which(X[,NH4]<=0.24)),2])>0) {li[intersect(which(X[,NH4]>0.12),which(X[,NH4]<=0.24)),2] <- 0.125}
if(length(li[which(X[,NH4]>0.24),2])>0) {li[which(X[,NH4]>0.24),2]<- 0}

if(length(li[which(X[,NO3]<0.6),3])>0) {li[which(X[,NO3]<0.6),3]<- 1}
if(length(li[intersect(which(X[,NO3]>=0.6),which(X[,NO3]<=1.2)),3])>0) {li[intersect(which(X[,NO3]>=0.6),which(X[,NO3]<=1.2)),3] <- 0.5}
if(length(li[intersect(which(X[,NO3]>1.2),which(X[,NO3]<=2.4)),3])>0) {li[intersect(which(X[,NO3]>1.2),which(X[,NO3]<=2.4)),3] <- 0.25}
if(length(li[intersect(which(X[,NO3]>2.4),which(X[,NO3]<=4.8)),3])>0) {li[intersect(which(X[,NO3]>2.4),which(X[,NO3]<=4.8)),3] <- 0.125}
if(length(li[which(X[,NO3]>4.8),3])>0) {li[which(X[,NO3]>4.8),3]<- 0}

if(length(li[which(X[,TP]<50),4])>0) {li[which(X[,TP]<50),4]<- 1}
if(length(li[intersect(which(X[,TP]>=50),which(X[,TP]<=100)),4])>0) {li[intersect(which(X[,TP]>=50),which(X[,TP]<=100)),4] <- 0.5}
if(length(li[intersect(which(X[,TP]>100),which(X[,TP]<=200)),4])>0) {li[intersect(which(X[,TP]>100),which(X[,TP]<=200)),4] <- 0.25}
if(length(li[intersect(which(X[,TP]>200),which(X[,TP]<=400)),4])>0) {li[intersect(which(X[,TP]>200),which(X[,TP]<=400)),4] <- 0.125}
if(length(li[which(X[,TP]>400),4])>0) {li[which(X[,TP]>400),4]<- 0}

li[,5] <- round(apply(li[,1:4], 1, mean),digits=2)

if(length(li[which(li[,5]<0.17),6])>0)  {li[which(li[,5]<0.17),6] <- 5}
if(length(li[intersect(which(li[,5]>=0.17),which(li[,5]<0.33)),6])>0) {li[intersect(which(li[,5]>=0.17),which(li[,5]<0.33)),6] <- 4}
if(length(li[intersect(which(li[,5]>=0.33),which(li[,5]<0.5)),6])>0) {li[intersect(which(li[,5]>=0.33),which(li[,5]<0.5)),6] <- 3}
if(length(li[intersect(which(li[,5]>=0.5),which(li[,5]<0.66)),6])>0) {li[intersect(which(li[,5]>=0.5),which(li[,5]<0.66)),6] <- 2}
if(length(li[which(li[,5]>=0.66),6])>0)  {li[which(li[,5]>=0.66),6] <- 1}

if(length(li[which(li[,5]<0.17),6])>0)  {li[which(li[,5]<0.17),7] <- "CATTIVO"}
if(length(li[intersect(which(li[,5]>=0.17),which(li[,5]<0.33)),6])>0) {li[intersect(which(li[,5]>=0.17),which(li[,5]<0.33)),7] <- "SCARSO"}
if(length(li[intersect(which(li[,5]>=0.33),which(li[,5]<0.5)),6])>0) {li[intersect(which(li[,5]>=0.33),which(li[,5]<0.5)),7] <- "SUFFICIENTE"}
if(length(li[intersect(which(li[,5]>=0.5),which(li[,5]<0.66)),6])>0) {li[intersect(which(li[,5]>=0.5),which(li[,5]<0.66)),7] <- "BUONO"}
if(length(li[which(li[,5]>=0.66),6])>0)  {li[which(li[,5]>=0.66),7] <- "ELEVATO"}

li <- data.frame(X[,c(STA,O2,NH4,NO3,TP)],li[,5:7])
colnames(li) <- c(colnames(X[,c(STA,O2,NH4,NO3,TP)]), "LIMeco","CLASSE","STATO ECOLOGICO")
if(writexlsx==T){writelim(li)}
return(li)
}
}
