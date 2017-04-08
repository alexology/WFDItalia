#' ooao
#'
#' Calculate ooao.
#' @param data.frame as specified in details.
#' @keywords data preparation
#' @export
#' @examples
#' ooao()

ooao <- function(LIMeco, STAR_ICMi,cex){
temp <- data.frame(LIMeco,STAR_ICMi)
names(temp) <- c("LIMeco", "STAR_ICMi")
d <- rep(NA,nrow(temp))
for(i in 1:10){
d[i] <- max(temp[i,1],temp[i,2])
}
temp <- data.frame(temp, d)
temp_col <- matrix(NA,nrow(temp), ncol=ncol(temp))
if(length(temp_col[which(temp==1)])>0){temp_col[which(temp==1)] <- "lightblue"}
if(length(temp_col[which(temp==2)])>0){temp_col[which(temp==2)] <- "green"}
if(length(temp_col[which(temp==3)])>0){temp_col[which(temp==3)] <- "yellow"}
if(length(temp_col[which(temp==4)])>0){temp_col[which(temp==4)] <- "orange"}
if(length(temp_col[which(temp==5)])>0){temp_col[which(temp==5)] <- "red"}
plot(c(1:nrow(temp)) ~ c(1:(ncol(temp_col)), rep(3,nrow(temp_col)-ncol(temp_col))), type="n", xlim=c(1.3,ncol(temp_col)+0.3), axes=F, xlab="METRICHE", ylab="STAZIONE", las=2)
sequi <- seq(1.5, ncol(temp_col),0.5)
for(i in 1:ncol(temp_col)){
points(c(1:nrow(temp)) ~ rep(sequi[i],nrow(temp)), pch=22, col=rev(temp_col[,i]), bg=rev(temp_col[,i]), cex=cex)
}
lab <-c((colnames(temp[,1:(ncol(temp)-1)])),"ooao", "metrica")
axis(1, at=sequi[1:(ncol(temp_col)+1)], labels=lab)
axis(2, at=c(1:nrow(temp)),labels=rev(rownames(temp)), las=2)
box(lwd=2)
}
