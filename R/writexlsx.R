writexlsx <- function(X){
z <- "1. Sì"
nam <- list.files()
if(sum(match(nam,"STAR_ICMi.xlsx"),na.rm=T)==1){
z <- select.list(c("1. Sì", "2. No"), title="Sovrascivere STAR_ICMi.xls?")
}
if(sum(match(nam,"STAR_ICMi.xlsx"),na.rm=T)==0|sum(match(nam,"STAR_ICMi.xlsx"),na.rm=T)==1&z=="1. Sì?"){
wb <- createWorkbook()
sheet <- createSheet(wb, sheetName="STAR_ICMi")
ref.met <-  X[[1]][1:7]
names(ref.met) <- c("ASPT", "N_Fam", "N_EPT_Fam", "1-GOLD", "Diversità di Shannon","log SelEPTD", "STAR_ICMi")
ref.cla <- X[[1]][8:11]
names(ref.cla) <- c("Elevato-Buono", "Buono-Sufficiente", "Sufficiente-Scarso", "Scarso-Cattivo")
cs <- CellStyle(wb) + Alignment(h="ALIGN_CENTER")
cs1 <- CellStyle(wb) + Font(wb, isItalic=TRUE) + Alignment(h="ALIGN_CENTER")
cs2 <- CellStyle(wb) + Font(wb, isBold=TRUE)+ Alignment(h="ALIGN_CENTER")
addDataFrame(ref.met, sheet, startRow=1, startColumn=1, row.names=F, colnamesStyle=cs2, colStyle=list("1"=cs,"2"=cs,"3"=cs,"4"=cs,"5"=cs,"6"=cs,"7"=cs))
addDataFrame(ref.cla, sheet, startRow=4, startColumn=1, row.names=F, colnamesStyle=cs2,, colStyle=list("1"=cs,"2"=cs,"3"=cs,"4"=cs))
addDataFrame(X[[2]], sheet, startRow=9, startColumn=1, rownamesStyle=cs1, colnamesStyle=cs2, colStyle=list("1"=cs,"2"=cs,"3"=cs,"4"=cs,"5"=cs,"6"=cs,"7"=cs,"8"=cs,"9"=cs,"10"=cs))
rowm1 <-  nrow(X[[2]])
cb <- CellBlock(sheet, 10, 9, rowm1, 1, create=T)
CB.setColData(cb, 9, 1)
CB.setRowData(cb, 10:rowm1, 1)
x <- as.matrix(X[[2]][,8])
CB.setMatrixData(cb, x, 1, 1)
fill <- Fill(foregroundColor = "red", backgroundColor="red")
fill1 <- Fill(foregroundColor = "orange", backgroundColor="orange")
fill2 <- Fill(foregroundColor = "yellow", backgroundColor="yellow")
fill3 <- Fill(foregroundColor = "green", backgroundColor="green")
fill4 <- Fill(foregroundColor = "blue", backgroundColor="blue")
ind <- which(x == "CATTIVO", arr.ind=TRUE)
ind1 <- which(x == "SCARSO", arr.ind=TRUE)
ind2 <- which(x == "SUFFICIENTE", arr.ind=TRUE)
ind3 <- which(x == "BUONO", arr.ind=TRUE)
ind4 <- which(x == "ELEVATO", arr.ind=TRUE)
CB.setFill(cb, fill, ind[,1], ind[,2])
CB.setFill(cb, fill1, ind1[,1], ind1[,2])
CB.setFill(cb, fill2, ind2[,1], ind2[,2])
CB.setFill(cb, fill3, ind3[,1], ind3[,2])
CB.setFill(cb, fill4, ind4[,1], ind4[,2])
saveWorkbook(wb, "STAR_ICMi.xlsx")
}
}
