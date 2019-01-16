#' data.prep
#'
#' Questa funzione consente la preparazione del dataset per il calcolo degli indici STAR_ICMi e ISA.
#' @param data.frame data.frame di input importato come specificato nei dettagli.
#' @details
#' Il dataset deve essere importato assicurandosi che nella funzione read.table il comando check.names sia posto uguale a F. La funzione data.prep consente di preparare
#' dati in cui il nome di uno o pi? taxa sia ripetuto. Nel caso non si utilizzai read.table per l'importazione ? necessario assicurarsi che il taxa ripetuto abbia lo stesso nome
#' nel data.frame importato (ad esempio "BAETIDAE", "BAETIDAE" e non "BAETIDAE.1", "BAETIDAE.2").\cr
#' La funzione data.prep fornisce una lista di valori diversa a seconda che si voglia calcolare l'idnice STAR_ICMi o l'indice ISA. Nel primo caso la funzione restituisce
#' un oggetto di classe STAR e nel secondo un'oggetto di classe ISA
#' @keywords preparazione dei dati
#' @export
#' @examples
#' data.prep()

data.prep <- function (x) 
{
    if (is.vector(x) == TRUE) {
        stop("Dataset con n? di colonne uguale a 1 non permessi")
    }
    a <- select.list(c("1. Rete Surber", "2. Substrati artificiali"), 
        title = "TIPO DI CAMPIONAMENTO")
    colnames(x) <- toupper(colnames(x))
    x_t <- t(x)
    t <- sapply(by(x_t, rownames(x_t), colSums), identity)
    if (nrow(x) == 1) {
        t <- as.data.frame(t(as.matrix(t)))
    }
    t_1 <- colnames(t)
    if (a == "2. Substrati artificiali") {
        data.mts <- as.character(mts_t[, 2])
        mts_d <- match(t_1, data.mts)
        if (length(which(is.na(mts_d))) == ncol(x)) {
            b <- select.list(c("1. Procedere", "2. Non Procedere"), 
                title = "Nessuna Unità Operazionale!")
            if (b == "2. Non Procedere") {
                stop("annullato dall'Utente")
            }
            if (b == "1. Procedere") {
                data.names <- rownames(aspt_v)
                r1 <- t_1[is.na(match(t_1, data.names))]
                r <- list(sort(r1), sinonimi)
                names(r) <- cbind("presenti solo nei dati", "CONTROLLA SINONIMI!!!")
                if (length(r1) == 0) {
                  r_in <- which(rownames(aspt_v) %in% colnames(t))
                  aspt_v1 <- aspt_v[r_in, ]
                  data_ready <- list(t, aspt_v1, 0, 0)
                  class(data_ready) <- "ISA"
                  print("DATI OK")
                  return(data_ready)
                }
                else {
                  print(r)
                }
            }
        }
        if (length(which(is.na(mts_d))) != ncol(x)) {
            mts_names <- t_1[!is.na(mts_d)]
            mts_taxa <- t[, !is.na(mts_d), drop = FALSE]
            com_ou <- intersect(mts_t[, 2], colnames(mts_taxa))
            com_ou2 <- mts_t[mts_t[, 2] %in% com_ou, , drop = F]
            com_ou2 <- com_ou2[order(com_ou2[, 2]), ]
            mts_taxa_t <- t(mts_taxa)
            mts_ept <- data.frame(com_ou2[, 1], mts_taxa_t)
            if (nrow(x) == 1) {
                mts_ept_1 <- aggregate(. ~ mts_ept[, 1], data = mts_ept, 
                  sum)
                mts_ept_1 <- mts_ept_1[, -2]
                rownames(mts_ept_1) <- mts_ept_1[, 1]
                mts_ept_2 <- mts_ept_1[, -1]
                names(mts_ept_2) <- rownames(mts_ept_1)
            }
            if (nrow(x) > 1) {
                mts_ept_1 <- aggregate(. ~ mts_ept[, 1], data = mts_ept[, 
                  -1], sum)
                rownames(mts_ept_1) <- mts_ept_1[, 1]
                mts_ept_2 <- mts_ept_1[, -1]
            }
            t_1 <- c(t_1[is.na(mts_d)], rownames(mts_ept_1))
            data.names <- rownames(aspt_v)
            r1 <- t_1[is.na(match(t_1, data.names))]
            r <- list(sort(r1), sinonimi)
            names(r) <- cbind("presenti solo nei dati", "CONTROLLA SINONIMI!!!")
            if (length(r1) == 0){
                t <- t[, is.na(mts_d)]
                t <- data.frame(t, t(mts_ept_2))
                t_t <- t(t)
                t_t <- t_t[order(rownames(t_t)), ]
                t <- t(t_t)
                r_in <- which(rownames(aspt_v) %in% colnames(t))
                aspt_v1 <- aspt_v[r_in, ]
                mts_ept <- data.frame(com_ou2[, 3], mts_ept[, 
                  -1])
                colnames(mts_ept)[1] <- "punteggio"
                if (nrow(x) == 1) {
                  rownames(mts_ept) <- names(mts_taxa)
                }
                data_ready <- list(t, aspt_v1, mts_taxa, mts_ept)
                class(data_ready) <- "ISA"
                print("DATI OK")
                return(data_ready)
            }        
            else {
              print(r)
            }

        }

    }
    if (a == "1. Rete Surber") {
        data.names <- rownames(aspt_v)
        r1 <- t_1[is.na(match(t_1, data.names))]
        r <- list(sort(r1), sinonimi)
        names(r) <- cbind("presenti solo nei dati", "CONTROLLA SINONIMI!!!")
        if (length(r1) == 0) {
            r_in <- which(rownames(aspt_v) %in% colnames(t))
            aspt_v1 <- aspt_v[r_in, ]
            data_ready <- list(t, aspt_v1)
            class(data_ready) <- "STAR"
            print("DATI OK")
            return(data_ready)
        }
        else {
            print(r)
        }
    }
}
