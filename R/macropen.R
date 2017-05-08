#' macropen
#'
#' This function calculates STAR_ICMi index (surber) or ISA index (artificial substrates). Questa funzione consente il calcolo dell'indice STAR_ICMi nel caso di campionamento con rete surber o dell'indice ISA nel caso di cammpionamento con substrati artificiali.
#' @param data.frame a data.prep object.
#' @param rif reference values provided by the user. If a drop down menu will be provided
#' @param writexlsx se T salva nella cartella di destinazione i risultati della funzione
#' @param MTS valori possibili 1 e 2. Vedi Detail per approfondimenti
#' @details I valori del comando MTS sono relativi alla correzione da applicare alla metrica ASPT qualora si utilizzino i substrati artificiali. MTS =1 fornisce i valori proposti in Buffagni et al. (2014) pari a -0.1067*n° famiglie+7.3533. MTS = 2 implementa invece i valori presenti nel software MacrOper versione 0.0.1 pari a -0.10667*n? famiglie+0.9909
#' @keywords STAR_ICMi, ISA
#' @export
#' @examples
#' macropen()


macropen <- function (x, rif = NULL, writexlsx = F, MTS=1)
{
    if(class(x)=="ISA"){
    if (missing(rif)) {
        rif1 <- scelta_mts()
    }
    else {
        if (length(rif) == 12) {
            rif1 = t(as.data.frame(rif))
        }
        else {
            stop("C'E' QUALCOSA CHE NON VA")
        }
    }
    }

    if(class(x)=="STAR"){
    if (missing(rif)) {
        rif1 <- scelta()
    }
    else {
        if (length(rif) == 11) {
            rif1 = t(as.data.frame(rif))
        }
        else {
            stop("C'E' QUALCOSA CHE NON VA")
        }
    }
    }
    nfam_s <- nfam(x)
    aspt_s <- aspt(x)
    if(MTS==1){
    aspt_s_c <- aspt_s-(-0.1067*nfam_s+7.3533)+rif1[, 1]}
    if(MTS==2){
    aspt_s_c <- aspt_s-(-0.10667*nfam_s+0.9909)}
    ept_s <- ept(x)
    gold_s <- gold(x)
    shannon_s <- shannon(x)
    eptd_s <- EPTD(x)
    if(class(x)=="ISA"){
    	mts_s <- mts(x)
    }
    if(class(x)=="ISA"){
    aspt_s1 <- (aspt_s_c-2)/(rif1[, 1]-2)
    nfam_s1 <- nfam_s/rif1[, 2]
    ept_s1 <- ept_s/rif1[, 3]
    gold_s1 <- gold_s/rif1[, 4]
    shannon_s1 <- shannon_s/rif1[, 5]
    eptd_s1 <- eptd_s/rif1[, 6]
    mts_s1 <- mts_s/rif1[, 8]
    star_icmi_r1 <- (0.334 * aspt_s1 + 0.167 * nfam_s1 +
        0.083 * ept_s1 + 0.067 * gold_s1 + 0.083 * shannon_s1 +
        0.266 * eptd_s1)/rif1[, 7]
    isa <-  star_icmi_r1*0.6+mts_s1*0.4
    star_icmi_r1 <- round(star_icmi_r1,3)
    isa <- round(isa,3)
    star_icmi_r <- data.frame(aspt_s, nfam_s, ept_s, gold_s,
        shannon_s, eptd_s, star_icmi_r1,mts_s,isa)
    rownames(star_icmi_r) <- c(rownames(x[[1]]))
    star_icmi_r <- stato_eco_mts(star_icmi_r, rif1)
    names(star_icmi_r) <- c("ASPT", "N° Famiglie", "EPT", "1-GOLD",
        "Shannon", "EPTD", "STAR_ICMi", "MTS","ISA","STATO ECOLOGICO", "CLASSE")
    results <- list(rif1, star_icmi_r)
    if (writexlsx == T) {
        writexlsx_mts(results)
    }
}

    if(class(x)=="STAR"){
    aspt_s1 <- (aspt_s - 2)/(rif1[, 1] - 2)
    nfam_s1 <- nfam_s/rif1[, 2]
    ept_s1 <- ept_s/rif1[, 3]
    gold_s1 <- gold_s/rif1[, 4]
    shannon_s1 <- shannon_s/rif1[, 5]
    eptd_s1 <- eptd_s/rif1[, 6]
    star_icmi_r1 <- round(round((0.334 * aspt_s1 + 0.167 * nfam_s1 +
        0.083 * ept_s1 + 0.067 * gold_s1 + 0.083 * shannon_s1 +
        0.266 * eptd_s1),3)/rif1[, 7], 3)
    star_icmi_r <- data.frame(aspt_s, nfam_s, ept_s, gold_s,
        shannon_s, eptd_s, star_icmi_r1)
    rownames(star_icmi_r) <- rownames(x[[1]])
    star_icmi_r <- stato_eco(star_icmi_r, rif1)
    names(star_icmi_r) <- c("ASPT", "N° Famiglie", "EPT", "1-GOLD",
        "Shannon", "EPTD", "STAR_ICMi", "STATO ECOLOGICO", "CLASSE")
    results <- list(rif1, star_icmi_r)
    if (writexlsx == T) {
        writexlsx(results)
    }
    }
    return(results)
}


