## This program finds the P1 and P2 for a given denuncia 
## dataset: inspecciones.csv and rociado.csv 

#set directory
#setwd("~/queue-modeling/")    #for Mike
setwd("~/Desktop/Levy Lab/queue-modeling")   #for Patrick

inspecciones <- data.frame(read.csv("inspecciones - INSPECCIONES.csv"))
#inspecciones <- data.frame(read.csv("rociado - ROCIADO.csv"))
inspecciones <- inspecciones[, c("UNICODE.", "SITUACION","NRO_DENUNCIA","DIA", "MES", "ANIO", "IN_TCAP_TOT", "PD_TCAP_TOT", "INSP_COMPLETA")]

## Sum contents of IN_TCAP_TOT and PD_TCAP_TOT and INSP_COMPLETA and replace NA with zeros 
inspecciones$INSP_COMPLETA <- ifelse(is.na(inspecciones$INSP_COMPLETA), 0, inspecciones$INSP_COMPLETA)
inspecciones$IN_TCAP_TOT <- ifelse(is.na(inspecciones$IN_TCAP_TOT), 0, inspecciones$IN_TCAP_TOT)
inspecciones$PD_TCAP_TOT <- ifelse(is.na(inspecciones$PD_TCAP_TOT), 0, inspecciones$PD_TCAP_TOT)
inspecciones$sumTotal <- inspecciones$IN_TCAP_TOT + inspecciones$PD_TCAP_TOT
inspecciones$sumTotal <- ifelse(is.na(inspecciones$sumTotal), 0, inspecciones$sumTotal)

## replace $Situacion with only positive contents (sumtotal > 0)
indPos <- inspecciones[which(inspecciones$sumTotal != 0), ]

################################################
#  Choose Dataset 
#  indPos - P1 and P2 positive (TRUE)
#  inspecciones.date - P1 and P2 pos and non-pos (FALSE)
################################################

if (TRUE) {
  ## Take starting at 2012 when P1 and P2 was utilized
  indPos <- indPos[which(indPos$ANIO >= 2012),]
  inspecciones.date <- indPos[which(indPos$ANIO >= 2012),]
  
 } else {
  ## Subset larger dataset for when P1 and P2 notation was used
  indPos <- indPos[which(indPos$ANIO >= 2012),]
  inspecciones.date <- inspecciones[which(inspecciones$ANIO >= 2012),]
}

## Get positive denuncias where sumTotal != 0
denunNo <- indPos$NRO_DENUNCIA[indPos$SITUACION == "D"]

#############################################
          # countPValue # 
## dNo is an int, while pValue is a string 
## Returns number of P1s or P2s for given dNo
## TODO: take in  a data base as a function 
#############################################
countPValue <- function(dNo, pValue) {
  indDeNun <- inspecciones.date[which(inspecciones.date$NRO_DENUNCIA == dNo),]
  sum <- sum(indDeNun$SITUACION == pValue)
  sum1 <- 0
  sum2 <- 0
  
  # Account to for mispelling of P1 and P2 
  if (pValue == "P1") {
    sum1 <- sum1 + sum(indDeNun$SITUACION == "p1")
  }
  if (pValue == "P2") {
    sum1 <- sum1 + sum(indDeNun$SITUACION == "p2")
  }
  
  return(sum + sum1 + sum2)
}

##########################################
          # insp_completa  #
 ##all houses that had an inspection##
##########################################
insp_completa <- inspecciones.date$INSP_COMPLETA[inspecciones.date$NRO_DENUNCIA[inspecciones.date$SITUACION == "D"]]

## function to count inspCompleta for each postive denuncia 
countCompleta <- function(dNo, inspCompleta) {
  indDeNun <- inspecciones.date[which(inspecciones.date$NRO_DENUNCIA == dNo),]
  sum <- sum(indDeNun$INSP_COMPLETA == inspCompleta)
  return(sum)
}

##########################################
            # sumTotal #
    ##Total bugs from denuncia##
##########################################
countTotalBugs <- function(dNo) {
  indDeNun <- inspecciones.date[which(inspecciones.date$NRO_DENUNCIA == dNo),]
  denun <- indDeNun[which(indDeNun$SITUACION == "D"),]
  sum <- sum(denun$sumTotal)
  return(sum)
}

##########################################
      # Total Sum - countAllBugs #
 # Max between D P1 P2 - countMaxBugs #
##########################################
countAllBugs <- function(dNo) {
  indDeNun <- inspecciones.date[which(inspecciones.date$NRO_DENUNCIA == dNo),]
  sum <- sum(indDeNun$sumTotal)
  return(sum)
}

countMaxBugs <- function(dNo) {
  indDeNun <- inspecciones.date[which(inspecciones.date$NRO_DENUNCIA == dNo),]
  indDeNunMax <- which.max(indDeNun$sumTotal)
  max <- indDeNun$sumTotal[indDeNunMax]
  return(max)
}

##########################################
          # Date of Dununcia # 
##########################################
denunciaDate <- function(dNo, type) {
  indDeNun <- inspecciones.date[which(inspecciones.date$NRO_DENUNCIA == dNo),]
  denun <- indDeNun[which(indDeNun$SITUACION == "D"),]
  if (type == "DIA") {
    DIA <- denun$DIA[[1]]
    return(DIA)
  }
  if (type == "MES") {
    MES <- denun$MES[[1]]
    return(MES)
  }
  if (type == "ANIO") {
    ANIO <- denun$ANIO[[1]]
    return(ANIO)
  }
}

# loop through count function to create table of P1 and P2 per denunNo including inspCompleta
valTable <- c()
for (i in denunNo) { 
  valTable <- rbind(valTable, c(i, countPValue(i, "P1"), countPValue(i, "P2"), 
                                countMaxBugs(i), denunciaDate(i, "DIA"),
                                denunciaDate(i, "MES"), denunciaDate(i, "ANIO"), countCompleta(i, "INSP_COMPLETA")))
}

colnames(valTable) <- c("NRO_DENUNCIA", "P1", "P2", "Total Bug Count", "DIA",
                        "MES", "ANIO", "INSPCOMPLETA")
valTable.df <- data.frame(valTable)
print(valTable.df)

hist(as.numeric(valTable.df$P1))
hist(as.numeric(valTable.df$P2))
