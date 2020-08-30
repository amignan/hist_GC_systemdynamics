#Copyright (c) 2020 Arnaud Mignan

#  Permission is hereby granted, free of charge, to any person obtaining a copy
#of this software and associated documentation files (the "Software"), to deal
#in the Software without restriction, including without limitation the rights
#to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#copies of the Software, and to permit persons to whom the Software is
#furnished to do so, subject to the following conditions:

#  The above copyright notice and this permission notice shall be included in all
#copies or substantial portions of the Software.

#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#SOFTWARE.
#--------------------------------------------------------------------------------

# This is a translation of the World2 model from DYNAMO to R, based on the source
#codes given in Forrester (1971), chapter 3 and appendix B.
# Original ref.: Forrester JW (1971), World Dynamics. Wright-Allen Press, Inc., 
#Cambridge, 144 pp.
# The World2 model can be considered the first computer-based doomsday model. It is
#a 5th-order differential equation model with population, natural resource, capital 
#investment, capital-investment-in-agriculture fraction, and pollution as main 
#variables. 

rm(list=ls())


# LIBRARIES
library(signal)	 # interp1()

# FUNCTIONS
# logical function used as time switch to change parameter value
CLIP <- function(FUNCT1, FUNCT2, THRESH.K, VAL) if(THRESH.K >= VAL) return(FUNCT1) else return(FUNCT2)


DT <- .2
TIME <- seq(1900, 2100, DT)   # CALENDAR TIME (YEARS)
n <- length(TIME)

## (1) Population P ##
# P.K = P.J + DT (BR.JK - DR.JK)
# BR = BIRTH RATE (PEOPLE/YEAR)
# DR = DEATH RATE (PEOPLE/YEAR)
# PI = POPULATION INITIAL (PEOPLE)
P <- numeric(n)     # POPULATION (PEOPLE)
PI <- 1.65e9        # in YEAR = 1900

## (2) Birth rate BR ##
# BR.KL = P.K CLIP(BRN, BRN1, SWT1, TIME.K) BRFM.K BRMM.K BRCM.K BRPM.K
# BRFM = BIRTH-RATE-FROM-FOOD MULTIPLIER ()
# BRMM = BIRTH-RATE-FROM-MATERIAL MULTIPLIER ()
# BRCM = BIRTH-RATE-FROM-CROWDING MULTIPLIER ()
# BRPM = BIRTH-RATE-FROM-POLLUTION MULTIPLIER ()
BR <- numeric(n)    # BIRTH RATE (PEOPLE/YEAR)
BRN <- .04          # BIRTH RATE NORMAL (FRACTION/YEAR)
BRN1 <- .04         # BIRTH RATE NORMAL no. 1 (FRACTION/YEAR)
SWT1 <- 1970        # SWITCH TIME no. 1 FOR BRN (YEARS)

## (3) Birth-rate-from-material multiplier BRMM ##
BRMM = function(MSL.K){
  # MSL   = MATERIAL STANDARD OF LIVING ()
  # BRMMT = BIRTH-RATE-FROM-MATERIAL MULTIPLIER TABLE
  lookup.table <- data.frame(MSL = seq(0, 5, 1), 
                             BRMMT = c(1.2, 1., .85, .75, .7, .7))
  return(interp1(lookup.table$MSL, lookup.table$BRMMT, MSL.K))
}

## (4) Material standard of living MSL ##
# MSL.K = ECIR.K / ECIRN
# ECIR = EFFECTIVE-CAPITAL-INVESTMENT RATIO (CAPITAL UNITS/PERSON)
# ECIRN = EFFECTIVE-CAPITAL-INVESTMENT RATIO NORMAL (CAPITAL UNITS/PERSON)
MSL <- numeric(n)   # MATERIAL STANDARD OF LIVING
ECIRN <- 1

## (5) Effective-Capital-Investment Ratio ECIR ##
# ECIR.K = CIR.K (1 - CIAF.K) NREM.K / (1 - CIAFN)
# CIR   = CAPITAL-INVESTMENT RATIO (CAPITAL UNITS/PERSON)
# CIAF  = CAPITAL-INVESTMENT-IN-AGRICULTURE FRACTION ()
# CIAFN = CAPITAL-INVESTMENT-IN-AGRICULTURE FRACTION NORMAL ()
# NREM  = NATURAL-RESOURCE-EXTRACTION MULTIPLIER ()
ECIR <- numeric(n)  # EFFECTIVE-CAPITAL-INVESTMENT RATIO

## (6) Natural-Resource-Extraction Multiplier NREM ##
NREM = function(NRFR.K){
  # NRFR  = NATURAL-RESOURCE FRACTION REMAINING ()
  # NREMT = NATURAL-RESOURCE-EXTRACTION-MULTIPLIER TABLE
  lookup.table <- data.frame(NRFRT = seq(0,1,.25), 
                             NREMT = c(0, .15, .5, .85, 1))
  return(interp1(lookup.table$NRFRT, lookup.table$NREMT, NRFR.K))
}

## (7) Natural-Resource Fraction Remaining NRFR ##
# NRFR.K = NR.K / NRI
# NR  = NATURAL RESOURCES (NATURAL RESOURCE UNITS)
# NRI = NATURAL RESOURCES INITIAL (NATURAL RESOURCE UNITS)
NRFR <- numeric(n)  # NATURAL RESOURCES FRACTION REMAINING
NRI <- 900e9

## (8) Natural Resource NR ##
# NR.K = NR.J + DT (-NRUR.JK)
# NRUR = NATURAL-RESOURCE-USAGE RATE (NATURAL RESOURCE UNITS/YEAR)
NR <- numeric(n)    # NATURAL RESOURCES

## (9) Natural-Resource-Usage Rate NRUR ##
# NRUR.KL = P.K CLIP(NRUN, NRUN1, SWT2, TIME.K) NRMM.K
# NRUN  = NATURAL-RESOURCE USAGE NORMAL (NATURAL RESOURCE UNITS/PERSON/YEAR)
# NRUN1 = NATURAL-RESOURCE USAGE NORMAL no. 1 (NATURAL RESOURCE UNITS/PERSON/YEAR)
# SWT2  = SWITCH TIME no. 2 FOR NRUN (YEARS)
# NRMM  = NATURAL-RESOURCE-FROM-MATERIAL MULTIPLIER ()
NRUR <- numeric(n)  # NATURAL-RESOURCE USAGE RATE
NRUN <- 1
NRUN1 <- 1
SWT2 <- 1970

## (10) DEATH RATE DR ##
# DR.KL = P.K CLIP(DRN, DRN1, SWT3, TIME.K) DRMM.K DRPM.K DRFM.K DRCM.K
# DRN  = DEATH RATE NORMAL (FRACTION/YEAR)
# DRN1 = DEATH RATE NORMAL no. 1 (FRACTION/YEAR)
# SWT3 = SWITCH TIME no. 3 FOR DRN (YEARS)
# DRMM = DEATH-RATE-FROM-MATERIAL MULTIPLIER ()
# DRPM = DEATH-RATE-FROM-POLLUTION MULTIPLIER ()
# DRFM = DEATH-RATE-FROM-FOOD MULTIPLIER ()
# DRCM = DEATH-RATE-FROM-CROWDING MULTIPLIER ()
DR <- numeric(n)    # DEATH RATE (PEOPLE/YEAR)
DRN <- .028
DRN1 <- .028
SWT3 <- 1970

## (11) Death-Rate-from-Material Multiplier DRMM ##
DRMM = function(MSL.K){
  # DRMMT = DEATH-RATE-FROM-MATERIAL MULTIPLIER TABLE
  lookup.table <- data.frame(MSL = seq(0, 5, .5), 
                             DRMMT = c(3, 1.8, 1, .8, .7, .6, .53, .5, .5, .5, .5))
  return(interp1(lookup.table$MSL, lookup.table$DRMMT, MSL.K))
}

## (12) Death-Rate-from-Pollution Multiplier DRPM ##
DRPM = function(POLR.K){
  # POLR  = POLLUTION RATIO ()
  # DRPMT = DEATH-RATE-FROM-POLLUTION MULTIPLIER TABLE
  lookup.table <- data.frame(POLR = seq(0, 60, 10), 
                             DRPMT = c(.92, 1.3, 2, 3.2, 4.8, 6.8, 9.2))
  return(interp1(lookup.table$POLR, lookup.table$DRPMT, POLR.K))
}

## (13) Death-Rate-from-Food Multiplier DRFM ##
DRFM = function(FR.K){
  # FR  = FOOD RATIO ()
  # DRFMT = DEATH-RATE-FROM-FOOD MULTIPLIER TABLE
  lookup.table <- data.frame(FR = seq(0, 2, .25), 
                             DRFMT = c(30, 3, 2, 1.4, 1, .7, .6, .5, .5))
  return(interp1(lookup.table$FR, lookup.table$DRFMT, FR.K))
}

## (14) Death-Rate-from-Crowding Multiplier DRCM ##
DRCM = function(CR.K){
  # CR  = CROWDING RATIO ()
  # DRCMT = DEATH-RATE-FROM-CROWDING MULTIPLIER TABLE
  lookup.table <- data.frame(CR = seq(0, 5, 1), 
                             DRCMT = c(.9, 1, 1.2, 1.5, 1.9, 3))
  return(interp1(lookup.table$CR, lookup.table$DRCMT, CR.K))
}

## (15) Crowding Ratio CR ##
# CR.K = P.K / (LA * PDN)
# LA  = LAND AREA (SQUARE KILOMETERS)
# PDN = POPULATION DENSITY NORMAL (PEOPLE/SQUARE KILOMETER)
CR <- numeric(n)    # CROWDING RATIO
LA <- 135e6
PDN <- 26.5

## (16) Birth-Rate-from_Crowding Multiplier BRCM ##
BRCM = function(CR.K){
  # BRCMT = BIRTH-RATE-FROM-CROWDING MULTIPLIER TABLE
  lookup.table <- data.frame(CR = seq(0, 5, 1), 
                             BRCMT = c(1.05, 1, .9, .7, .6, .55))
  return(interp1(lookup.table$CR, lookup.table$BRCMT, CR.K))
}

## (17) Birth-Rate-from-Food Multiplier BRFM ##
BRFM = function(FR.K){
  # BRFMT = BIRTH-RATE-FROM-FOOD MULTIPLIER TABLE
  lookup.table <- data.frame(FR = seq(0, 4, 1), 
                             BRFMT = c(0, 1, 1.6, 1.9, 2))
  return(interp1(lookup.table$FR, lookup.table$BRFMT, FR.K))
}

## (18) Birth-Rate-from-Pollution Multiplier BRPM ##
BRPM = function(POLR.K){
  # BRPMT = BIRTH-RATE-FROM-POLLUTION MULTIPLIER TABLE
  lookup.table <- data.frame(POLR = seq(0, 60, 10), 
                             BRPMT = c(1.02, .9, .7, .4, .25, .15, .1))
  return(interp1(lookup.table$POLR, lookup.table$BRPMT, POLR.K))
}

## (19) Food Ratio FR ##
# FR.K = FPCI.K FCM.K FPM.K CLIP(FC, FC1, SWT7, TIME.K) / FN
# FPCI = FOOD POTENTIAL FROM CAPITAL INVESTMENT (FOOD UNITS/PERSON/YEAR)
# FCM  = FOOD-FROM-CROWDING MULTIPLIER ()
# FPM  = FOOD-FROM-POLLUTION MULTIPLIER ()
# FC   = FOOD COEFFICIENT ()
# FC1  = FOOD COEFFICIENT no. 1 ()
# SWT7 = SWITCH TIME no. 7 FOR FC (YEARS)
# FN   = FOOD NORMAL (FOOD UNITS/PERSON/YEAR)
FR <- numeric(n)    # FOOD RATIO
FC <- 1
FC1 <- 1
FN <- 1
SWT7 <- 1970

## (20) Food-from-Crowding Multiplier FCM ##
FCM = function(CR.K){
  # FCMT = FOOD-FROM-CROWDING MULTIPLIER TABLE
  # CR   = CROWDING RATIO ()
  lookup.table <- data.frame(CR = seq(0, 5, 1), 
                             FCMT = c(2.4, 1, .6, .4, .3, .2))
  return(interp1(lookup.table$CR, lookup.table$FCMT, CR.K))
}

## (21) Food from Capital Investment FPCI ##
FPCI = function(CIRA.K){
  # FPCIT = FOOD-FROM-CAPITAL-INVESTMENT TABLE
  # CIRA  = CAPITAL-INVESTMENT RATIO IN AGRICULTURE (CAPITAL UNITS/PERSON)
  lookup.table <- data.frame(CIRA = seq(0, 6, 1), 
                             FPCIT = c(.5, 1., 1.4, 1.7, 1.9, 2.05, 2.2))
  return(interp1(lookup.table$CIRA, lookup.table$FPCIT, CIRA.K))
}

## (22) Capital-Investment Ratio in Agriculture CIRA ##
# CIRA.K = CIR.K CIAF.K / CIAFN
# CIR   = CAPITAL-INVESTMENT RATIO (CAPITAL UNITS/PERSON)
# CIAF  = CAPITAL-INVESTMENT-in-AGRICULTURE FRACTION ()
# CIAFN = CAPITAL-INVESTMENT-in-AGRICULTURE FRACTION NORMAL ()
CIRA <- numeric(n)  # CAPITAL-INVESTMENT RATIO IN AGRICULTURE
CIAFN <- .3

## (23) Capital-Investment Ratio CIR ##
# CIR.K = CI.K / P.K
# CI = CAPITAL-INVESTMENT (CAPITAL UNITS)
CIR <- numeric(n)   # CAPITAL-INVESTMENT RATIO

## (24) Capital Investment CI ##
# CI.K = CI.J + DT (CIG.JK - CID.JK)
# CIG = CAPITAL-INVESTMENT GENERATION (CAPITAL UNITS/YEAR)
# CID = CAPITAL-INVESTMENT DISCARD (CAPITAL UNITS/YEAR)
# CII = CAPITAL-INVESTMENT INITIAL (CAPITAL UNITS)
CI <- numeric(n)    # CAPITAL-INVESTMENT
CII <- .4e9

## (25) Capital-Investment Generation CIG ##
# CIG.K = P.K CIM.K CLIP(CIGN, CIGN1, SWT4, TIME.K)
# CIM   = CAPITAL-INVESTMENT MULTIPLIER ()
# CIGN  = CAPITAL-INVESTMENT GENERATION NORMAL (CAPITAL UNITS/PERSON/YEAR)
# CIGN1 = CAPITAL-INVESTMENT GENERATION NORMAL no. 1 (CAPITAL UNITS/PERSON/YEAR)
# SWT4  = SWITCH TIME no. 4 FOR CIGN (YEARS)
CIG <- numeric(n)   # CAPITAL-INVESTMENT GENERATION
CIGN1 <- .05
CIGN <- .05
SWT4 <- 1970

## (26) Capital-Investment Multiplier CIM ##
CIM = function(MSL.K){
  # CIMT = CAPITAL-INVESTMENT-MULTIPLIER TABLE
  lookup.table <- data.frame(MSL = seq(0, 5, 1), 
                             CIMT = c(.1, 1, 1.8, 2.4, 2.8, 3))
  return(interp1(lookup.table$MSL, lookup.table$CIMT, MSL.K))
}

## (27) Capital-Investment Discard CID ##
# CID.KL = CI.K CLIP(CIDN, CIDN1, SWTS, TIME.K)
# CIDN  = CAPITAL-INVESTMENT DISCARD NORMAL (FRACTION/YEAR)
# CIDN1 = CAPITAL-INVESTMENT DISCARD NORMAL no. 1 (FRACTION/YEAR)
# SWT5  = SWITCH TIME no. 5 FOR CIDN (YEARS)
CID <- numeric(n)   # CAPITAL-INVESTMENT DISCARD
CIDN1 <- .025
CIDN <- .025
SWT5 <- 1970

## (28) Food-from-Pollution Multiplier FPM ##
FPM = function(POLR.K){
  # FPMT = FOOD-FROM-POLLUTION-MULTIPLIER TABLE
  lookup.table <- data.frame(POLR = seq(0, 60, 10), 
                             FPMT = c(1.02, .9, .65, .35, .2, .1, .05))
  return(interp1(lookup.table$POLR, lookup.table$FPMT, POLR.K))
}

## (29) Pollution Ratio POLR ##
# POLR.K = POL.K / POLS
# POL  = POLLUTION (POLLUTION UNITS)
# POLS = POLLUTION STANDARD (POLLUTION UNITS)
POLR <- numeric(n)  # POLLUTION RATIO
POLS <- 3.6e9

## (30) Pollution POL ##
# POL.K = POL.J + DT (POLG.JK - POLA.JK)
# POLG = POLLUTION GENERATION (POLLUTION UNITS/YEAR)
# POLA = POLLUTION ABSORPTION (POLLUTION UNITS/YEAR)
# POLI = POLLUTION INITIAL (POLLUTION UNITS)
POL <- numeric(n)   # POLLUTION
POLI <- .2e9

## (31) Pollution Generation POLG ##
# POLG.KL = P.K CLIP(POLN, POLN1, SWT6, TIME.K) POLCM.K
# POLN  = POLLUTION NORMAL (POLLUTION UNITS/PERSON/YEAR)
# POLN1 = POLLUTION NORMAL no. 1 (POLLUTION UNITS/PERSON/YEAR)
# SWT6  = SWITCH TIME no. 6 FOR POLN (YEARS)
# POLCM = POLLUTION-FROM-CAPITAL MULTIPLIER ()
POLG <- numeric(n)  # POLLUTION GENERATION
POLN1 <- 1
POLN <- 1
SWT6 <- 1970

## (32) Pollution-from-Capital Multiplier POLCM ##
POLCM = function(CIR.K){
  # POLCMT = POLLUTION-FROM-CAPITAL-MULTIPLIER TABLE
  lookup.table <- data.frame(CIR = seq(0, 5, 1), 
                             POLCMT = c(.05, 1, 3, 5.4, 7.4, 8))
  return(interp1(lookup.table$CIR, lookup.table$POLCMT, CIR.K))
}

## (33) Pollution Absorption POLA ##
# POLA.KL = POL.K / POLAT.K
# POLA  = POLLUTION ABSORPTION (POLLUTION UNITS/YEAR)
# POLAT - POLLUTION-ABSORPTION TIME (YEARS)
POLA <- numeric(n)  # POLLUTION ABSORPTION

## (34) Pollution-Absorption Time POLAT
POLAT = function(POLR.K){
  # POLATT = POLLUTION-ABSORPTION-TIME TABLE
  lookup.table <- data.frame(POLR = seq(0, 60, 10), 
                             POLATT = c(.6, 2.5, 5, 8, 11.5, 15.5, 20))
  return(interp1(lookup.table$POLR, lookup.table$POLATT, POLR.K))
}

## (35) Capital-Investment-in-Agriculture Fraction CIAF ##
# CIAF.K = CIAF.J + (DT / CIAFT) (CFIFR.J * CIQR.J - CIAF.J)
# CIAFT = CAPITAL-INVESTMENT-IN-AGRICULTURE-FRACTION ADJUSTMENT TIME (YEARS)
# CIAFI = CAPITAL-INVESTMENT-IN-AGRICULTURE-FRACTION INITIAL ()
# CFIFR = CAPITAL FRACTION INDICATED BY FOOD RATIO ()
# CIQR  = CAPITAL-INVESTMENT-FROM-QUALITY RATIO ()
CIAF <- numeric(n)  # CAPITAL-INVESTMENT-IN-AGRICULTURE FRACTION
CIAFI <- .2
CIAFT <- 15

## (36) Capital Fraction Indicated by Food Ratio CFIFR ##
CFIFR = function(FR.K){
  # POLATT = POLLUTION-ABSORPTION-TIME TABLE
  lookup.table <- data.frame(FR = seq(0, 2, .5), 
                             CIFRT = c(1, .6, .3, .15, .1))
  return(interp1(lookup.table$FR, lookup.table$CIFRT, FR.K))
}

## (37) Quality of Life QL ##
# QL.K = QLS QLM.K QLC.K QLF.K QLP.K
# QL  = QUALITY OF LIFE (SATISFACTION UNITS)
# QLS = QUALITY-OF-LIFE STANDARD (SATISFACTION UNITS)
# QLM = QUALITY OF LIFE FROM MATERIAL ()
# QLC = QUALITY OF LIFE FROM CROWDING ()
# QLF = QUALITY OF LIFE FROM FOOD ()
# QLP = QUALITY OF LIFE FROM POLLUTION ()
QL <- numeric(n)    # QUALITY OF LIFE
QLS <- 1

## (38) Quality of Life from Material QLM ##
QLM = function(MSL.K){
  # QLMT = QUALITY-OF-LIFE-FROM-MATERIAL TABLE
  lookup.table <- data.frame(MSL = seq(0, 5, 1), 
                             QLMT = c(.2, 1, 1.7, 2.3, 2.7, 2.9))
  return(interp1(lookup.table$MSL, lookup.table$QLMT, MSL.K))
}

## (39) Quality of Life from Crowding QLC ##
QLC = function(CR.K){
  # QLCT = QUALITY-OF-LIFE-FROM-CROWDING TABLE
  lookup.table <- data.frame(CR = seq(0, 5, .5), 
                             QLCT = c(2, 1.3, 1, .75, .55, .45, .38, .3, .25, .22, .2))
  return(interp1(lookup.table$CR, lookup.table$QLCT, CR.K))
}

## (40) Quality of Life from Food QLF ##
QLF = function(FR.K){
  # QLFT = QUALITY-OF-LIFE-FROM-FOOD TABLE
  lookup.table <- data.frame(FR = seq(0, 4, 1), 
                             QLFT = c(0, 1, 1.8, 2.4, 2.7))
  return(interp1(lookup.table$FR, lookup.table$QLFT, FR.K))
}

## (41) Quality of Life from Pollution QLP ##
QLP = function(POLR.K){
  # QLPT = QUALITY-OF-LIFE-FROM-POLLUTION TABLE
  lookup.table <- data.frame(POLR = seq(0, 60, 10), 
                             QLPT = c(1.04, .85, .6, .3, .15, .05, .02))
  return(interp1(lookup.table$POLR, lookup.table$QLPT, POLR.K))
}

## (42) Natural-Resource-from-Material Multiplier NRMM ##
NRMM = function(MSL.K){
  # NRMMT = NATURAL-RESOURCE-FROM-MATERIAL-MULTIPLIER TABLE
  lookup.table <- data.frame(MSL = seq(0, 10, 1), 
                             NRMMT = c(0, 1, 1.8, 2.4, 2.9, 3.3, 3.6, 3.8, 3.9, 3.95, 4))
  return(interp1(lookup.table$MSL, lookup.table$NRMMT, MSL.K))
}

## (43) Capital-Investment-from-Quality Ratio CIQR ##
CIQR = function(QLM_QFL.K){
  # CIQRT = CAPITAL-INVESTMENT-FROM-QUALITY-RATIO TABLE
  # QLM   = QUALITY OF LIFE FROM MATERIAL ()
  # QFL   = QUALITY OF LIFE FROM FOOD ()
  lookup.table <- data.frame(QLM_QFL = seq(0, 2, .5), 
                             CIQRT = c(.7, .8, 1, 1.5, 2))
  return(interp1(lookup.table$QLM_QFL, lookup.table$CIQRT, QLM_QFL.K))
}


P[1] <- PI
NR[1] <- NRI
CI[1] <- CII
POL[1] <- POLI
CIAF[1] <- CIAFI

CIR[1] <- CI[1] / P[1]
POLG[1] <- P[1] * CLIP(POLN, POLN1, SWT6, TIME[1]) * POLCM(CIR[1])
POLR[1] <- POL[1] / POLS
POLA[1] <- POL[1] / POLAT(POLR[1])
CR[1] <- P[1] / (LA * PDN)
NRFR[1] <- NR[1] / NRI
ECIR[1] <- CIR[1] * (1 - CIAF[1]) * NREM(NRFR[1]) / (1 - CIAFN)
MSL[1] <- ECIR[1] / ECIRN
CIRA[1] <- CIR[1] * CIAF[1] / CIAFN
FR[1] <- FPCI(CIRA[1]) * FCM(CR[1]) * FPM(POLR[1]) * CLIP(FC, FC1, SWT7, TIME[1]) / FN

CID[1] <- NA
CIG[1] <- NA
BR[1] <- NA
DR[1] <- NA
QL[1] <- NA

for(K in 2:n){
  J <- K - 1

  BR[K] <- P[J] * CLIP(BRN, BRN1, SWT1, TIME[J]) * BRMM(MSL[J]) * BRCM(CR[J]) * BRFM(FR[J]) * BRPM(POLR[J])  # (2-3,16-18)
  DR[K] <- P[J] * CLIP(DRN, DRN1, SWT3, TIME[J]) * DRMM(MSL[J]) * DRPM(POLR[J]) * DRFM(FR[J]) * DRCM(CR[J])  # (10-14)
  P[K] <- P[J] + DT * (BR[K] - DR[K])    # (1)
  
  NRUR[K] <- P[J] * CLIP(NRUN, NRUN1, SWT2, TIME[J]) * NRMM(MSL[J])   # (9,42)
  NR[K] = NR[J] + DT * (-NRUR[K])   # (8)
  NRFR[K] <- NR[K] / NRI     # (7)
  
  POLG[K] <- P[J] * CLIP(POLN, POLN1, SWT6, TIME[J]) * POLCM(CIR[J])  # (31-32)
  POLA[K] <- POL[J] / POLAT(POLR[J])   # (33-34)
  POL[K] <- POL[J] + DT * (POLG[K] - POLA[K])   # (30)
  POLR[K] <- POL[K] / POLS   # (29)
  
  CIAF[K] <- CIAF[J] + (DT / CIAFT) * (CFIFR(FR[J]) * CIQR(QLM(MSL[J]) / QLF(FR[J])) - CIAF[J])  # (35-36,43)

  CID[K] <- CI[J] * CLIP(CIDN, CIDN1, SWT5, TIME[J])   # (27)
  CIG[K] <- P[J] * CIM(MSL[J]) * CLIP(CIGN, CIGN1, SWT4, TIME[J])  # (25-26)
  CI[K] <-  CI[J] + DT * (CIG[K] - CID[K])    # (24)
  CR[K] <- P[K] / (LA * PDN)  # (15)
  CIR[K] <- CI[K] / P[K]    # (23)
  
  CIRA[K] <- CIR[K] * CIAF[K] / CIAFN   # (22)
  FR[K] <- FCM(CR[K]) * FPCI(CIRA[K]) * FPM(POLR[K]) * CLIP(FC, FC1, SWT7, TIME[K]) / FN   # (19-21, 28)
  
  ECIR[K] <- CIR[K] * (1 - CIAF[K]) * NREM(NRFR[K]) / (1 - CIAFN)  # (5,6)
  MSL[K] <- ECIR[K] / ECIRN   # (4)
  
  QL[K] <- QLS * QLM(MSL[K]) * QLC(CR[K]) * QLF(FR[K]) * QLP(POLR[K])   # (37-41)
}


# basic plots with same y-range as in Fig. 4-1 of Forrester (1971)
plot(TIME, P, type = 'l', ylim = c(0, 8e9))
plot(TIME, POLR, type = 'l', ylim = c(0, 40))
plot(TIME, CI, type = 'l', ylim = c(0, 20e9))
plot(TIME, QL, type = 'l', ylim = c(0, 2))
plot(TIME, NR, type = 'l', ylim = c(0, 1000e9))










