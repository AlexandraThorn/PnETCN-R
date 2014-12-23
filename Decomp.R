Decomp <- function(share,clim,veg,rstep) {
#
# PnET-CN decomposition routine
#


#global veg site clim share rstep

# Add atmospheric N deposition
#print(paste("share$NH4 (1) =", share$NH4))
share$NO3 <- share$NO3 + clim$NO3dep[rstep]
share$NH4 <- share$NH4 + clim$NH4dep[rstep]

# Temperature effect on all soil processes
tEffSoil <- max(share$Tave, 1)
TMult <- (exp(0.1 * (share$Tave - 7.1)) * 0.68) * 1
WMult <- share$MeanSoilMoistEff

# Add litter to Humus pool
#print(paste("share$HON =", share$HON))
#print(paste("share$HOM =", share$HOM))
share$HOM <- share$HOM + share$TotalLitterM
share$HON <- share$HON + share$TotalLitterN

# Humus dynamics
KhoAct <- veg$Kho * (clim$dayspan[rstep] / 365)
DHO <- share$HOM * (1 - exp(-KhoAct * TMult * WMult))
#print(paste("DHO =", DHO))
GrossNMin <- DHO * (share$HON / share$HOM)
share$SoilDecResp <- DHO * veg$CFracBiomass
share$SoilDecRespYr <- share$SoilDecRespYr + share$SoilDecResp
share$GrossNMinYr <- share$GrossNMinYr + GrossNMin
share$HON <- share$HON - GrossNMin
share$HOM <- share$HOM - DHO

# Immobilization and net mineralization
SoilPctN <- (share$HON / share$HOM) * 100
NReten <- (veg$NImmobA + veg$NImmobB * SoilPctN) / 100
GrossNImmob <- NReten * GrossNMin
share$HON <- share$HON + GrossNImmob
share$GrossNImmobYr <- share$GrossNImmobYr + GrossNImmob
NetNMin <- GrossNMin - GrossNImmob

#print(paste("NetNMin =", NetNMin))
#print(paste("GrossNMin =", NetNMin))
#print(paste("NReten =", NReten))
#print(paste("share$NRatioNit =", share$NRatioNit))
share$NH4 <- share$NH4 + NetNMin
NetNitr <- (share$NH4 * share$NRatioNit)
#print(paste("share$NH4 (1.5) =", share$NH4))
share$NO3 <- share$NO3 + NetNitr
share$NH4 <- share$NH4 - NetNitr

# Plant Uptake
RootNSinkStr <- (min((share$RootNSinkEff * TMult), 0.98))
#print(paste("share$NH4 (2) =", share$NH4))
#print(paste("share$NO3 =", share$NO3))
#print(paste("RootNSinkStr (1) =", RootNSinkStr))
PlantNUptake <- (share$NH4 + share$NO3) * RootNSinkStr
#print(paste("PlantNUptake =", PlantNUptake))
#print(paste("share$PlantN =", share$PlantN))
#print(paste("veg$MaxNStore =", veg$MaxNStore))
if ( (PlantNUptake + share$PlantN) > veg$MaxNStore) {
  PlantNUptake <- veg$MaxNStore - share$PlantN
  RootNSinkStr <- PlantNUptake / (share$NO3 + share$NH4)
}#end
if ( PlantNUptake < 0) {
  PlantNUptake <- 0
  RootNSinkStr <- 0
}#end
share$PlantN <- share$PlantN + PlantNUptake
share$PlantNUptakeYr <- share$PlantNUptakeYr + PlantNUptake
 
#print(paste("RootNSinkStr (2) =", RootNSinkStr))
NH4Up <- share$NH4 * RootNSinkStr
share$NH4 <- share$NH4 - NH4Up
NO3Up <- share$NO3 * RootNSinkStr
share$NO3 <- share$NO3 - NO3Up
 
share$NetNMinYr <- share$NetNMinYr + NetNMin
share$NetNitrYr <- share$NetNitrYr + NetNitr

#print(paste("share$NH4 (3) =", share$NH4))
return(share)
}
