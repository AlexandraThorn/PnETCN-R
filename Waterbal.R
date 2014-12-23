Waterbal <- function(share,clim,site,veg,rstep) {
#
# PnET ecosystem water balance routine.
#


#global veg site clim share rstep


# Potential transpiration
CanopyGrossPsnMG <- share$CanopyGrossPsn * 1000.0 * (44.0 / 12.0)
WUE <- (veg$WUEConst / share$VPD) * share$DWUE
PotTransd <- (CanopyGrossPsnMG / share$DelAmax / WUE) / 10000.0

# Precip input and snow/rain partitioning
Evap <- clim$prec[rstep] * veg$PrecIntFrac
precrem <- clim$prec[rstep] - Evap

if ( share$Tave > 2.0) {
  SnowFrac <- 0
}#end
if ( share$Tave < -5.0) {
  SnowFrac <- 1.0
}#end
if ( (share$Tave >= -5.0) && (share$Tave <= 2.0)) {
  SnowFrac <- (share$Tave - 2.0) / -7.0
}#end

# Snowmelt
#print(paste("share$SnowPack =",share$SnowPack))
#print(paste("precrem =",precrem))
#print(paste("SnowFrac =",SnowFrac))
#print(paste("rstep =",rstep))
share$SnowPack <- share$SnowPack + precrem * SnowFrac
if ( share$SnowPack > 0) {
  Tavew <- share$Tave
  if ( share$Tave < 1.0) {
    Tavew <- 1.0
  }#end
  SnowMelt <- 0.15 * Tavew * clim$dayspan[rstep]
  if ( SnowMelt > share$SnowPack) {
    SnowMelt <- share$SnowPack
  }#end
}else{
  SnowMelt <- 0
}#end
share$SnowPack <- share$SnowPack - SnowMelt

# Fast flow
waterin <- SnowMelt + precrem * (1.0 - SnowFrac)
FastFlow <- veg$FastFlowFrac * waterin
waterin <- waterin - FastFlow
waterind <- waterin / clim$dayspan[rstep]

# Transpiration
Trans <- 0
if ( PotTransd > 0) {
  TotSoilMoistEff <- 0
  for (  wday  in  1 : clim$dayspan[rstep] ) {
    share$Water <- share$Water + waterind
    if ( share$Water >= PotTransd/veg$f) {
      Transd <- PotTransd
    }else{
      Transd <- share$Water * veg$f
    }#end
    share$Water <- share$Water - Transd
    Trans <- Trans + Transd
    TotSoilMoistEff <- TotSoilMoistEff + (min(share$Water, site$WHC) / site$WHC)^(1.0 + veg$SoilMoistFact)
  }#end
  share$MeanSoilMoistEff <- min(1.0, (TotSoilMoistEff / clim$dayspan[rstep]))

  # Water stress
  share$DWater <- Trans / (PotTransd * clim$dayspan[rstep])
  share$Dwatertot <- share$Dwatertot + (share$DWater * clim$dayspan[rstep])
  share$DwaterIx <- share$DwaterIx + clim$dayspan[rstep]

}else{
  share$DWater <- 1.0
  share$Water <- share$Water + waterin
  share$MeanSoilMoistEff <- 1.0
}#end

# Calculate actural ozone effect and NetPsn with drought stress
if ( (clim$O3[rstep]>0) && (share$CanopyGrossPsn>0)) {
  if ( share$WUEO3Eff==0) {
    # no O3 effect on WUE (assumes no stomatal imparement)
    CanopyDO3 <- share$CanopyDO3Pot + ((1 - share$CanopyDO3Pot) * (1 - share$DWater))
  }else{
    # reduce the degree to which drought offsets O3 (assumes stomatal imparement in proportion to effects on psn)
    CanopyDO3 <- share$CanopyDO3Pot + ((1 - share$CanopyDO3Pot) * (1 - (share$DWater / share$CanopyDO3Pot)))
  }#end
  share$DroughtO3Frac <- share$CanopyDO3Pot / CanopyDO3
}else{
  CanopyDO3 <- 1
  share$DroughtO3Frac <- 1
}#end

# Manually turn water stress off
if ( site$WaterStress==0) {
  share$DWater<-1
}#end

CanopyGrossPsnAct <- share$CanopyGrossPsn * share$DWater
share$CanopyGrossPsnActMo <- CanopyGrossPsnAct * clim$dayspan[rstep]
share$GrsPsnMo <- share$CanopyGrossPsnActMo
share$NetPsnMo <- (CanopyGrossPsnAct - (share$DayResp + share$NightResp) * share$FolMass) * clim$dayspan[rstep] * CanopyDO3
share$NetPsnMoNoO3 <- (CanopyGrossPsnAct - (share$DayResp + share$NightResp) * share$FolMass) * clim$dayspan[rstep]

if ( share$Water > site$WHC) {
  share$Drainage <- share$Water - site$WHC
  share$Water <- site$WHC
}else{
  share$Drainage <- 0
}#end

share$Drainage <- share$Drainage + FastFlow
share$FracDrain <- share$Drainage / (share$Water + clim$prec[rstep])
share$TotTrans <- share$TotTrans + Trans
share$TotWater <- share$TotWater + share$Water
share$TotPsn <- share$TotPsn + share$NetPsnMo
share$TotDrain <- share$TotDrain + share$Drainage
share$TotPrec <- share$TotPrec + clim$prec[rstep]
share$TotEvap <- share$TotEvap + Evap
share$TotGrossPsn <- share$TotGrossPsn + share$GrsPsnMo
share$ET<-Trans+Evap

return(share)
}
