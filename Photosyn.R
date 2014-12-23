Photosyn <- function(share,clim,site,veg,rstep) {
#
# Photosynthesis routine for the PnET ecosystem model.
#


#global veg site clim share rstep


# Determine temperature penalty (DTemp)
PsnTMax <- veg$PsnTOpt + (veg$PsnTOpt - veg$PsnTMin)
DTemp <- ((PsnTMax - share$Tday) * (share$Tday - veg$PsnTMin)) / ((PsnTMax - veg$PsnTMin) / 2.0)^2
if ( (clim$tmin[rstep] < 6) && (DTemp > 0) && (share$GDDTot >= veg$GDDFolEnd)) {
  # Frost effect
  DTemp <- DTemp * (1.0 - ((6.0 - clim$tmin[rstep]) / 6.0) * (clim$dayspan[rstep] / 30.0))
}#end
DTemp <- max(DTemp, 0)

# Determine vapor pressure deficite penalty (DVPD)
share$DVPD <- 1.0 - veg$DVPD1 * share$VPD^veg$DVPD2
ten9 <- 1000000000.0

# Set atmospheric CO2 concentration
Ca <- clim$CO2[rstep]
#print(paste(clim$CO2[1:20]))

# CO2 effect on photosynthesis
# Leaf internal/external CO2
CiCaRatio <- (-0.075 * veg$FolNCon) + 0.875 #not used outside this block
# Ci at present (350 ppm) CO2
Ci350 <- 350 * CiCaRatio # used in this and next block
# Ci at RealYear CO2 level
CiElev <- Ca * CiCaRatio # used in this and next block
Arel350 <- 1.22 * ((Ci350 - 68) / (Ci350 + 136)) # not used outside this block
ArelElev <- 1.22 * ((CiElev - 68) / (CiElev + 136)) # not used outside this block
#print(paste("share$DelAmax=",share$DelAmax))
#print(paste("Arel350=",Arel350))
#print(paste("ArelElev=",ArelElev))
share$DelAmax <- 1 + ((ArelElev - Arel350) / Arel350) #needed for Waterbal calcs

# Calculate CO2 effect on conductance and set slope and intercept for A-gs relationship
if ( site$CO2gsEffect==1) {
  Delgs <- share$DelAmax / ((Ca - CiElev) / (350 - Ci350)) #only used in next line
  share$DWUE <- 1 + (1 - Delgs) #needed for Waterbal calcs
  gsSlope <- (-1.1309 * share$DelAmax) + 1.9762 #needed for photosyn
  gsInt <- (0.4656 * share$DelAmax) - 0.9701 #needed for photosyn
}else{
  share$DWUE <- 1 #needed for Waterbal calcs
  gsSlope <- (-0.6157 * share$DelAmax) + 1.4582 #needed for photosyn
  gsInt <- (0.4974 * share$DelAmax) - 0.9893 #needed for photosyn
}#end

# Maximum photosynthesis in morning
#print(paste("share$DelAmax=",share$DelAmax))
#print(paste("share$FolMass=",share$FolMass))
#print(paste("share$BudC=",share$BudC))
#print(paste("share$BudN=",share$BudN))
#print(paste("veg$FolNCon=",veg$FolNCon))
Amax <- (veg$AmaxA + veg$AmaxB * veg$FolNCon) * share$DelAmax

BaseFolResp <- veg$BaseFolRespFrac * Amax
#print(paste("Amax=",Amax))
Amax <- Amax * veg$AmaxFrac #From this point Amax is Amax over day not morning
#print(paste("Amax=",Amax))
#print(paste("BaseFolResp=",BaseFolResp))
GrossAmax <- Amax + BaseFolResp
#print(paste("share$DVPD=",share$DVPD))
#print(paste("share$DayLength=",share$DayLength))
#print(paste("GrossAmax=",GrossAmax))
#print(paste("DTemp=",DTemp))
#print(paste("ten9=",ten9))
GrossAmax <- (GrossAmax * share$DVPD * DTemp * share$DayLength * 12.0) / ten9

if ( GrossAmax < 0) {
  GrossAmax <- 0
}#end

# Calculate respiration
share$DayResp <- (BaseFolResp * veg$RespQ10^((share$Tday - veg$PsnTOpt) / 10.0) * share$DayLength * 12.0) / ten9
share$NightResp <- (BaseFolResp * veg$RespQ10^((share$Tnight - veg$PsnTOpt) / 10.0) * share$NightLength * 12.0) / ten9

#FIND - pretty muchly rest needs to be done interatively
# Initialize ozone effect
CanopyNetPsnO3 <- 0
CanopyNetPsnPot <- 0

# Calculate canopy ozone extinction based on folmass
O3Prof <- 0.6163 + (0.00105 * share$FolMass)

# Integrate photosynthesis across canopy layers
if ( share$FolMass > 0) {
  share$CanopyNetPsn <- 0
  share$CanopyGrossPsn <- 0
  share$LAI <- 0
  share$PosCBalMass <- share$FolMass # PosCBalMass defaults to current mass
  O3Effect <- 0
  Layer <- 0
  
  for (  ix  in  1 : 50 ) {
    i <- ix * (share$FolMass / 50.0) # assume unif (orm leaf distribution) {
    SLWLayer <- veg$SLWmax - (veg$SLWdel * i) # specif (ic leaf weight) {
    share$LAI <- share$LAI + (share$FolMass / 50.0) / SLWLayer #integrate LAI
    Il <- clim$par[rstep] * exp(-veg$k * share$LAI) # light attenuation
    LightEff <- (1.0 - exp(-Il * log(2.0) / veg$HalfSat)) # light atten. penalty
    LayerGrossPsnRate <- GrossAmax * LightEff # layer Psn/area
    LayerGrossPsn <- LayerGrossPsnRate * (share$FolMass / 50.0) # layer Psn
    LayerResp <- (share$DayResp + share$NightResp) * (share$FolMass / 50.0)
    LayerNetPsn <- LayerGrossPsn - LayerResp # layer net Psn
    if ( (LayerNetPsn < 0) && (share$PosCBalMass == share$FolMass)) {
      #print(paste("Negative LayerNetPsn during rstep=",rstep))
      share$PosCBalMass <- (ix - 1.0) * (share$FolMass / 50.0)
    }#end # update PosCBalMass if ( net psn <- 0) {
    share$CanopyNetPsn <- share$CanopyNetPsn + LayerNetPsn # integrate Psn
    share$CanopyGrossPsn <- share$CanopyGrossPsn + LayerGrossPsn # int gr. Psn
    
    # Ozone effects on Net Psn
    if ( (clim$O3[rstep]>0)) {
      # Convert netpsn to micromoles for calculating conductance
      netPsnumol <- ((LayerNetPsn * 10 ^ 6) / (share$DayLength * 12)) / ((share$FolMass / 50) / SLWLayer)
      # Calculate ozone extinction throughout the canopy
      Layer <- Layer + 1
      RelLayer <- Layer / 50
      RelO3 <- 1 - (RelLayer * O3Prof) ^ 3
      # Calculate Conductance (mm/s): Conductance down-regulates with prior O3 effects on Psn
      LayerG <- (gsInt + (gsSlope * netPsnumol)) * (1 - O3Effect)
      # For no downregulation use:    LayerG <- gsInt + (gsSlope * netPsnumol)
      if ( LayerG < 0) {
        LayerG <- 0
      }#end
      # Calculate cumulative ozone effect for each canopy layer with consideration that previous O3 effects were modif (ied by drought) {
      O3Effect <- (O3Effect * share$DroughtO3Frac) + (0.0026 * LayerG * clim$O3[rstep] * RelO3)
      LayerDO3 <- 1 - O3Effect
    }else{
      LayerDO3 <- 1
    }#end

    LayerNetPsnO3 <- LayerNetPsn * LayerDO3
    CanopyNetPsnO3 <- CanopyNetPsnO3 + LayerNetPsnO3
  }#end # exit canopy integration loop

  if ( (DTemp > 0) && (share$GDDTot > veg$GDDFolEnd) && (clim$doy[rstep] < veg$SenescStart)) { # if between }#end of growth and senescence and DTemp > 0
    share$PosCBalMassTot <- share$PosCBalMassTot + (share$PosCBalMass * clim$dayspan[rstep]) #PosCBalMass integrated, to be averaged at }#end of year 
    share$PosCBalMassIx <- share$PosCBalMassIx + clim$dayspan[rstep]
  }#end  

  if ( share$LightEffMin > LightEff) {
    share$LightEffMin <- LightEff
  }#end
  
}else{
  share$PosCBalMass <- 0
  share$CanopyNetPsn <- 0
  share$CanopyGrossPsn <- 0
  share$LAI <- 0
  share$DayResp <- 0
  share$NightResp <- 0
}#end

# Calculate whole-canopy ozone effects before drought
if ( (clim$O3[rstep]>0) && (share$CanopyGrossPsn>0)) {
  CanopyNetPsnPot <- share$CanopyGrossPsn - (share$DayResp * share$FolMass) - (share$NightResp * share$FolMass)
  share$CanopyDO3Pot <- CanopyNetPsnO3 / CanopyNetPsnPot
}else{
  share$CanopyDO3Pot <- 1
}#end

return(share)
}
