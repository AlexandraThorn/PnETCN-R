AllocateMo <- function(share,clim,veg,rstep,CN_Mode) {
#
# Monthly C allocation for the PnET ecosystem model.
#


#global veg site clim share rstep CN_Mode

#Add new C to PlantC pool
share$PlantC <- share$PlantC + share$NetPsnMo - share$FolGRespMo

#Add monthly increments to yearly wood respiration, fol. C production, and fol.
# growth respiration
WoodMRespMo <- share$CanopyGrossPsnActMo * veg$WoodMRespA
share$WoodMRespYr <- share$WoodMRespYr + WoodMRespMo
share$FolProdCYr <- share$FolProdCYr + share$FolProdCMo
share$FolGRespYr <- share$FolGRespYr + share$FolGRespMo

if ( share$GDDTot >= veg$GDDWoodStart & veg$PFT!=4) {
  #print("if=if")
  #print(paste("share$GDDTot=",share$GDDTot))
  #print(paste("veg$GDDWoodStart=",veg$GDDWoodStart))
  #print(paste("veg$GDDWoodEnd=",veg$GDDWoodEnd))
  GDDWoodEff <- (share$GDDTot - veg$GDDWoodStart) / (veg$GDDWoodEnd - veg$GDDWoodStart)
  GDDWoodEff <- max(0, min(1.0, GDDWoodEff)) 
  delGDDWoodEff <- GDDWoodEff - share$OldGDDWoodEff #delta prop. wood GDDs
  #print(paste("share$WoodC=", share$WoodC))
  WoodProdCMo <- share$WoodC * delGDDWoodEff #Wood C allocated to growth
  WoodGRespMo <- WoodProdCMo * veg$GRespFrac #Wood growth respiration calc'd
  share$WoodProdCYr <- share$WoodProdCYr + WoodProdCMo
  share$WoodGRespYr <- share$WoodGRespYr + WoodGRespMo
  share$OldGDDWoodEff <- GDDWoodEff
}else{
  #print("if=else")
  WoodProdCMo <- 0
  WoodGRespMo <- 0
}#end
#print(paste("WoodProdCMo (AllocateMo)=", WoodProdCMo))

TMult <- (exp(0.1 * (share$Tave - 7.1)) * 0.68) * 1.0
#print(paste("clim$dayspan[rstep] (AllocateMo)=", clim$dayspan[rstep]))
#print(paste("veg$RootAllocA (AllocateMo)=", veg$RootAllocA))
#print(paste("veg$RootAllocB (AllocateMo)=", veg$RootAllocB))
#print(paste("share$FolProdCMo (AllocateMo)=", share$FolProdCMo))
RootCAdd <- veg$RootAllocA * (clim$dayspan[rstep] / 365.0) + veg$RootAllocB * share$FolProdCMo #Root C allocation defined in terms of foliar C production
#print(paste("share$RootC (1) (AllocateMo)=", share$RootC))
share$RootC <- share$RootC + RootCAdd
#print(paste("TMult (AllocateMo)=", TMult))
#print(paste("share$RootC (2) (AllocateMo)=", share$RootC))
RootAllocCMo <- min(1.0, ((1.0/12.0) * TMult)) * share$RootC
share$RootC <- share$RootC - RootAllocCMo
#print(paste("RootAllocCMo (1) (AllocateMo)=", RootAllocCMo))
#print(paste("veg$RootMRespFrac (AllocateMo)=", veg$RootMRespFrac))
#print(paste("veg$GRespFrac (AllocateMo)=", veg$GRespFrac))
RootProdCMo <- RootAllocCMo / (1.0 + veg$RootMRespFrac + veg$GRespFrac)
#print(paste("RootProdCMo (1) (AllocateMo)=", RootProdCMo))
share$RootProdCYr <- share$RootProdCYr + RootProdCMo
RootMRespMo <- RootProdCMo * veg$RootMRespFrac
share$RootMRespYr <- share$RootMRespYr + RootMRespMo
RootGRespMo <- RootProdCMo * veg$GRespFrac
share$RootGRespYr <- share$RootGRespYr + RootGRespMo
share$PlantC <- share$PlantC - RootCAdd - WoodMRespMo - WoodGRespMo
share$NetCBal <- share$NetPsnMo - share$SoilRespMo - WoodMRespMo - WoodGRespMo - share$FolGRespMo

# PnET-CN Only -----------------------------------------------------------------
if ( CN_Mode==1) {
  share$WoodMass <- share$WoodMass + (WoodProdCMo / veg$CFracBiomass)
  share$WoodMassN <- share$WoodMassN + ((WoodProdCMo / veg$CFracBiomass) * veg$WLPctN * share$NRatio)
  #print(paste("share$PlantN(1) (AllocateMo)=", share$PlantN))
  #print(paste("WoodProdCMo (AllocateMo)=", WoodProdCMo))
  #print(paste("veg$CFracBiomass (AllocateMo)=", veg$CFracBiomass))
  #print(paste("veg$WLPctN (AllocateMo)=", veg$WLPctN))
  #print(paste("share$NRatio (AllocateMo)=", share$NRatio))
  share$PlantN <- share$PlantN - ((WoodProdCMo / veg$CFracBiomass) * veg$WLPctN * share$NRatio)
  share$RootMass <- share$RootMass + (RootProdCMo / veg$CFracBiomass)
  share$RootMassN <- share$RootMassN + ((RootProdCMo / veg$CFracBiomass) * veg$RLPctN * share$NRatio)
  #print(paste("share$PlantN(2) (AllocateMo)=", share$PlantN))
  #print(paste("RootProdCMo (2) (AllocateMo)=", RootProdCMo))
  #print(paste("veg$CFracBiomass (AllocateMo)=", veg$CFracBiomass))
  #print(paste("veg$RLPctN (AllocateMo)=", veg$RLPctN))
  #print(paste("share$NRatio (AllocateMo)=", share$NRatio))
  share$PlantN <- share$PlantN - ((RootProdCMo / veg$CFracBiomass) * veg$RLPctN * share$NRatio)
  #print(paste("share$PlantN(3) (AllocateMo)=", share$PlantN))
  share$NetCBal <- share$NetPsnMo - share$SoilDecResp - share$WoodDecResp - WoodMRespMo - WoodGRespMo - share$FolGRespMo - RootMRespMo - RootGRespMo
}#end
# ------------------------------------------------------------------------------

return(share)
}
