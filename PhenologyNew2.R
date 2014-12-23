UpdatePhenoStage <- function(share,clim,site,veg,rstep,CN_Mode){
  share$GDDTot_alt = share$DWaterGDDTot
  #share$GDDTot_alt = share$GDD_DWater_gt_thresholdTot
  #share$GDDTot_alt = share$GDD_SoilWater_gt_thresholdTot
#share$PhenoStage distinguishes:
       #growth & maturity=1 from  dormancy & senescence=0
  if(share$PhenoStage==0) { #if senescent or dormant
    #if((share$GDDTot_alt<=veg$GDDFolEnd && share$PhenoStageYearStart==0) ||
    #   (share$GDDTot_alt>veg$GDDFolEnd && share$PhenoStageYearStart==1)) {
    if((clim$doy[rstep]<veg$SenescStart && share$PhenoStageYearStart==0) ||
       (clim$doy[rstep]>veg$SenescStart && share$PhenoStageYearStart==1)) {

      if(veg$PFT==4){
        #figure out grass algorithm
      }else{
        #change to PhenoStage=1:
          # if GDDTot >=veg$GDDFolStart
          #  and either: current day < senesc doy AND PhenoStageYearStart = 0
          #      or: current day > senesc doy AND PhenoStageYearStart = 1
        if((share$GDDTot_alt >=veg$GDDFolStart)){
         # if(clim$doy[rstep]<veg$SenescStart ||) #need conditionals to cover winter growing season
          share$PhenoStage=1
          if(share$IsFirstYear){
            share$IsFirstYear=FALSE
          }else{
            share=AllocateYr_NewPhenology(share,clim,site,veg,rstep,CN_Mode)
            share=GrowthInit_NewPhenology(share)
          }
  
        }
      }
    }
  }else if(share$PhenoStage==1){ # if PhenoStage=1; i.e. if growing or mature
    #NEED TO FIX AND  CLOSE BRACKETS
    if((share$GDDTot_alt>=veg$GDDFolStart && share$PhenoStageYearStart==0) ||
       (share$GDDTot_alt<veg$GDDFolStart && share$PhenoStageYearStart==1)) {
      if(veg$PFT==4){
        #figure out whether I *need* a grass algorithm
      }else{
        if(clim$doy[rstep] >= veg$SenescStart){
        #if(clim$doy[rstep] >= veg$SenescStart || share$GDDTot_alt > veg$GDDFolEnd){
          share$PhenoStage=0
        }
      }
    }
  }
  
  return(share)
}

PhenologyNew2 <- function(share,clim,veg,rstep,GrowthPhase) {
#
# Phenology calculations for the PnET ecosystem model.
#


#global veg site clim share rstep

if ( GrowthPhase == 1) {
  if (share$PhenoStage==1) {
    OldFolMass <- share$FolMass  
    GDDFolEff <- (share$GDDTot_alt - veg$GDDFolStart) / (veg$GDDFolEnd - veg$GDDFolStart)
    GDDFolEff <- max(0, min(1, GDDFolEff)) # if > 1, lower to 1
    delGDDFolEff <- GDDFolEff - share$OldGDDFolEff # change from previous value
    #print(paste("share$FolMass=", share$FolMass, ", share$BudC=", share$BudC, ",delGDDFolEff=", delGDDFolEff, ", veg$CFracBiomass=", veg$CFracBiomass, ", share$OldGDDFolEff=", share$OldGDDFolEff, ", share$GDDTot_alt=", share$GDDTot_alt, ", rstep=", rstep))
    if(share$GDDTot_alt<veg$GDDFolEnd){
      share$FolMass <- share$FolMass + (share$BudC * delGDDFolEff) / veg$CFracBiomass
    }else{
      if((share$DWater >= 0.3) && (share$FolMass <= share$PosCBalMass)){
        EstETPerFolMass = share$ET / share$FolMass
        maxFolMassH2O = share$Water / EstETPerFolMass # gH2O available / (gH2O/gLeaf)

        #PlantC>=FolProdCMo + veg$RootAllocA * clim$dayspan[rstep]/365 + veg$RootAllocB * share$FolProdCMo
        #FolProdCMo + veg$RootAllocB * FolProdCMo <= PlantC - veg$RootAllocA * clim$dayspan[rstep]/365
        #FolProdCMo <= (PlantC - veg$RootAllocA * clim$dayspan[rstep]/365)/(1+veg$RootAllocA
        maxFolProdC = max(0,(share$PlantC - veg$RootAllocA * clim$dayspan[rstep]/365)/(1+veg$GRespFrac+veg$RootAllocA)) # still need to account for growth respiration!!!

        share$FolMass = max(share$FolMass,min(maxFolMassH2O, share$FolMass + maxFolProdC/veg$CFracBiomass,share$FolMass*(1+clim$dayspan[rstep]*0.5/30))) #Built in RGR parameter - 0.5 per month for starters; NEED TO TEST
      }
    }
    share$FolProdCMo <- (share$FolMass - OldFolMass) * veg$CFracBiomass
    share$FolGRespMo <- share$FolProdCMo * veg$GRespFrac
    share$OldGDDFolEff <- GDDFolEff
    #share$OldDWaterGDDFolEff <- DWaterGDDFolEff
  }else{
    share$FolProdCMo <- 0 
    share$FolGRespMo <- 0
  }#end
  
}else{ #if GrowthPhase==2
  share$FolLitM <- 0
  #if(share$PhenoStage==1 && (share$DWaterGDDTot<veg$GDDFolStart || share$DWaterGDDTot>veg$GDDFolEnd)){
  #  if(share$DWater<0.3){ #this is an internal parameter
  #    FolMassNew <- max(0,share$FolMass*(1-0.003*clim$dayspan[rstep]))#and another
  #    if ( FolMassNew < share$FolMass) {
  #      share$LAI <- share$LAI * (FolMassNew / share$FolMass)
  #      share$FolLitM <- share$FolMass - FolMassNew
  #    }#end
  #    share$FolMass <- FolMassNew
  #  }
  #}else 
  if(share$PhenoStage==1 && share$DWater < 0.3 && share$GDDTot_alt > veg$GDDFolEnd){ #new parameter
    #permitting senescence during water limitation
    FolMassNew <- share$FolMass*(1-clim$dayspan[rstep]*0.05/30) #new parameter
  }else if ( (share$PosCBalMass < share$FolMass) && share$PhenoStage==0) {
    #incorporating water deficit as phenology cue
    FolMassNew <- min(share$FolMass,max(share$PosCBalMass, veg$FolMassMin))
    FolMassNew <- min(FolMassNew, share$FolMass * share$DWater)
  }else{
    FolMassNew <- share$FolMass
  }
  if ( FolMassNew == 0) {
    share$LAI <- 0
  }else if ( FolMassNew < share$FolMass) {
    share$LAI <- share$LAI * (FolMassNew / share$FolMass)
  }#end
  if ( FolMassNew < share$FolMass) {
    share$FolLitM <- share$FolMass - FolMassNew
  }#end
  share$FolMass <- FolMassNew
}#end

return(share)
}
