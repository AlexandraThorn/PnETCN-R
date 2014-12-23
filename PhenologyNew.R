UpdatePhenoStage <- function(share,clim,site,veg,rstep,CN_Mode){
  share$GDDTot_alt = share$GDDTot
  #share$GDDTot_alt = share$DWaterGDDTot
  #share$GDDTot_alt = share$GDD_DWater_gt_thresholdTot
  #share$GDDTot_alt = share$GDD_SoilWater_gt_thresholdTot
#share$PhenoStage distinguishes:
       #growth & maturity=1 from  dormancy & senescence=0
  if(share$PhenoStage==0) { #if senescent or dormant
    #if((share$GDDTot_alt<=veg$GDDFolEnd && share$PhenoStageYearStart==0) ||
    #   (share$GDDTot_alt>veg$GDDFolEnd && share$PhenoStageYearStart==1)) {
    #if((clim$doy[rstep]<veg$SenescStart && share$PhenoStageYearStart==0) ||
    #   (clim$doy[rstep]>veg$SenescStart && share$PhenoStageYearStart==1)) {
    if((clim$doy[rstep]<veg$SenescStart && share$GDDSenesc>veg$GDDFolStart) ||
       (clim$doy[rstep]>veg$SenescStart && share$GDDSenesc<veg$GDDFolStart)) {

      #need way to kick things off in first year...

      #if(veg$PFT==4){
        #figure out grass algorithm
        #for Tonzi, greenup seems to occur in first month with
             # prec > 1 (or 5) cm after drought during senescence
      #}else{
        #change to PhenoStage=1:
          # if GDDTot >=veg$GDDFolStart
          #  and either: current day < senesc doy AND PhenoStageYearStart = 0
          #      or: current day > senesc doy AND PhenoStageYearStart = 1
        #print(paste("share$GDDTot_alt=",share$GDDTot_alt, " ; veg$GDDFolStart=",veg$GDDFolStart))
        if((share$GDDTot_alt >=veg$GDDFolStart)){
          #print("foo")
          #if(clim$doy[rstep]<veg$SenescStart ||) #need conditionals to cover winter growing season
          share$PhenoStage=1
          if(share$IsFirstYear){
            share$IsFirstYear=FALSE
          }else{
            share=AllocateYr_NewPhenology(share,clim,site,veg,rstep,CN_Mode)#FIND
            share=GrowthInit_NewPhenology(share)#FIND
          }
  
        }
      #}  #end of second half of conditional for test of PFT==4
    }
  }else if(share$PhenoStage==1){ # if PhenoStage=1; i.e. if growing or mature
    if((share$GDDTot_alt>=veg$GDDFolStart && share$PhenoStageYearStart==0) ||
       (share$GDDTot_alt<veg$GDDFolStart && share$PhenoStageYearStart==1)) {
      #if(veg$PFT==4){
        #figure out whether I *need* a grass algorithm
      #}else{
        if(clim$doy[rstep] >= veg$SenescStart){
        #if(clim$doy[rstep] >= veg$SenescStart || share$GDDTot_alt > veg$GDDFolEnd){
          share$PhenoStage=0
        }
      #}  #end of second half of conditional for test of PFT==4
    }
  }
  
  return(share)
}

PhenologyNew <- function(share,clim,veg,rstep,GrowthPhase) {
#
# Phenology calculations for the PnET ecosystem model.
#


#global veg site clim share rstep

if ( GrowthPhase == 1) {
  #print("GrowthPhase=1")
  if (share$PhenoStage==1) {
    OldFolMass <- share$FolMass  
    #GDDFolEff <- (share$GDDTot - veg$GDDFolStart) / (veg$GDDFolEnd - veg$GDDFolStart)
    #GDDFolEff <- max(0, min(1, GDDFolEff)) # if > 1, lower to 1
    #delGDDFolEff <- GDDFolEff - share$OldGDDFolEff # change from previous value
    if(veg$GDDFolEnd<veg$GDDFolStart){ #if leaf growth bridges year turnover
      ClimYear=subset(clim,clim$year==clim$year[rstep])
      ClimYear$GDD=ClimYear$dayspan*rowMeans(data.frame(ClimYear$tmin,ClimYear$tmax))
      ClimYear$GDD[ClimYear$GDD<0]=0
      GDDMax=sum(ClimYear$GDD)
      #print(paste("GDDMax=",GDDMax))
      GDDFolGrowth=GDDMax + veg$GDDFolEnd - veg$GDDFolStart
      #print(paste("GDDFolGrowth=",GDDFolGrowth))
      if(share$GDDTot_alt>veg$GDDFolStart){
        GDDFolEff = (share$GDDTot_alt - veg$GDDFolStart)/GDDFolGrowth
      }else{
        GDDFolEff = (share$GDDTot_alt + GDDMax - veg$GDDFolStart)/GDDFolGrowth
      }
      #print(paste("GDDFolEff (1)=",GDDFolEff))
    }else{
      GDDFolEff <- (share$GDDTot_alt - veg$GDDFolStart) / (veg$GDDFolEnd - veg$GDDFolStart)
    }
    GDDFolEff <- max(0, min(1, GDDFolEff)) # if > 1, lower to 1
    #print(paste("GDDFolEff (2)=",GDDFolEff))
    delGDDFolEff <- GDDFolEff - share$OldGDDFolEff # change from previous value
    #DWaterGDDFolEff <- (share$DWaterGDDTot - veg$GDDFolStart) / (veg$GDDFolEnd - veg$GDDFolStart)
    #DWaterGDDFolEff <- max(0, min(1, DWaterGDDFolEff)) # if > 1, lower to 1
    #delDWaterGDDFolEff <- DWaterGDDFolEff - share$OldDWaterGDDFolEff # change from previous value
    #print(paste("share$FolMass=", share$FolMass, ", share$BudC=", share$BudC, ",delGDDFolEff=", delGDDFolEff, ", veg$CFracBiomass=", veg$CFracBiomass, ", share$OldGDDFolEff=", share$OldGDDFolEff, ", share$GDDTot_alt=", share$GDDTot_alt, ", rstep=", rstep))
    share$FolMass <- share$FolMass + (share$BudC * delGDDFolEff) / veg$CFracBiomass
    #print(paste("DWaterGDDFolEff=", DWaterGDDFolEff ))
    #share$FolMass <- share$FolMass + (share$BudC * delDWaterGDDFolEff) / veg$CFracBiomass
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
  if ( (share$PosCBalMass < share$FolMass) && share$PhenoStage==0) {
    #incorporating water deficit as phenology cue
    FolMassNew <- min(share$FolMass,max(share$PosCBalMass, veg$FolMassMin))
    FolMassNew <- min(FolMassNew, share$FolMass * share$DWater)
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
}#end

return(share)
}
