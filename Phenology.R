Phenology <- function(share,clim,veg,rstep,GrowthPhase) {
#
# Phenlology calculations for the PnET ecosystem model.
#


#global veg site clim share rstep

if ( GrowthPhase == 1) {
  if ( (share$GDDTot >= veg$GDDFolStart) && (clim$doy[rstep] < veg$SenescStart)) {
    OldFolMass <- share$FolMass  
    GDDFolEff <- (share$GDDTot - veg$GDDFolStart) / (veg$GDDFolEnd - veg$GDDFolStart)
    GDDFolEff <- max(0, min(1, GDDFolEff))
    delGDDFolEff <- GDDFolEff - share$OldGDDFolEff
    share$FolMass <- share$FolMass + (share$BudC * delGDDFolEff) / veg$CFracBiomass
    share$FolProdCMo <- (share$FolMass - OldFolMass) * veg$CFracBiomass
    share$FolGRespMo <- share$FolProdCMo * veg$GRespFrac
    share$OldGDDFolEff <- GDDFolEff
  }else{
    share$FolProdCMo <- 0 
    share$FolGRespMo <- 0
  }#end
  
}else{
  share$FolLitM <- 0
  if ( (share$PosCBalMass < share$FolMass) && (clim$doy[rstep] > veg$SenescStart)) {
    FolMassNew <- max(share$PosCBalMass, veg$FolMassMin)
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
