storeoutput <- function(out,share,rstep,ystep,NewYear) {
#
# Add variables to the returned output structure so that the user may work
# with them (or save them) at the command line after running the model.
#

#global veg site clim out share rstep ystep


# Store iteration step variables (these may be monthly, daily etc. based on
# the stepping of the input climate data)
if ( NewYear==0) {
  out$grosspsn[rstep]<-share$GrsPsnMo
  out$netpsn[rstep]<-share$NetPsnMo
  out$netcbal[rstep]<-share$NetCBal
  out$vpd[rstep]<-share$VPD
  out$folmass[rstep]<-share$FolMass
  out$folnmass[rstep]<-share$FolN
  out$plantnMo[rstep]<-share$PlantN
  out$GDDTot[rstep]<-share$GDDTot
  out$PlantC[rstep]<-share$PlantC
  out$BudC[rstep]<-share$BudC
  out$BudN[rstep]<-share$BudN
  out$WoodC[rstep]<-share$WoodC
  out$WoodN[rstep]<-share$WoodMassN
  out$RootC[rstep]<-share$RootC
  out$RootN[rstep]<-share$RootMassN
  out$PosCBalMass[rstep]<-share$PosCBalMass
  out$FolMassMax[rstep]<-veg$FolMassMax
}#end


# Store annual variables at the conclusion of each years run
if ( NewYear==1) {
  out$nppfol[ystep]<-share$NPPFolYr
  out$nppwood[ystep]<-share$NPPWoodYr
  out$npproot[ystep]<-share$NPPRootYr
  out$nep[ystep]<-share$NEP
  out$gpp[ystep]<-share$TotGrossPsn
  
  # Water cycle
  out$waterstress[ystep]<-share$DWater
  out$trans[ystep]<-share$TotTrans
  out$soilwater[ystep]<-share$TotWater
  out$psn[ystep]<-share$TotPsn
  out$drain[ystep]<-share$TotDrain
  out$prec[ystep]<-share$TotPrec
  out$evap[ystep]<-share$TotEvap
  out$et[ystep]<-share$ET
  
  # Carbon cycle
  out$plantc[ystep]<-share$PlantC
  out$budc[ystep]<-share$BudC
  out$woodc[ystep]<-share$WoodC
  out$rootc[ystep]<-share$RootC
  
  # Nitrogen cycle
  out$plantnYr[ystep]<-share$PlantN
  out$budn[ystep]<-share$BudN
  out$ndrain[ystep]<-share$NDrainYr
  out$netnmin[ystep]<-share$NetNMinYr
  out$grossnmin[ystep]<-share$GrossNMinYr
  out$nplantuptake[ystep]<-share$PlantNUptakeYr
  out$grossnimob[ystep]<-share$GrossNImmobYr
  out$littern[ystep]<-share$TotalLitterNYr
  out$netnitrif [ystep]<-share$NetNitrYr 
  out$nratio[ystep]<-share$NRatio
  out$foln[ystep]<-veg$FolNCon
  
  # TBCA
  out$litm[ystep]<-share$TotalLitterMYr
  out$litn[ystep]<-share$TotalLitterNYr
  out$rmresp[ystep]<-share$RootMRespYr
  out$rgresp[ystep]<-share$RootGRespYr
  out$decresp[ystep]<-share$SoilDecRespYr
  
  
  #advance year counter
  #ystep<-ystep+1
}#end

return(out)
}
