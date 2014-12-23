CalculateYr_NewPhenology <- function(share,clim,site,veg,rstep,CN_Mode) {
#
# Annual C allocation for the PnET ecosystem model.
#

#For new phenology routine
share$PhenoStageYearStart=share$PhenoStage

#global veg site clim share rstep CN_Mode

share$NPPFolYr <- share$FolProdCYr / veg$CFracBiomass
share$NPPWoodYr <- share$WoodProdCYr / veg$CFracBiomass
share$NPPRootYr <- share$RootProdCYr / veg$CFracBiomass


# NEP calculation for PnET-II
share$NEP <- share$TotPsn - share$WoodMRespYr - share$WoodGRespYr - share$FolGRespYr - share$SoilRespYr

# PnET-CN Only -----------------------------------------------------------------
if ( CN_Mode==1) {
  # Annual total variables for PnET-CN
  share$NEP <- share$TotPsn - share$SoilDecRespYr - share$WoodDecRespYr - share$WoodMRespYr - share$WoodGRespYr - share$FolGRespYr - share$RootMRespYr - share$RootGRespYr
  share$FolN <- (share$FolMass * share$FolNCon / 100)
  share$FolC <- share$FolMass * veg$CFracBiomass
  share$TotalN <- share$FolN + share$WoodMassN + share$RootMassN + share$HON + share$NH4 + share$NO3 + share$BudN + share$DeadWoodN + share$PlantN
  share$TotalM <- (share$BudC / veg$CFracBiomass) + share$FolMass + (share$WoodMass + share$WoodC / veg$CFracBiomass) + share$RootMass + share$DeadWoodM + share$HOM + (share$PlantC / veg$CFracBiomass)

}#end
# ------------------------------------------------------------------------------

 return(share)
}
