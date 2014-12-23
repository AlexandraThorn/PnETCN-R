YearInit_NewPhenology <- function(share) {
#
# Annual initialization for the PnET ecosystem model.
#


#global veg site clim share rstep


share$GDDTot <- 0 #NEEDS to be updated at year roll-over for woody phenology
share$DWaterGDDTot <- 0 #NEEDS to be updated at year roll-over for woody phenology
share$GDD_DWater_gt_thresholdTot <- 0 #NEEDS to be updated at year roll-over for woody phenology
share$GDD_SoilWater_gt_thresholdTot <- 0 #NEEDS to be updated at year roll-over for woody phenology
share$WoodMRespYr <- 0
share$SoilRespYr <- 0
share$TotTrans <- 0
share$TotPsn <- 0
share$TotGrossPsn <- 0
share$TotDrain <- 0
share$TotPrec <- 0
share$TotEvap <- 0
share$TotWater <-0
share$FolProdCYr <- 0
share$WoodProdCYr <- 0
share$RootProdCYr <- 0
share$RootMRespYr <- 0
share$FolGRespYr <- 0
share$WoodGRespYr <- 0
share$RootGRespYr <- 0
share$OldGDDFolEff <- 0
#share$OldDWaterGDDFolEff <- 0
share$OldGDDWoodEff <- 0
share$NDrainYr<-0
share$NetNMinYr<-0
share$GrossNMinYr<-0
share$PlantNUptakeYr<-0
share$GrossNImmobYr<-0
share$TotalLitterMYr<-0
share$TotalLitterNYr<-0
share$NetNitrYr<-0
share$SoilDecRespYr<-0
share$WoodDecRespYr<-0
share$NetNMinLastYr <- share$NetNMinYr

return(share)
}
