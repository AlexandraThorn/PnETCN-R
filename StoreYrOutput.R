##StoreYrOutput##
# Saves the annual results to an output file

StoreYrOutput = function(share,veg,ystep,outyear){

		outyear[ystep,1] = share$NPPFolYr;
		outyear[ystep,2] = share$NPPWoodYr;
		outyear[ystep,3] = share$NPPRootYr;
		outyear[ystep,4] = share$NEP;
		outyear[ystep,5] = share$TotGrossPsn;
		
		# Water Cycle
		outyear[ystep,6] = share$DWater;
		outyear[ystep,7] = share$TotTrans;
		outyear[ystep,8] = share$TotWater;
		outyear[ystep,9] = share$TotPsn;
		outyear[ystep,10] = share$TotDrain;
		outyear[ystep,11] = share$TotPrec;
		outyear[ystep,12] = share$TotEvap;
		outyear[ystep,13] = share$ET;
		
		# Carbon Cycle
		outyear[ystep,14] = share$PlantC;
		outyear[ystep,15] = share$BudC;
		outyear[ystep,16] = share$WoodC;
		outyear[ystep,17] = share$RootC;
		
		# Nitrogen Cycle
		outyear[ystep,18] = share$PlantN;
		outyear[ystep,19] = share$BudN;
		outyear[ystep,20] = share$NDrainYr;
		outyear[ystep,21] = share$NetNMinYr;
		outyear[ystep,22] = share$GrossNMinYr;
		outyear[ystep,23] = share$PlantNUptakeYr;
		outyear[ystep,24] = share$GrossNImmobYr;
		outyear[ystep,25] = share$TotalLitterNYr;
		outyear[ystep,26] = share$NetNitrYr;
		outyear[ystep,27] = share$NRatio;
		outyear[ystep,28] = veg$FolNCon;
		
		outyear[ystep,29] = share$TotalLitterMYr;
		outyear[ystep,30] = share$TotalLitterNYr;
		outyear[ystep,31] = share$RootMRespYr;
		outyear[ystep,32] = share$RootGRespYr;
		outyear[ystep,33] = share$SoilDecRespYr;
		
		# Advance the year counter
		ystep = ystep + 1;

	return(list(ystep,outyear))
}