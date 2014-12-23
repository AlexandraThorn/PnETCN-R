CNTrans <- function(share,clim,site,veg,rstep) {
#
# Carbon and nitrogen translocation routine
#


#global veg site clim share rstep

# Check for a disturbance year
BiomLossFrac<-0
RemoveFrac<-0
if ( clim$doy[rstep]>335) {
  for (  i in 1 : length(site$distyear) ) {
    if ( clim$year[rstep]==site$distyear[i]) {
      BiomLossFrac <- site$distintensity[i]
      RemoveFrac <- site$distremove[i]
      share$HOM <- share$HOM * (1 - site$distsoilloss[i])
      share$HON <- share$HON * (1 - site$distsoilloss[i])
      break
    }#end
  }#end
}#end

#print(paste("veg$RootTurnoverA=",veg$RootTurnoverA))
#print(paste("veg$RootTurnoverB=",veg$RootTurnoverB))
#print(paste("share$NetNMinLastYr=",share$NetNMinLastYr))
#print(paste("veg$RootTurnoverC=",veg$RootTurnoverC))
RootTurnover <- veg$RootTurnoverA - (veg$RootTurnoverB * share$NetNMinLastYr) + (veg$RootTurnoverC * share$NetNMinLastYr^2)
#print(paste("RootTurnover (1)=",RootTurnover))
if ( RootTurnover>2.5) {
  RootTurnover <- 2.5
}#end
if ( RootTurnover<0.1) {
  RootTurnover <- 0.1
}#end
#print(paste("RootTurnover (2)=",RootTurnover))
##print(paste("clim$dayspan =",clim$dayspan))
#print(paste("rstep=",rstep))
#print(paste("clim$dayspan[rstep] =",clim$dayspan[rstep]))
RootTurnover <- RootTurnover * (clim$dayspan[rstep] / 365)
#print(paste("BiomLossFrac=",BiomLossFrac))
#print(paste("RootTurnover(3)=",RootTurnover))
if ( BiomLossFrac>RootTurnover) {
  RootTurnover <- BiomLossFrac
}#end
RootLitM <- share$RootMass * RootTurnover
RootLitN <- RootLitM * (share$RootMassN / share$RootMass)
share$RootMass <- share$RootMass - RootLitM
share$RootMassN <- share$RootMassN - RootLitN

if (veg$PFT!=4) #ADDED CONDITIONALS FOR GRASSLAND - NO WOOD
{
  #print(paste("share$WoodMass (2) (CNTrans)=",share$WoodMass))
  #print(paste("veg$WoodTurnover (2) (CNTrans)=",veg$WoodTurnover))
  if ( BiomLossFrac>0) {
    #print(paste("if=if"))
    WoodLitM <- share$WoodMass * BiomLossFrac * (1 - RemoveFrac)
    WoodLitN <- share$WoodMassN * BiomLossFrac * (1 - RemoveFrac)
    share$WoodMass <- share$WoodMass * (1 - BiomLossFrac)
    share$WoodMassN <- share$WoodMassN * (1 - BiomLossFrac)
  }else{
    #print(paste("if=else"))
    WoodLitM <- share$WoodMass * veg$WoodTurnover * (clim$dayspan[rstep] / 365)
    WoodLitN <- share$WoodMassN * veg$WoodTurnover * (clim$dayspan[rstep] / 365)
    share$WoodMass <- share$WoodMass - WoodLitM
    share$WoodMassN <- share$WoodMassN - WoodLitN
  }#end
  
  #print(paste("share$DeadWoodM(1) (CNTrans)=",share$DeadWoodM))
  #print(paste("WoodLitM (2) (CNTrans)=",WoodLitM))
  share$DeadWoodM <- share$DeadWoodM + WoodLitM
  share$DeadWoodN <- share$DeadWoodN + WoodLitN
  #print(paste("share$DeadWoodM(2) (CNTrans)=",share$DeadWoodM))
  #print(paste("veg$WoodLitLossRate (CNTrans)=",veg$WoodLitLossRate))
  #print(paste("clim$dayspan[rstep] (CNTrans)=",clim$dayspan[rstep]))
  WoodMassLoss <- share$DeadWoodM * veg$WoodLitLossRate * (clim$dayspan[rstep] / 365)
  #print(paste("WoodMassLoss (CNTrans)=",WoodMassLoss))
  #print(paste("veg$WoodLitCLoss (CNTrans)=",veg$WoodLitCLoss))
  WoodTransM <- WoodMassLoss * (1 - veg$WoodLitCLoss)
  share$WoodDecResp <- (WoodMassLoss - WoodTransM) * veg$CFracBiomass
  share$WoodDecRespYr <- share$WoodDecRespYr + share$WoodDecResp
  WoodTransN <- (WoodMassLoss / share$DeadWoodM) * share$DeadWoodN
  share$DeadWoodM <- share$DeadWoodM - WoodMassLoss
  share$DeadWoodN <- share$DeadWoodN - WoodTransN
}

FolNLoss <- share$FolLitM * (veg$FolNCon / 100) #N lost from leaves
Retrans <- FolNLoss * veg$FolNRetrans # Portion lost N imported to stem
#print(paste("share$PlantN(1) (CNTrans)=", share$PlantN))
share$PlantN <- share$PlantN + Retrans # Update Plant N based on import
#print(paste("share$PlantN(2) (CNTrans)=", share$PlantN))
FolLitN <- FolNLoss - Retrans # Remainder is lost as litter

if ( BiomLossFrac>0) {
  share$FolLitM <- share$FolLitM + (share$FolMass * BiomLossFrac)
  FolLitN <- FolLitN + (share$FolMass * BiomLossFrac * (veg$FolNCon / 100))
  share$FolMass <- share$FolMass * (1 - BiomLossFrac)
  share$PlantC <- share$PlantC * (1 - BiomLossFrac)
  share$PlantN <- share$PlantN + (veg$MaxNStore - share$PlantN) * BiomLossFrac
}#end
#print(paste("share$PlantN(3) (CNTrans)=", share$PlantN))

#print(paste("share$TotalLitterM (CNTrans)=",share$TotalLitterM))
#print(paste("share$TotalLitterN (CNTrans)=",share$TotalLitterN))
#print(paste("share$FolLitM (CNTrans)=",share$FolLitM))
#print(paste("RootLitM (CNTrans)=",RootLitM))
#print(paste("WoodTransM (CNTrans)=",WoodTransM))
#print(paste("FolLitN (CNTrans)=",FolLitN))
#print(paste("RootLitN (CNTrans)=",RootLitN))
#print(paste("WoodTransN (CNTrans)=",WoodTransN))
if(veg$PFT==4)
{
  share$TotalLitterM <- share$FolLitM + RootLitM
  share$TotalLitterN <- FolLitN + RootLitN
}else{
  share$TotalLitterM <- share$FolLitM + RootLitM + WoodTransM
  share$TotalLitterN <- FolLitN + RootLitN + WoodTransN
}

# Agriculture
#print(paste("rstep=",rstep))
#print(paste("clim$year[rstep]=",clim$year[rstep]))
if ( clim$year[rstep]>=site$agstart && clim$year[rstep]<site$agstop) {
  share$TotalLitterM <- share$TotalLitterM * (1 - site$agrem)
  share$TotalLitterN <- share$TotalLitterN * (1 - site$agrem)
  if(veg$PFT!=4)
  {
    share$WoodMass <- share$WoodMass * (1 - site$agrem * (clim$dayspan[rstep] / 365))
    share$WoodMassN <- share$WoodMassN * (1 - site$agrem * (clim$dayspan[rstep] / 365))
  }
}#end

share$TotalLitterMYr <- share$TotalLitterMYr + share$TotalLitterM
share$TotalLitterNYr <- share$TotalLitterNYr + share$TotalLitterN

return(share)

}
