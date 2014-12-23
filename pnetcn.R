source('initvars.R')
source('AllocateMo.R')
source('AllocateYr.R')
source('AtmEnviron.R')
source('CNTrans.R')
source('Decomp.R')
source('Leach.R')
source('Phenology.R')
source('Photosyn.R')
source('SoilResp.R')
source('storeoutput.R')
source('Waterbal.R')
source('YearInit.R')

pnetcn <-function( climIn,siteIn,vegIn) {  
#
# [out]<-pnetcn(climIn,siteIn,vegIn)
#

## Initialize
#clear global
#
## Vegetation and site constants
#global veg     # Vegetation parameters (input)
#global site    # Site specif (ic parameters (input)) {
#global clim    # Climatological time series (input)
#global share   # Internal variables shared between functions
#global out     # Calculated variables int}#ended to be returned to the user
#global rstep   # The current running timestep of the execution (day or month)
#global ystep   # The yearstep (only used in constructing the output array)
#global CN_Mode # 1 if ( running PnET-CN) {

# Assign input structures to those used within the model
veg <- vegIn  
site <- siteIn  
clim <- climIn

ystep=1
rstep=1
# Initialize variables
share=initvars(clim,veg)
#print(paste("share$PlantN (0) (pnetcn, rstep=",rstep,") =",share$PlantN))
share$FolMassMax=NA
share$FolMassMin=NA
share$FolNCon=NA
share$year=clim$year[1]
share$doy=clim$doy[1]
share$time.POSIXt=strptime(paste(share$year,share$doy,sep="-"),format="%Y-%j")
outyearly=share
#outyearly$NetPsnMoNoO3=NA
##outyearly$SnowPack=NA
outmonthly=share
#outmonthly$NetPsnMoNoO3=NA
##outmonthly$SnowPack=NA #APPARENTLY this modifies share... don't want
#print(paste("share$GDDTot=",share$GDDTot))
clim$dayspan=clim$doy
#clim$dayspan=30
for (  i in 1 : length(clim$doy) ) {
  clim$dayspan[i]<-30
}#end

CN_Mode<-1

## Main run loop
# Loop through all climate records
#print(paste("rstep(1)=",rstep))
#print(paste("clim$doy[rstep](1)=",clim$doy[rstep]))
for (  rstep in 1 : length(clim$doy) ) {
  #print(paste("rstep(2)=",rstep))
  #print(paste("clim$doy[rstep](2)=",clim$doy[rstep]))
  #print(paste("(rstep!=1) =",rstep!=1))

  # End-of-year activity
  if ( (rstep != 1) && (clim$doy[rstep] < clim$doy[rstep-1])) {
    share$FolMassMax=veg$FolMassMax #to be able to change
    share$FolMassMin=veg$FolMassMin #to be able to change
    share$FolNCon=veg$FolNCon #to be able to change
    share=AllocateYr(share,clim,site,veg,rstep,1)
    #print("I am here?")
    #print(paste("veg$FolMassMax=", veg$FolMassMax))
    #print(paste("share$FolMassMax=", share$FolMassMax))
    veg$FolMassMax=share$FolMassMax #changing ************
    veg$FolMassMin=share$FolMassMin #changing
    veg$FolNCon=share$FolNCon #changing
    #out=storeoutput(1)
    outyearly=rbind(outyearly,share)
    ystep=ystep+1
    print(paste("Year ",clim$year[rstep], "starting."))
    share=YearInit(share)
    #print(paste("rstep(3)=",rstep))
    #print(paste("clim$doy[rstep](3)=",clim$doy[rstep]))
  }#end

  # Call subroutines
  #print(paste("share$PlantN (1) (pnetcn, rstep=",rstep,") =",share$PlantN))
  share=AtmEnviron(share,clim,rstep)
  share=Phenology(share,clim,veg,rstep,1)
  share=Photosyn(share,clim,site,veg,rstep)
  share$SnowPack=site$SnowPack #so can be modified by Waterbal routine
  #print(paste("share$SnowPack (before, rstep=",rstep,") =",share$SnowPack))
  #print(paste("site$SnowPack (before, rstep=",rstep,") =",site$SnowPack))
  share=Waterbal(share,clim,site,veg,rstep)
  site$SnowPack=share$SnowPack #incorporates change to SnowPack
  #print(paste("share$SnowPack (after, rstep=",rstep,") =",share$SnowPack))
  #print(paste("site$SnowPack (after, rstep=",rstep,") =",site$SnowPack))
  #AllocateMo_NoTMult
  #print(paste("share$PlantN (2) (pnetcn, rstep=",rstep,") =",share$PlantN))
  share=AllocateMo(share,clim,veg,rstep,CN_Mode)
  share=Phenology(share,clim,veg,rstep,2)
  #print(paste("share$PlantN (2.5) (pnetcn, rstep=",rstep,") =",share$PlantN))
  share=CNTrans(share,clim,site,veg,rstep)
  #print(paste("share$PlantN (3) (pnetcn, rstep=",rstep,") =",share$PlantN))
  share=Decomp(share,clim,veg,rstep)
  share=Leach(share)
  #out=storeoutput(outmonthly,share,rstep,0)
  #print(names(outmonthly))
  #print(names(share))
  #add dates
  share$year=clim$year[rstep]
  share$doy=clim$doy[rstep]
  share$time.POSIXt=strptime(paste(share$year,share$doy,sep="-"),format="%Y-%j")
  outmonthly=rbind(outmonthly,share)

}#end

# Calculate final year
#AllocateYr
share=AllocateYr(share,clim,site,veg,rstep,1)
#storeoutput(1)
outyearly=rbind(outyearly,share)
return(c(outmonthly,outyearly));
}
