GrowthInit_NewPhenology <- function(share) {
#
# Annual initialization for the PnET ecosystem model.
#


#global veg site clim share rstep


share$PosCBalMassTot <- 0 # Update after AllocateYr & before Photosyn starts
share$PosCBalMassIx <- 0 # Update after AllocateYr & before Photosyn starts
share$Dwatertot <- 0 #Update after AllocateYr & before Waterbal starts?
share$DwaterIx <- 0 #Update after AllocateYr & before Waterbal starts?
share$LightEffMin<-1 #?

return(share)
}
