Leach <- function(share) {
#
# PnET-CN leaching routine
#


#global veg site clim share rstep


NDrain <- share$FracDrain * share$NO3
share$NDrainYr <- share$NDrainYr + NDrain
NDrainMgL <- (NDrain * 1000) / (share$Drainage * 10)
share$NO3 <- share$NO3 - NDrain

return(share)
}
