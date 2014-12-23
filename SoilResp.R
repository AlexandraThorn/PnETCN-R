SoilResp <- function() {
#
# PnET ecosystem model soil respiration routine.
#


#global veg site clim share rstep

share$SoilRespMo <- veg$SoilRespA * exp(veg$SoilRespB * (share$Tave + 0))
share$SoilRespMo <- share$SoilRespMo * share$MeanSoilMoistEff
share$SoilRespMo <- share$SoilRespMo * (share$dayspan[rstep] / 30.5)
share$SoilRespYr <- share$SoilRespYr + share$SoilRespMo

}
