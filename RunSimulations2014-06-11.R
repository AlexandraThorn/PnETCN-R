require(xts)
source("AmerifluxToClimateFile.R")
source("spinup_pnetcn.R")
source("pnetcn_NewPhenology.R")

#Must create directory "./Ameriflux/" containing a subdirectory for each
 # site (i.e. "Fermi","Konza","Freeman","Tonzi","Sevilleta","Heritage") with L2
 # AmeriFlux files for this code to run
 #
 # NOTE: AmeriFlux filenames depend on data version numbers

Tonzi_Ameriflux2014.05.08=AmerifluxImport(c("./Ameriflux/Tonzi/AMF_USTon_2001_L2_GF_V006.csv","./Ameriflux/Tonzi/AMF_USTon_2002_L2_GF_V006.csv","./Ameriflux/Tonzi/AMF_USTon_2003_L2_GF_V006.csv","./Ameriflux/Tonzi/AMF_USTon_2004_L2_GF_V006.csv","./Ameriflux/Tonzi/AMF_USTon_2005_L2_GF_V006.csv","./Ameriflux/Tonzi/AMF_USTon_2006_L2_GF_V006.csv","./Ameriflux/Tonzi/AMF_USTon_2007_L2_GF_V006.csv","./Ameriflux/Tonzi/AMF_USTon_2008_L2_GF_V006.csv","./Ameriflux/Tonzi/AMF_USTon_2009_L2_GF_V006.csv","./Ameriflux/Tonzi/AMF_USTon_2010_L2_GF_V006.csv","./Ameriflux/Tonzi/AMF_USTon_2011_L2_GF_V006.csv"))
# 
 
 #Note: for validation used L4 for Fermi; however, L2 used as input for all runs
 Konza_Ameriflux2014.06.11=AmerifluxImport(c("./Ameriflux/Konza/AMF_USKon_2006_L2_GF_V004.csv", "./Ameriflux/Konza/AMF_USKon_2007_L2_GF_V004.csv","./Ameriflux/Konza/AMF_USKon_2008_L2_GF_V004.csv","./Ameriflux/Konza/AMF_USKon_2009_L2_GF_V004.csv","./Ameriflux/Konza/AMF_USKon_2010_L2_GF_V004.csv","./Ameriflux/Konza/AMF_USKon_2011_L2_GF_V004.csv","./Ameriflux/Konza/AMF_USKon_2012_L2_GF_V004.csv"))
 Fermi_Ameriflux2014.06.11=AmerifluxImport(c("./Ameriflux/Fermi/AMF_USIB2_2004_L2_GF_V006.csv", "./Ameriflux/Fermi/AMF_USIB2_2005_L2_GF_V006.csv","./Ameriflux/Fermi/AMF_USIB2_2006_L2_GF_V006.csv","./Ameriflux/Fermi/AMF_USIB2_2007_L2_GF_V006.csv","./Ameriflux/Fermi/AMF_USIB2_2008_L2_GF_V006.csv","./Ameriflux/Fermi/AMF_USIB2_2009_L2_GF_V006.csv","./Ameriflux/Fermi/AMF_USIB2_2010_L2_GF_V006.csv","./Ameriflux/Fermi/AMF_USIB2_2011_L2_GF_V006.csv"))
 Sevilleta_Ameriflux2014.06.11=AmerifluxImport(c("./Ameriflux/Sevilleta/AMF_USSes_2007_L2_GF_V002.csv", "./Ameriflux/Sevilleta/AMF_USSes_2008_L2_GF_V002.csv","./Ameriflux/Sevilleta/AMF_USSes_2009_L2_GF_V002.csv","./Ameriflux/Sevilleta/AMF_USSes_2010_L2_GF_V002.csv"))
 Freeman_Ameriflux2014.06.11=AmerifluxImport(c("./Ameriflux/Freeman/AMF_USFR2_2005_L2_GF_V002.csv", "./Ameriflux/Freeman/AMF_USFR2_2006_L2_GF_V002.csv","./Ameriflux/Freeman/AMF_USFR2_2007_L2_GF_V002.csv","./Ameriflux/Freeman/AMF_USFR2_2008_L2_GF_V002.csv"))
 Pinyon_Ameriflux2014.06.11=AmerifluxImport(c("./Ameriflux/Heritage/AMF_USMpj_2008_L2_GF_V002.csv", "./Ameriflux/Heritage/AMF_USMpj_2009_L2_GF_V002.csv","./Ameriflux/Heritage/AMF_USMpj_2010_L2_GF_V002.csv"))

#Load site parameters
load("SiteAndVegParams.RData")

#Import Daymet climate files
#Must create directory "./Daymet/" containing a subdirectory for each
 # site (i.e. "Fermi","Konza","Freeman","Tonzi","Sevilleta","Heritage") with 
 # imported Daymet csv files named as below
 # source for Daymet files: <http://daymet.ornl.gov/singlepixel.html>
Tonzi_DayMet_Clim_xts=DaymetToClimxts(DayMetImport("./Daymet/Tonzi/11550_DAYMET_Tonzi.csv",sep="\t"))
Fermi_DayMet_Clim_xts=DaymetToClimxts(DayMetImport("./Daymet/Fermi/11746_DAYMET_Fermi.csv",sep="\t"))
FreemanRanchMesquite_DayMet_Clim_xts=DaymetToClimxts(DayMetImport("./Daymet/Freeman/10662_DAYMET_Freeman.csv",sep="\t"))
PinyonJuniper_DayMet_Clim_xts=DaymetToClimxts(DayMetImport("./Daymet/Heritage/11197_DAYMET_Heritage.csv",sep="\t"))
Konza_DayMet_Clim_xts=DaymetToClimxts(DayMetImport("./Daymet/Konza/11563_DAYMET_Konza.csv",sep="\t"))
Sevilleta_DayMet_Clim_xts=DaymetToClimxts(DayMetImport("./Daymet/Sevilleta/11197_DAYMET_Sevilleta.csv",sep="\t"))
 
#Data transformation and simulation runs
Tonzi_Ameriflux2014.05.08_Clim=AmerifluxToClimDF(Tonzi_Ameriflux2014.05.08)
Tonzi_Daymet_Clim_Transform2014.05.04_fitPAR_LeaveCO2NA = DayMetTransform_NoPrecipFit_LeaveCO2NA(Tonzi_DayMet_Clim_xts, Tonzi_Ameriflux2014.05.08_Clim)
Tonzi_Daymet_Clim_Transform2014.05.04_fitPAR_rep40_newCO2 = spinup_clim_calcCO2(Tonzi_Daymet_Clim_Transform2014.05.04_fitPAR_LeaveCO2NA,40)
TonziGrassTime=system.time({TonziGrass2014.05.04_newCO2=pnetcn_NewPhenology(Tonzi_Daymet_Clim_Transform2014.05.04_fitPAR_rep40_newCO2,tonzi_site, veg_tonzigrasspheno)})

Konza_Ameriflux2014.06.11_Clim=AmerifluxToClimDF(Konza_Ameriflux2014.06.11)
Konza_Daymet_Clim_Transform2014.06.11_fitPAR_LeaveCO2NA = DayMetTransform_NoPrecipFit_LeaveCO2NA(Konza_DayMet_Clim_xts, Konza_Ameriflux2014.06.11_Clim)
Konza_Daymet_Clim_Transform2014.06.11_fitPAR_rep40_newCO2 = spinup_clim_calcCO2(Konza_Daymet_Clim_Transform2014.06.11_fitPAR_LeaveCO2NA,40)
KonzaTime=system.time({Konza2014.06.11_newCO2=pnetcn_NewPhenology(Konza_Daymet_Clim_Transform2014.06.11_fitPAR_rep40_newCO2, konza_site, veg_konzapheno)})


Sevilleta_Ameriflux2014.06.11_Clim=AmerifluxToClimDF(Sevilleta_Ameriflux2014.06.11)
Sevilleta_Daymet_Clim_Transform2014.06.11_fitPAR_LeaveCO2NA = DayMetTransform_NoPrecipFit_LeaveCO2NA(Sevilleta_DayMet_Clim_xts, Sevilleta_Ameriflux2014.06.11_Clim)
Sevilleta_Daymet_Clim_Transform2014.06.11_fitPAR_rep40_newCO2 = spinup_clim_calcCO2(Sevilleta_Daymet_Clim_Transform2014.06.11_fitPAR_LeaveCO2NA,40)
SevilletaTime=system.time({Sevilleta2014.06.11_newCO2=pnetcn_NewPhenology(Sevilleta_Daymet_Clim_Transform2014.06.11_fitPAR_rep40_newCO2, sevilleta_site, veg_sevilletapheno)})


Freeman_Ameriflux2014.06.11_Clim=AmerifluxToClimDF(Freeman_Ameriflux2014.06.11)
Freeman_Daymet_Clim_Transform2014.06.11_fitPAR_LeaveCO2NA = DayMetTransform_NoPrecipFit_LeaveCO2NA(FreemanRanchMesquite_DayMet_Clim_xts, Freeman_Ameriflux2014.06.11_Clim)
Freeman_Daymet_Clim_Transform2014.06.11_fitPAR_rep40_newCO2 = spinup_clim_calcCO2(Freeman_Daymet_Clim_Transform2014.06.11_fitPAR_LeaveCO2NA,40)
FreemanWoodTime=system.time({FreemanWood2014.06.11_newCO2=pnetcn_NewPhenology(Freeman_Daymet_Clim_Transform2014.06.11_fitPAR_rep40_newCO2, freemanranchmesquite_site, veg_freemanwoodpheno)})
FreemanGrassTime=system.time({FreemanGrass2014.06.11_newCO2=pnetcn_NewPhenology(Freeman_Daymet_Clim_Transform2014.06.11_fitPAR_rep40_newCO2, freemanranchmesquite_site, veg_freemangrasspheno)})

Fermi_Ameriflux2014.06.11_Clim=AmerifluxToClimDF(Fermi_Ameriflux2014.06.11)
Fermi_Daymet_Clim_Transform2014.06.11_fitPAR_LeaveCO2NA = DayMetTransform_NoPrecipFit_LeaveCO2NA(Fermi_DayMet_Clim_xts, Fermi_Ameriflux2014.06.11_Clim)
Fermi_Daymet_Clim_Transform2014.06.11_fitPAR_rep40_newCO2 = spinup_clim_calcCO2(Fermi_Daymet_Clim_Transform2014.06.11_fitPAR_LeaveCO2NA,40)
FermiTime=system.time({Fermi2014.06.11_newCO2=pnetcn_NewPhenology(Fermi_Daymet_Clim_Transform2014.06.11_fitPAR_rep40_newCO2, fermi_site, veg_fermipheno)})

Pinyon_Ameriflux2014.06.11_Clim=AmerifluxToClimDF(Pinyon_Ameriflux2014.06.11)
Pinyon_Daymet_Clim_Transform2014.06.11_fitPAR_LeaveCO2NA = DayMetTransform_NoPrecipFit_LeaveCO2NA(PinyonJuniper_DayMet_Clim_xts, Pinyon_Ameriflux2014.06.11_Clim)
Pinyon_Daymet_Clim_Transform2014.06.11_fitPAR_rep40_newCO2 = spinup_clim_calcCO2(Pinyon_Daymet_Clim_Transform2014.06.11_fitPAR_LeaveCO2NA,40)
PinyonTime=system.time({Pinyon2014.06.11_newCO2=pnetcn_NewPhenology(Pinyon_Daymet_Clim_Transform2014.06.11_fitPAR_rep40_newCO2, pinyonjuniper_site, veg_pinyonpheno)})


TonziWoodTime=system.time({TonziWood2014.05.04_newCO2=pnetcn_NewPhenology(Tonzi_Daymet_Clim_Transform2014.05.04_fitPAR_rep40_newCO2,tonzi_site, veg_tonziwoodpheno)})



