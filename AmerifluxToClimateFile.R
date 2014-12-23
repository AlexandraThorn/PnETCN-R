require(xts)
source('FluxDataFunctions.R') #source for ameriflux_posixt

DayMetTransform_NoPrecipFit_LeaveCO2NA <- function(DaymetClimxts,AmerifluxClimxts)
{
  filemerge0=merge.xts(DaymetClimxts,AmerifluxClimxts)
  filemerge=aggregate(filemerge0,as.yearmon(index(filemerge0)),function(x) mean(na.omit(x))) #aggregation to combine on monthly basis
  newclim=data.frame(filemerge$year, filemerge$doy, filemerge$tmin.1, filemerge$tmax.1, filemerge$par.1, filemerge$prec.1, filemerge$O3, filemerge$CO2.1, filemerge$NH4dep, filemerge$NO3dep)
  #automatic variants - fill in final years if not included in DAYMET
  names(newclim)=c("year","doy","tmin","tmax","par","prec","O3","CO2","NH4dep","NO3dep")
  #print(names(newclim))
  #print(newclim$year)
  newclim$year[ is.na(filemerge$year)]= filemerge$year.1[ is.na(filemerge$year)]
  #print(newclim$year)
  newclim$doy[ is.na(filemerge$doy)]= filemerge$doy.1[ is.na(filemerge$doy)]
  newclim$O3[ is.na(filemerge$O3)]= filemerge$O3.1[ is.na(filemerge$O3)]
  newclim$NH4dep[ is.na(filemerge$NH4dep)]= filemerge$NH4dep.1[ is.na(filemerge$NH4dep)]
  newclim$NO3dep[ is.na(filemerge$NO3dep)]= filemerge$NO3dep.1[ is.na(filemerge$NO3dep)]

  fit=lm(filemerge$tmin.1~filemerge$tmin)
  fitcoeff=as.numeric(coefficients(fit)) #just give values of coefficients
  newclim$tmin[is.na(newclim$tmin)]=filemerge$tmin[is.na(newclim$tmin)]*fitcoeff[2]+fitcoeff[1]
  #newclim$tmin[is.na(test$tmin)]=filemerge$tmin[is.na(test$tmin)]*fitcoeff[2]+fitcoeff[1]
  #newclim$tmin=filemerge$tmin*fitcoeff[2]+fitcoeff[1]

  fit=lm(filemerge$tmax.1~filemerge$tmax)
  fitcoeff=as.numeric(coefficients(fit)) #just give values of coefficients
  newclim$tmax[is.na(newclim$tmax)]=filemerge$tmax[is.na(newclim$tmax)]*fitcoeff[2]+fitcoeff[1]

  fit=lm(filemerge$par.1~filemerge$par)
  fitcoeff=as.numeric(coefficients(fit)) #just give values of coefficients
  newclim$par[is.na(newclim$par)]=filemerge$par[is.na(newclim$par)]*fitcoeff[2]+fitcoeff[1]

  #fit=lm(filemerge$prec.1~filemerge$prec)
  #fitcoeff=as.numeric(coefficients(fit)) #just give values of coefficients
  newclim$prec[is.na(newclim$prec)]=filemerge$prec[is.na(newclim$prec)]


  return(newclim)
  #return(filemerge)
}

DayMetTransform_NoPrecipFit <- function(DaymetClimxts,AmerifluxClimxts)
{
  filemerge0=merge.xts(DaymetClimxts,AmerifluxClimxts)
  filemerge=aggregate(filemerge0,as.yearmon(index(filemerge0)),function(x) mean(na.omit(x))) #aggregation to combine on monthly basis
  newclim=data.frame(filemerge$year, filemerge$doy, filemerge$tmin.1, filemerge$tmax.1, filemerge$par.1, filemerge$prec.1, filemerge$O3, filemerge$CO2.1, filemerge$NH4dep, filemerge$NO3dep)
  names(newclim)=c("year","doy","tmin","tmax","par","prec","O3","CO2","NH4dep","NO3dep")

  fit=lm(filemerge$tmin.1~filemerge$tmin)
  fitcoeff=as.numeric(coefficients(fit)) #just give values of coefficients
  newclim$tmin[is.na(newclim$tmin)]=filemerge$tmin[is.na(newclim$tmin)]*fitcoeff[2]+fitcoeff[1]
  #newclim$tmin[is.na(test$tmin)]=filemerge$tmin[is.na(test$tmin)]*fitcoeff[2]+fitcoeff[1]
  #newclim$tmin=filemerge$tmin*fitcoeff[2]+fitcoeff[1]

  fit=lm(filemerge$tmax.1~filemerge$tmax)
  fitcoeff=as.numeric(coefficients(fit)) #just give values of coefficients
  newclim$tmax[is.na(newclim$tmax)]=filemerge$tmax[is.na(newclim$tmax)]*fitcoeff[2]+fitcoeff[1]

  fit=lm(filemerge$par.1~filemerge$par)
  fitcoeff=as.numeric(coefficients(fit)) #just give values of coefficients
  newclim$par[is.na(newclim$par)]=filemerge$par[is.na(newclim$par)]*fitcoeff[2]+fitcoeff[1]

  #fit=lm(filemerge$prec.1~filemerge$prec)
  #fitcoeff=as.numeric(coefficients(fit)) #just give values of coefficients
  newclim$prec[is.na(newclim$prec)]=filemerge$prec[is.na(newclim$prec)]

  newclim$CO2[is.na(newclim$CO2)]=filemerge$CO2[is.na(newclim$CO2)]

  return(newclim)
  #return(filemerge)
}

DayMetTransform <- function(DaymetClimxts,AmerifluxClimxts)
{
  filemerge0=merge.xts(DaymetClimxts,AmerifluxClimxts)
  filemerge=aggregate(filemerge0,as.yearmon(index(filemerge0)),function(x) mean(na.omit(x))) #aggregation to combine on monthly basis
  newclim=data.frame(filemerge$year, filemerge$doy, filemerge$tmin.1, filemerge$tmax.1, filemerge$par.1, filemerge$prec.1, filemerge$O3, filemerge$CO2.1, filemerge$NH4dep, filemerge$NO3dep)
  names(newclim)=c("year","doy","tmin","tmax","par","prec","O3","CO2","NH4dep","NO3dep")

  fit=lm(filemerge$tmin.1~filemerge$tmin)
  fitcoeff=as.numeric(coefficients(fit)) #just give values of coefficients
  newclim$tmin[is.na(newclim$tmin)]=filemerge$tmin[is.na(newclim$tmin)]*fitcoeff[2]+fitcoeff[1]
  #newclim$tmin[is.na(test$tmin)]=filemerge$tmin[is.na(test$tmin)]*fitcoeff[2]+fitcoeff[1]
  #newclim$tmin=filemerge$tmin*fitcoeff[2]+fitcoeff[1]

  fit=lm(filemerge$tmax.1~filemerge$tmax)
  fitcoeff=as.numeric(coefficients(fit)) #just give values of coefficients
  newclim$tmax[is.na(newclim$tmax)]=filemerge$tmax[is.na(newclim$tmax)]*fitcoeff[2]+fitcoeff[1]

  fit=lm(filemerge$par.1~filemerge$par)
  fitcoeff=as.numeric(coefficients(fit)) #just give values of coefficients
  newclim$par[is.na(newclim$par)]=filemerge$par[is.na(newclim$par)]*fitcoeff[2]+fitcoeff[1]

  fit=lm(filemerge$prec.1~filemerge$prec)
  fitcoeff=as.numeric(coefficients(fit)) #just give values of coefficients
  newclim$prec[is.na(newclim$prec)]=filemerge$prec[is.na(newclim$prec)]*fitcoeff[2]+fitcoeff[1]

  newclim$CO2[is.na(newclim$CO2)]=filemerge$CO2[is.na(newclim$CO2)]

  return(newclim)
  #return(filemerge)
}

DayMetImport <-function(filename, linesbeforeheader=6, linesbeforedata=7, nullvaluelist=c(),sep=",")
{
  return (AmerifluxImport(c(filename),linesbeforeheader=linesbeforeheader,linesbeforedata=linesbeforedata,nullvaluelist=nullvaluelist,sep=sep))
}

AmerifluxImport <- function(filenamelist, linesbeforeheader=17, linesbeforedata=20, nullvaluelist=c(-9999,-6999),sep=",", L2_L4="L2")
{
  filename=filenamelist[1]
  fileheader=read.csv(filename,skip=linesbeforeheader,nrows=1,header=FALSE,strip.white=TRUE,sep=sep)
  alldata=read.csv(filename,skip=linesbeforedata,header=FALSE,sep=sep)
  names(alldata)=as.matrix(fileheader)
  if (L2_L4=="L4")
  {
    alldata$YEAR=as.numeric(substr(filename,6,9))
  }
  if (length(filenamelist)>1)
  {
    for (filename in filenamelist[2:length(filenamelist)]){
      fileheader=read.csv(filename,skip=linesbeforeheader,nrows=1,header=FALSE,strip.white=TRUE,sep=sep)
      newdata=read.csv(filename,skip=linesbeforedata,header=FALSE,sep=sep)
      names(newdata)=as.matrix(fileheader)
      if (L2_L4=="L4")
      {
        newdata$YEAR=as.numeric(substr(filename,6,9))
      }
      #alldata=merge(alldata,newdata,all=TRUE,sort=FALSE)
      alldata=rbind(alldata,newdata)
    }
    if(length(alldata$PAR)<1){
      alldata$PAR=alldata$PAR_in
    }
    if(length(alldata$PREC)<1){
      alldata$PREC=alldata$PRECIP
    }
  }
  alldata[alldata==-9999 | alldata==-6999]<-NA #NEED TO FIX
  print("Finished AmerifluxImport.")
  return(alldata)
}

DaymetToClimDF <-function(daymetdf)
{
  time.POSIXt=strptime(paste(daymetdf$year,daymetdf$yday,sep="-"), format="%Y-%j")
  daymetdf$par=daymetdf$"dayl (s)"*daymetdf$"srad (W/m^2)"/(0.21*60*60*24)
  daymet_xts=xts(daymetdf,time.POSIXt)
  temp_monthly_min_xts=apply.monthly(daymet_xts$"tmin (deg c)",mean)
  temp_monthly_max_xts=apply.monthly(daymet_xts$"tmax (deg c)",mean)
  par_monthly_xts=apply.monthly(daymet_xts$par,mean)
  prec_monthly_xts=apply.monthly(daymet_xts$"prcp (mm/day)",sum)/10
  daymet_xts_monthly=merge.xts(temp_monthly_min_xts,temp_monthly_max_xts,par_monthly_xts,prec_monthly_xts)
  names(daymet_xts_monthly)=c("tmin","tmax","par","prec")

  daymet_xts_monthly$O3=0 # ozone defaults to 0
  daymet_xts_monthly$CO2=350 # CO2 defaults to 0
  daymet_xts_monthly$NH4dep=0 # ammonium deposition defaults to 0
  daymet_xts_monthly$NO3dep=0 # nitrate deposition defaults to 0
  daymet_xts_monthly$year=as.numeric(format(index(daymet_xts_monthly),"%Y"))
  daymet_xts_monthly$doy=as.numeric(format(index(daymet_xts_monthly),"%j"))

  return(as.data.frame(daymet_xts_monthly))
}

DaymetToClimxts <-function(daymetdf)
{
  time.POSIXt=strptime(paste(daymetdf$year,daymetdf$yday,sep="-"), format="%Y-%j")
  daymetdf$par=daymetdf$"dayl (s)"*daymetdf$"srad (W/m^2)"/(0.21*60*60*24)
  daymet_xts=xts(daymetdf,time.POSIXt)
  temp_monthly_min_xts=apply.monthly(daymet_xts$"tmin (deg c)",mean)
  temp_monthly_max_xts=apply.monthly(daymet_xts$"tmax (deg c)",mean)
  par_monthly_xts=apply.monthly(daymet_xts$par,mean)
  prec_monthly_xts=apply.monthly(daymet_xts$"prcp (mm/day)",sum)/10
  daymet_xts_monthly=merge.xts(temp_monthly_min_xts,temp_monthly_max_xts,par_monthly_xts,prec_monthly_xts)
  names(daymet_xts_monthly)=c("tmin","tmax","par","prec")

  daymet_xts_monthly$O3=0 # ozone defaults to 0
  daymet_xts_monthly$CO2=350 # CO2 defaults to 0
  daymet_xts_monthly$NH4dep=0 # ammonium deposition defaults to 0
  daymet_xts_monthly$NO3dep=0 # nitrate deposition defaults to 0
  daymet_xts_monthly$year=as.numeric(format(index(daymet_xts_monthly),"%Y"))
  daymet_xts_monthly$doy=as.numeric(format(index(daymet_xts_monthly),"%j"))

  return(daymet_xts_monthly)
}

AmerifluxToClimDF_Daily <- function(amerifluxdf,L2_L4="L2",PAR_threshold=1)
{
  #edited data.frame
  newdf=ameriflux_posixt(amerifluxdf)
  newdf$PAR_daylight=newdf$PAR
  newdf$PAR_daylight[newdf$PAR_daylight<PAR_threshold]<-NA #exclude night values

  #daily temperature max and min
  temp_xts=xts(newdf$TA,newdf$time.POSIXt)
  temp_daily_min_xts=apply.daily(temp_xts,min)
  temp_daily_max_xts=apply.daily(temp_xts,max)
  climdf_daily=merge.xts(temp_daily_min_xts,temp_daily_max_xts)
  names(climdf_daily)=c("tmin","tmax")
  climdf_daily$par=apply.daily(na.omit(xts(newdf$PAR_daylight,newdf$time.POSIXt)),mean)
  climdf_daily$prec=apply.daily(na.omit(xts(newdf$PREC,newdf$time.POSIXt)),sum)/10 #add up and divide by 10 to convert to cm (from mm)
  climdf_daily$O3=0 # ozone defaults to 0
  CO2_xts=na.omit(xts(newdf$CO2,newdf$time.POSIXt))
  if(length(CO2_xts)>0){
    climdf_daily$CO2=apply.daily(na.omit(xts(newdf$CO2,newdf$time.POSIXt)),mean) # CO2 as 24 hour mean (might alternatively use daylight mean)
  }else{
    climdf_daily$CO2=350 # CO2 as 24 hour mean (might alternatively use daylight mean)
  }
  climdf_daily$NH4dep=0 # ammonium deposition defaults to 0
  climdf_daily$NO3dep=0 # nitrate deposition defaults to 0
  climdf_daily$year=as.numeric(format(index(climdf_daily),"%Y"))
  climdf_daily$doy=as.numeric(format(index(climdf_daily),"%j"))
  climdf_daily=aggregate(climdf_daily,as.Date(index(climdf_daily)),function(x) mean(na.omit(x))) #aggregation to combine on daily basis
  return(climdf_daily)
  #return(subset(climdf_monthly,select=c("Year","doy","tmax","tmin","par","prec","O3","CO2","NH4dep","NO3dep")))
}

AmerifluxToClimDF <- function(amerifluxdf,L2_L4="L2",PAR_threshold=1)
{
  #edited data.frame
  newdf=ameriflux_posixt(amerifluxdf)
  newdf$PAR_daylight=newdf$PAR
  newdf$PAR_daylight[newdf$PAR_daylight<PAR_threshold]<-NA #exclude night values

  #daily temperature max and min
  temp_xts=xts(newdf$TA,newdf$time.POSIXt)
  temp_daily_min_xts=apply.daily(temp_xts,min)
  temp_daily_max_xts=apply.daily(temp_xts,max)
  climdf_daily=merge.xts(temp_daily_min_xts,temp_daily_max_xts)
  names(climdf_daily)=c("tmin","tmax")

  #monthly values
  climdf_monthly=apply.monthly(climdf_daily,mean) #temperature max and min
  climdf_monthly$par=apply.monthly(na.omit(xts(newdf$PAR_daylight,newdf$time.POSIXt)),mean)
  climdf_monthly$prec=apply.monthly(na.omit(xts(newdf$PREC,newdf$time.POSIXt)),sum)/10 #add up and divide by 10 to convert to cm (from mm)
  climdf_monthly$O3=0 # ozone defaults to 0
  CO2_xts=na.omit(xts(newdf$CO2,newdf$time.POSIXt))
  if(length(CO2_xts)>0){
    climdf_monthly$CO2=apply.monthly(na.omit(xts(newdf$CO2,newdf$time.POSIXt)),mean) # CO2 as 24 hour mean (might alternatively use daylight mean)
  }else{
    climdf_monthly$CO2=350 # CO2 as 24 hour mean (might alternatively use daylight mean)
  }
  climdf_monthly$NH4dep=0 # ammonium deposition defaults to 0
  climdf_monthly$NO3dep=0 # nitrate deposition defaults to 0
  climdf_monthly=aggregate(climdf_monthly,as.yearmon(index(climdf_monthly)),function(x) mean(na.omit(x))) #aggregation to combine on monthly basis
  climdf_monthly$year=as.numeric(format(index(climdf_monthly),"%Y"))
  climdf_monthly$doy=as.numeric(format(index(climdf_monthly),"%j"))
  return(climdf_monthly)
  #return(subset(climdf_monthly,select=c("Year","doy","tmax","tmin","par","prec","O3","CO2","NH4dep","NO3dep")))
}
