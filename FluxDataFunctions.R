require(xts)

vemap_whc <- function(vemap,lat,long)
{
  return(0.1*vemap[floor(1+(49-lat)/0.5),floor(1+(124.5-abs(long))/0.5)])
}

ameriflux_keyvals_monthly <-function(df) #string
{
  myData=ameriflux_posixt(df)
  data_keyvals=data.frame(myData$NEE,myData$RE,myData$GPP)
  data_keyvals_monthly=dataframe_monthly(data_keyvals,myData$time.POSIXt)
  return(data_keyvals_monthly)
}

ameriflux_keyvals_monthly_fromfile <-function(filename) #string
{
  myData=read.csv(filename)
  myData=ameriflux_posixt(myData)
  data_keyvals=data.frame(myData$NEE,myData$RE,myData$GPP)
  data_keyvals_monthly=dataframe_monthly(data_keyvals,myData$time.POSIXt)
  return(data_keyvals_monthly)
}

ameriflux_posixt <- function(amerifluxdf)
{
  myData=amerifluxdf
  myData$hour=floor(myData$HRMIN/100)
  myData$min=myData$HRMIN-myData$hour*100
  myData$time.POSIXt=strptime(paste(myData$YEAR,myData$DOY,myData$hour,myData$min,sep="-"),format="%Y-%j-%H-%M")
  return(myData)
}

dataframe_monthly <-function(df,posixtime)
{
  monthly=data.frame(sapply(df,function(x) data_monthly_xts(x,posixtime)))
  monthly$time.POSIXt=index(data_monthly_xts(df[,1],posixtime))
  return(monthly)
}

data_monthly_xts <- function(data, #input data column
                           posixtime, #POSIXt=strptime(paste(y,doy,h,min,
                                      #                       sep="-"),
                                      #                format="%Y-%j-%H-%M")
                           inputtimestep=60*30, #half-hourly data
                           inputtimeunits=1.0, #per second
                           outputtimeunits=60.0*60.0*24.0*365.0, #per year
                           inputcarbonunits=12.0*10^(-6), #umols C (per gram)
                           outputcarbonunits=1.0) #g (per gram)
{
  data[data==-9999]<-NA
  perseconddata=data/inputtimeunits #note: inputtimeunits=sec per time unit
  halfhourdata=perseconddata*inputtimestep #times 60s/min * 30s/half hour
  outunitshalfhourdata=halfhourdata*inputcarbonunits/outputcarbonunits
  outdata_xts=xts(outunitshalfhourdata,posixtime)
  #outdata_xts2=na.omit(outdata_xts)
  outdata_daily_xts=apply.daily(outdata_xts,sum)
  outdata_monthly_xts=apply.monthly(outdata_daily_xts,sum)
  return (outdata_monthly_xts)
}


#plot(climdf$time.POSIXt,pnet_out_monthly$grosspsn,xlim=c(as.numeric(ISOdate(2007,1,1)),as.numeric(ISOdate(2013,1,1))),type="l",ylim=c(-100,600))
#points(index(na.omit(data_monthly_xts(key_fluxes$fluxes.GPP_gapfill,fluxes$time.POSIXt))),na.omit(data_monthly_xts(key_fluxes$fluxes.GPP_gapfill,fluxes$time.POSIXt)),col="red")
#points(index(na.omit(data_monthly_xts(key_fluxes$fluxes.GPP,fluxes$time.POSIXt))),-na.omit(data_monthly_xts(key_fluxes$fluxes.GPP,fluxes$time.POSIXt)))
