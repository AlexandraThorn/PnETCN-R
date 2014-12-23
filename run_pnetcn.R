source("pnetcn.R")

pnetcn_monthly <- function(clim,site,veg)
{
  #assumes 118 columns of monthly data, followed by 99 columns yearly data
  output=pnetcn(clim,site,veg)
  output_monthly=data.frame(output[names(output)[1:118]])
  return output_monthly
}

pnetcn_yearly <- function(clim,site,veg)
{
  #assumes 118 columns of monthly data, followed by 99 columns yearly data
  output=pnetcn(clim,site,veg)
  output_yearly=data.frame(output[names(output)[119:218]])
  return output_monthly
}

