library(ncdf4)
library(reshape2)
library(lubridate)


#set study area
lonmax <- -81 #top northern most coordinate
lonmin <- -98 #bott0m southern coordinate
latmax <- 30.5  #left eastern coordinate
latmin <- 18.5  #right western coordinate

# identify the variable you want to extract data for
var <- "chlor_a"


#list netCDF files
f <- list.files(pattern="_chlor",full.names=F, recursive = T) #What pattern can you use to identify all the netCDF files?

d <- plyr::adply(f, 1, function(file) {

  # open netCDF file
  data<-nc_open(file)

  # extract data
  lon<-ncvar_get(data,"lon")
  lat<-ncvar_get(data, "lat") # get the latitude
  tmp.array <- ncvar_get(data, data$var$chlor_a)
  dunits <- ncatt_get(data, var, "units")$value
  fillvalue <- ncatt_get(data, var, "_FillValue") #set the fill value for cells with no data

  dim(tmp.array)

  # remove the missing data
  tmp.array[tmp.array == fillvalue] <- NA

  #  matrix to data.frame
  dimnames(tmp.array)<-list(lon=lon,lat=lat)
  dat.var<-melt(tmp.array,id="lon")

  # select data from the study area
  dat.varSAtmp<-subset(dat.var, lon<=lonmax & lon>=lonmin & lat<=latmax & lat>=latmin)

  # extract date information
  dateini<-ncatt_get(data,0,"time_coverage_start")$value
  dateend<-ncatt_get(data,0,"time_coverage_end")$value
  datemean<-mean(c(as.Date(dateend,"%Y-%m-%dT%H:%M:%OSZ"),as.Date(dateini,"%Y-%m-%dT%H:%M:%OSZ")))
  year <-  year(datemean)# get the year
  month <- month(datemean) # get the month
  day <- day(datemean) # get the day (her code #year = substring(datemean, 0,4))

   # prepare final data set. Include the day (it is missing in the code below)
  dat.varSA<-data.frame(rep(as.integer(year,nrow(dat.varSAtmp))),
                        rep(as.integer(month,nrow(dat.varSAtmp))), 
                        rep(as.integer(day,nrow(dat.varSAtmp))),
                        dat.varSAtmp, 
                        rep(dunits,nrow(dat.varSAtmp)), 
                        rep(var, nrow(dat.varSAtmp)))
  
  
  names(dat.varSA)<-c("year","month","day","lon","lat","value","unit","var")

  # close connection
  nc_close(data)

  return(dat.varSA)

}, .progress="text", .inform = T)

d <- d[,-1]

# save csv file
write.csv(d, file = "Lab11_Exercise2.csv")
