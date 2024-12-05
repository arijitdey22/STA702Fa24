library(ncdf4)
library(RNetCDF)

###########################################################
#set the working directory where the nc files are
###########################################################

rm(list = ls())

####getting the pixels outside of India
ncin <- nc_open("RFone_imd_rf_1x1_2022.nc")

lon.vals <- ncin$dim[[1]]$vals
lat.vals <- ncin$dim[[2]]$vals
rain.vals <- ncvar_get(ncin, ncin$var[[1]])

jan2may.leap <- 31 + 29 + 31 + 30 + 31
jan2may.nonleap <- 31 + 28 + 31 + 30 + 31
JJAS.days <- 30 + 31 + 31 + 30

JJAS.leap <- (jan2may.leap + 1):(jan2may.leap + JJAS.days)
JJAS.nonleap <- (jan2may.nonleap + 1):(jan2may.nonleap + JJAS.days)

JJAS.rain.vals <- c(apply(rain.vals[ , , JJAS.nonleap], c(1, 2), sum)) #, na.rm = T

loc <- as.matrix(expand.grid(x = lon.vals, y = lat.vals))
keep <- which(!is.na(JJAS.rain.vals))
S <- loc[keep, ]

####data refining for all years

years <- 1901:2022
all.data <- matrix(0, nrow = length(years), ncol = dim(S)[1])

for(t in 1:length(years)){
  fname <- paste0("RFone_imd_rf_1x1_", years[t],".nc")
  ncin <- nc_open(fname)
  rain.vals <- ncvar_get(ncin, ncin$var[[1]])
  
  if(years[t] %% 4 == 0){
    curr.year <- matrix(NA, length(keep), 366)
    
    temp <- matrix(NA, length(lat.vals) * length(lon.vals), 366)
    for (j in 1:33)
    {
      temp[   ((j-1)*35 + 1):(j*35)  , ] <- rain.vals[, j, ]
    }
    
    all.data[t, ] <- rowSums(temp[keep, ])
  }else{
    curr.year <- matrix(NA, length(keep), 365)
    
    temp <- matrix(NA, length(lat.vals) * length(lon.vals), 365)
    for (j in 1:33)
    {
      temp[   ((j-1)*35 + 1):(j*35)  , ] <- rain.vals[, j, ]
    }
    
    all.data[t, ] <- rowSums(temp[keep, ])
  }
  
  print("--------------------------------------")
  print(paste0("We are at year ", years[t],"."))
  rm(ncin)
}

# --
# replacing 4 negative rainfall values with 0
ind.less.0 <- which(all.data < 0, arr.ind = T)
all.data[ind.less.0] <- 0
# --

save(all.data, S, file = "Monsoon_rain_imd.Rdata")
