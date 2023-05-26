# Maize competition
# modification and imputation of weather data, if necessary

library(tidyverse)
library(data.table)

weather_test <-fread(data.table = F, 
                      'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/Testing_Data-20221128T125744Z-001/Testing_Data/4_Testing_Weather_Data_2022.csv')

str(weather_test) # 26 unique Env, 16 numeric variables without NAs, 1 column with the date
summary(weather_test)
any(is.na(weather_test))
unique(weather_test$Env)

weather_test$Date <- as.character(weather_test$Date)
# insert - after years and months
fun_insert <-  function(x, pos, insert) {       # Create own function
  gsub(paste0("^(.{", pos, "})(.*)$"),
       paste0("\\1", insert, "\\2"),
       x)
}

weather_test$Date <- fun_insert(x = weather_test$Date,pos = 4, 
                                 insert = "-")
weather_test$Date <- fun_insert(x = weather_test$Date,pos = 7, 
                                 insert = "-")
weather_test$Date <- as.Date(weather_test$Date, format = "%Y-%m-%d")

weather_test <- weather_test[format(weather_test$Date,"%m")%in% c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10"),]

any(is.na(weather_test))
summary(weather_test)
weather_test <- weather_test[,-(c(6, 11,12, 15, 17))] # remove col with too many NAs


date_sum <- tapply(weather_test$Date, weather_test$Env, summary)
date_length <- tapply(weather_test$Date, weather_test$Env, length)
# ==> daily measurement for the whole year

### summarise variables pro Env -----------
#=> thermal time(from seeding day) , cumulative precipitation (from ?)
# see script crop phenotyping

season_weather <- data.frame(prec_mean = rep(NA, length(unique(weather_test$Env))), prec_cum = rep(NA, length(unique(weather_test$Env))),
                             T_mean = rep(NA, length(unique(weather_test$Env))), TT = rep(NA, length(unique(weather_test$Env))),
                             VPD = rep(NA, length(unique(weather_test$Env))))
season_weather$Env <- unique(weather_test$Env)
season_weather <- season_weather[order(season_weather$Env),]

# yearly mean precipitations January October
season_weather$prec_mean <- as.data.frame(tapply(weather_test$PRECTOTCORR, weather_test$Env, mean))[,1]

# mean Temperature between March and October
season_weather$T_mean <- as.data.frame(tapply(weather_test$T2M[format(weather_test$Date,"%m")%in% c("03", "04", "05", "06", "07", "08", "09", "10")],
                                              weather_test$Env[format(weather_test$Date,"%m") %in% c("03", "04", "05", "06", "07", "08", "09", "10")], mean))[,1]

# yearly cumulative precipitation
res <- data.frame(Env=NA, cum=NA)
for (i in unique(weather_test$Env)) {
  a <- as.data.frame(weather_test$PRECTOTCORR[weather_test$Env==i])
  colnames(a) <- "PRECTOTCORR"
  for (j in 2:nrow(a)) {
    if (j==2){
      a$cum <- rep(NA, nrow(a))
      a$cum[1] <- a$PRECTOTCORR[1]
      a$cum[j] <- a$cum[j-1] + a$PRECTOTCORR[j]
    } else {
      a$cum[j] <- a$cum[j-1] + a$PRECTOTCORR[j]
    }
  }
  res <- rbind(res,c(i,a$cum[nrow(a)]))
}

season_weather$prec_cum <- res$cum[2:27]%>% as.numeric()


# growing degree days (GDD) or thermal time

# baseline temperature
T_base <- 5

res <- data.frame(Env=NA, cum=NA)
for (i in unique(weather_test$Env)) {
  a <- as.data.frame(weather_test[weather_test$Env==i & format(weather_test$Date,"%m")%in%c("03", "04", "05", "06", "07", "08", "09", "10"),"T2M"])
  colnames(a) <- "T2M"
  a$cum <- rep(NA, nrow(a))
  if(a[1,"T2M"]<=T_base){
    a$cum[1] <- 0
  } else {
    a$cum[1] <- a$T2M[1]-T_base
  }
  for (j in 2:nrow(a)) {
      if(a[j,"T2M"]<=T_base){
        a$cum[j] <- a$cum[j-1]
      } else {
        a$cum[j] <- a$cum[j-1] + a[j,"T2M"]- T_base
      }
    }
  res <- rbind(res,c(i,a$cum[nrow(a)]))
}

season_weather$TT <- res$cum[2:27]%>% as.numeric()

# relative humidity mean between march and october
season_weather$RH <- as.data.frame(tapply(weather_test$RH2M[format(weather_test$Date,"%m")%in% c("03", "04", "05", "06", "07", "08", "09", "10")],
                                              weather_test$Env[format(weather_test$Date,"%m") %in% c("03", "04", "05", "06", "07", "08", "09", "10")], mean))[,1]


## VPD 
library(plantecophys)
?plantecophys
RHtoVPD

#functions derived from plantecophys
#function to calculate the vapor pressure deficit
RHtoVPD<-function (RH, TdegC, Pa = 101) 
  {
    esatval <- esat(TdegC, Pa)
    e <- (RH/100) * esatval
    VPD <- (esatval - e)/1000
    return(VPD)
  }

weather_test$VPD <- RHtoVPD(weather_test$RH2M, weather_test$T2M)

# mean VPD between March and October
season_weather$VPD <- as.data.frame(tapply(weather_test$VPD[format(weather_test$Date,"%m")%in% c("03", "04", "05", "06", "07", "08", "09", "10")],
                                              weather_test$Env[format(weather_test$Date,"%m") %in% c("03", "04", "05", "06", "07", "08", "09", "10")], mean))[,1]


write.csv(season_weather, 'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/Testing_Data-20221128T125744Z-001/Testing_Data/test_weather.csv')
