# Maize competition
# modification and imputation of weather data, if necessary

library(tidyverse)
library(data.table)

weather_train <-fread(data.table = F, 
                      'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/Training_Data-20221128T125754Z-001/Training_Data/4_Training_Weather_Data_2014_2021.csv')

str(weather_train) # 212 unique Env, 16 numeric variables without NAs, 1 column with the date
summary(weather_train)
any(is.na(weather_train))
unique(weather_train$Env)

weather_train$Date <- as.character(weather_train$Date)
# insert - after years and months
fun_insert <-  function(x, pos, insert) {       # Create own function
  gsub(paste0("^(.{", pos, "})(.*)$"),
       paste0("\\1", insert, "\\2"),
       x)
}

weather_train$Date <- fun_insert(x = weather_train$Date,pos = 4, 
                                 insert = "-")
weather_train$Date <- fun_insert(x = weather_train$Date,pos = 7, 
                                 insert = "-")
weather_train$Date <- as.Date(weather_train$Date, format = "%Y-%m-%d")

plot(weather_train$Date)
which.min(weather_train$Date)
date_sum <- tapply(weather_train$Date, weather_train$Env, summary)
date_length <- tapply(weather_train$Date, weather_train$Env, length)
# ==> daily measurement for the whole year

### summarise variables pro Env -----------
#=> thermal time(from seeding day) , cumulative precipitation (from ?)
# see script crop phenotyping

season_weather <- data.frame(prec_mean = rep(NA, length(unique(weather_train$Env))), prec_cum = rep(NA, length(unique(weather_train$Env))),
                             T_mean = rep(NA, length(unique(weather_train$Env))), TT = rep(NA, length(unique(weather_train$Env))),
                             VPD = rep(NA, length(unique(weather_train$Env))))
season_weather$Env <- unique(weather_train$Env)
season_weather <- season_weather[order(season_weather$Env),]

# yearly mean precipitations January- October
season_weather$prec_mean <- as.data.frame(tapply(weather_train$PRECTOTCORR[format(weather_train$Date,"%m")%in% c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10")],
                                                 weather_train$Env[format(weather_train$Date,"%m")%in% c("01", "02","03", "04", "05", "06", "07", "08", "09", "10")], mean))[,1]

# mean Temperature between March and October
season_weather$T_mean <- as.data.frame(tapply(weather_train$T2M[format(weather_train$Date,"%m")%in% c("03", "04", "05", "06", "07", "08", "09", "10")],
                                              weather_train$Env[format(weather_train$Date,"%m") %in% c("03", "04", "05", "06", "07", "08", "09", "10")], mean))[,1]

# yearly cumulative precipitation until october
res <- data.frame(Env=NA, cum=NA)
for (i in unique(weather_train$Env)) {
  a <- as.data.frame(weather_train$PRECTOTCORR[weather_train$Env==i & format(weather_train$Date,"%m")%in% c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10")])
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

season_weather$prec_cum <- res$cum[2:213]%>% as.numeric()


# growing degree days (GDD) or thermal time

# baseline temperature
T_base <- 5

res <- data.frame(Env=NA, cum=NA)
for (i in unique(weather_train$Env)) {
  a <- as.data.frame(weather_train[weather_train$Env==i & format(weather_train$Date,"%m")%in%c("03", "04", "05", "06", "07", "08", "09", "10"),"T2M"])
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

season_weather$TT <- res$cum[2:213]%>% as.numeric()

# relative humidity mean between march and october
season_weather$RH <- as.data.frame(tapply(weather_train$RH2M[format(weather_train$Date,"%m")%in% c("03", "04", "05", "06", "07", "08", "09", "10")],
                                              weather_train$Env[format(weather_train$Date,"%m") %in% c("03", "04", "05", "06", "07", "08", "09", "10")], mean))[,1]


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

weather_train$VPD <- RHtoVPD(weather_train$RH2M, weather_train$T2M)

# mean VPD between March and October
season_weather$VPD <- as.data.frame(tapply(weather_train$VPD[format(weather_train$Date,"%m")%in% c("03", "04", "05", "06", "07", "08", "09", "10")],
                                              weather_train$Env[format(weather_train$Date,"%m") %in% c("03", "04", "05", "06", "07", "08", "09", "10")], mean))[,1]


write.csv(season_weather, 'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/Training_Data-20221128T125754Z-001/Training_Data/train_weather.csv')
