# Maize competition
# modification and imputation of soil data

library(tidyverse)
library(data.table)

soil_train <- fread(data.table = F, 
                    'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/Training_Data-20221128T125754Z-001/Training_Data/3_Training_Soil_Data_2015_2021.csv')

### SOIL ----------------

str(soil_train)

# remove variables with too many NAs, more then 1/3
mask <- rep(NA, ncol(soil_train))
for (i in 1:ncol(soil_train)){
  mask[i] <- sum(is.na(soil_train[,i]))>0.3*nrow(soil_train)
}
soil_train <- soil_train[,!mask]

# aim: have soil data for the years 2014-2021 for each location!
# imputation
# create location column
soil_train$Loc <- rep(NA, nrow(soil_train))
for (i in 1:nrow(soil_train)) {
  soil_train$Loc[i] <- strsplit(as.character(soil_train$Env),"20")[[i]][1]
}


# create soil df for all the Env of trait_train and fill the data that we already have
soil_tot <- data.frame(matrix(NA, nrow = length(unique(trait_train$Env)), ncol = ncol(soil_train)))
colnames(soil_tot) <- colnames(soil_train)
soil_tot$Env <- unique(trait_train$Env)
for (i in 1:nrow(soil_tot)) {
  ENV <- soil_tot$Env[i]
  if (sum(soil_train$Env==ENV)==0){
    soil_tot[i,c(1,3:ncol(soil_tot))] <- NA}
  else {
  soil_tot[i,c(1,3:ncol(soil_tot))] <- soil_train[soil_train$Env==ENV,c(1,3:ncol(soil_train))]
  }
}


# set location and year column
soil_tot$Loc <- rep(NA, nrow(soil_tot))
for (i in 1:nrow(soil_tot)) {
  soil_tot$Loc[i] <- strsplit(as.character(soil_tot$Env),"20")[[i]][1]
}
soil_tot$Year <- rep(NA, nrow(soil_tot))
for (i in 1:nrow(soil_tot)) {
  soil_tot$Year[i] <- paste("20",strsplit(as.character(soil_tot$Env),"20")[[i]][2],sep = "")
  if (soil_tot$Year[i]=="20"){
    soil_tot$Year[i] <- paste("20",soil_tot$Year[i], sep = "")
    }
} 

# have a mean for each location and variable
loc_means <- aggregate(soil_train[,6:28], list(soil_train$Loc), FUN=function(x){mean(x, na.rm = TRUE)})

# I want to fill out each NAs in the table through mean values from loc_means

# We have those three location that might be the same
# c("IAH1a_","IAH1b_","IAH1c_") # they get all the data of loc: IAH1
soil_tot[soil_tot$Loc%in%c("IAH1a_","IAH1b_","IAH1c_"),6:28] <- loc_means[loc_means$Group.1=="IAH1_",2:24]
# same as above with c("TXH1-Dry_","TXH1-Early_","TXH1-Late_")
soil_tot[soil_tot$Loc%in%c("TXH1-Dry_","TXH1-Early_","TXH1-Late_"),6:28] <- loc_means[loc_means$Group.1=="TXH1_",2:24]

# fill NAs
for (nc in 6:28) {
  for (nr in c(1:162,164:nrow(soil_tot))){ # there are no soil records for the location TXH4_!!
    if (is.na(soil_tot[nr,nc])){
      LOC <- soil_tot$Loc[nr]
      VAR <- colnames(soil_tot)[nc]
      soil_tot[nr,nc] <- loc_means[loc_means$Group.1==LOC, VAR]
    } else {
      soil_tot[nr,nc] <- soil_tot[nr,nc]
    }
  }
}

# substitute NaN with colmean
for (nc in 6:28) {
  for (nr in c(1:162,164:nrow(soil_tot))){ # there are no records for the location TXH4_!!
    if (is.nan(soil_tot[nr,nc])){
      VAR <- colnames(soil_tot)[nc]
      soil_tot[nr,nc] <- mean(loc_means[,VAR], na.rm = TRUE)
    } else {
      soil_tot[nr,nc] <- soil_tot[nr,nc]
    }
  }
}

# set factors in soil_train
for (j in 1:5){
  i <- c("Year", "Env", "LabID", "Texture", "Loc")
  soil_train[,i[j]] <- as.factor(soil_train[,i[j]])
}

# set factors in soil_tot
for (j in 1:5){
  i <- c("Year", "Env", "LabID", "Texture", "Loc")
  soil_tot[,i[j]] <- as.factor(soil_tot[,i[j]])
}


# # remove variable which are not agronomically important, not important or makes no sense
remove <- c( 3,4,5, 29,30)
soil_tot <- soil_tot[, - (remove)]

# # remove NAs
 soil_tot <- na.omit(soil_tot)

# # save the df
write.csv(soil_tot, 'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/Training_Data-20221128T125754Z-001/Training_Data/Train_Soil_Data.csv')
