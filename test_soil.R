# Maize competition
# modification and imputation of soil data

library(tidyverse)
library(data.table)

soil_test <- fread(data.table = F, 
                    'Testing_Data-20221128T125744Z-001/Testing_Data/3_Testing_Soil_Data_2022.csv')
soil_train <- fread(data.table = F, 
                   'Training_Data-20221128T125754Z-001/Training_Data/Train_Soil_Data.csv')
test_trait <- fread(data.table = F, 
                    'Testing_Data-20221128T125744Z-001/Testing_Data/1_Submission_Template_2022.csv')


### SOIL ----------------

# aim: have soil data for the years 2022 for each location!
# imputation
# create location column
soil_test$Loc <- rep(NA, nrow(soil_test))
for (i in 1:nrow(soil_test)) {
  soil_test$Loc[i] <- strsplit(as.character(soil_test$Env),"20")[[i]][1]
}

# same column as soil_train
soil_test <- soil_test[,colnames(soil_test)%in%colnames(soil_train)]

# create soil df for all the Env of test_trait and fill the data that we already have
soil_tot <- data.frame(matrix(NA, nrow = length(unique(test_trait$Env)), ncol = ncol(soil_test)))
colnames(soil_tot) <- colnames(soil_test)
soil_tot$Env <- unique(test_trait$Env)
for (i in 1:nrow(soil_tot)) {
  ENV <- soil_tot$Env[i]
  if (sum(soil_test$Env==ENV)==0){
    soil_tot[i,c(1,3:ncol(soil_tot))] <- NA}
  else {
  soil_tot[i,c(1,3:ncol(soil_tot))] <- soil_test[soil_test$Env==ENV,c(1,3:ncol(soil_test))]
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

# have a mean for each location and variable 2014-2021
loc_means <- aggregate(soil_train[,4:26], list(soil_train$Loc), FUN=function(x){mean(x, na.rm = TRUE)})

# I want to fill out each NAs in the table through mean values from loc_means

# We have those three location that might be the same
# c("IAH1a_","IAH1b_","IAH1c_") # they get all the data of loc: IAH1
soil_tot[soil_tot$Loc%in%c("IAH1a_","IAH1b_","IAH1c_"),6:28] <- loc_means[loc_means$Group.1=="IAH1_",2:24]
# same as above with c("TXH1-Dry_","TXH1-Early_","TXH1-Late_")
soil_tot[soil_tot$Loc%in%c("TXH1-Dry_","TXH1-Early_","TXH1-Late_"),6:28] <- loc_means[loc_means$Group.1=="TXH1_",2:24]

# fill NAs
for (nc in 3:25) {
  for (nr in c(1:nrow(soil_tot))){ 
    if (is.na(soil_tot[nr,nc])){
      LOC <- soil_tot$Loc[nr]
      VAR <- colnames(soil_tot)[nc]
      soil_tot[nr,nc] <- loc_means[loc_means$Group.1==LOC, VAR]
    } else {
      soil_tot[nr,nc] <- soil_tot[nr,nc]
    }
  }
}


# set factors in soil_test
for (j in 1:5){
  i <- c("Year", "Env", "LabID", "Texture", "Loc")
  soil_test[,i[j]] <- as.factor(soil_test[,i[j]])
}

# set factors in soil_tot
for (j in 1:3){
  i <- c("Year", "Env", "Loc")
  soil_tot[,i[j]] <- as.factor(soil_tot[,i[j]])
}



# # remove NAs
 soil_tot <- na.omit(soil_tot)

# # save the df
write.csv(soil_tot, 'Testing_Data-20221128T125744Z-001/Testing_Data/test_soil.csv')
