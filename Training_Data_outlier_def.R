# Maize competition
# training with trait ~ env, year, location, geno, soil, ec, meta (without outliers)

library(tidyverse)
library(data.table)
library(ranger)

FUN <- 'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/R_script_clean/fun_GenomPred.R'

source(FUN)

### load data --------

# raw Training
trait_train <- fread(data.table = F, 
                     'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/Training_Data-20221128T125754Z-001/Training_Data/1_Training_Trait_Data_2014_2021.csv')
meta_train <- fread(data.table = F,
                    'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/Training_Data-20221128T125754Z-001/Training_Data/2_Training_Meta_Data_2014_2021.csv')
soil_train <- fread(data.table = F, 
                    'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/Training_Data-20221128T125754Z-001/Training_Data/3_Training_Soil_Data_2015_2021.csv')
weather_train <-fread(data.table = F, 
                      'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/Training_Data-20221128T125754Z-001/Training_Data/4_Training_Weather_Data_2014_2021.csv')
EC_train <- fread(data.table = F, 
                  'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/Training_Data-20221128T125754Z-001/Training_Data/6_Training_EC_Data_2014_2021.csv')

# clean training
spats <- fread(data.table = F, 
               'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/Training_Data-20221128T125754Z-001/Training_Data/Training_SpATS.csv')
meta <- fread(data.table = F,
              'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/Training_Data-20221128T125754Z-001/Training_Data/train_meta.csv')
soil <- fread(data.table = F, 
              'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/Training_Data-20221128T125754Z-001/Training_Data/Train_Soil_Data.csv')
soil <- soil[,-(1)]
weather <-fread(data.table = F, 
                       'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/Training_Data-20221128T125754Z-001/Training_Data/train_weather.csv')
EC <- fread(data.table = F, 
            'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/Training_Data-20221128T125754Z-001/Training_Data/train_EC.csv')
geno <- fread(data.table = F, 
              'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/genotype/geno_no_9')
rownames(geno) <- geno[,1]
geno <- geno[,-(1)]

# Data_test
# raw testing
trait_test <- fread(data.table = F, 
                    'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/Testing_Data-20221128T125744Z-001/Testing_Data/1_Submission_Template_2022.csv')
meta_test <- fread(data.table = F,
                   'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/Testing_Data-20221128T125744Z-001/Testing_Data/2_Testing_Meta_Data_2022.csv')
soil_test <- fread(data.table = F, 
                   'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/Testing_Data-20221128T125744Z-001/Testing_Data/3_Testing_Soil_Data_2022.csv')
weather_test <-fread(data.table = F, 
                     'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/Testing_Data-20221128T125744Z-001/Testing_Data/4_Testing_Weather_Data_2022.csv')
EC_test <- fread(data.table = F, 
                 'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/Testing_Data-20221128T125744Z-001/Testing_Data/6_Testing_EC_Data_2022.csv')

# clean testing
t_meta <- fread(data.table = F,
                'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/Testing_Data-20221128T125744Z-001/Testing_Data/test_meta.csv')
t_soil <- fread(data.table = F, 
                'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/Testing_Data-20221128T125744Z-001/Testing_Data/test_soil.csv')
t_soil <- t_soil[,-(1)]
t_weather <-fread(data.table = F, 
                  'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/Testing_Data-20221128T125744Z-001/Testing_Data/test_weather.csv')
t_weather <- t_weather[,-(1)]
t_EC <- fread(data.table = F, 
              'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/Testing_Data-20221128T125744Z-001/Testing_Data/test_EC.csv')


boxplot(trait_train$Yield_Mg_ha)
hist(trait_train$Yield_Mg_ha, plot = T)

### prepare train data ---------

#merge all env data togheter
all_env <- list(weather,soil)      
all_env <- all_env %>% reduce(full_join, by='Env')
all_env <- janitor::clean_names(all_env)
all_env <- na.omit(all_env)

# select trait and env with same Env
trait_train <- trait_train%>%select(1:3, "Hybrid", "Yield_Mg_ha")%>%na.omit()
out <- boxplot.stats(trait_train$Yield_Mg_ha)$out # remove outliers
out_ind <- which(trait_train$Yield_Mg_ha %in% c(out))
out_ind
trait_train <- trait_train[-out_ind,]
trait_train <- trait_train[trait_train$Env%in%all_env$env,]
trait_train <- trait_train[trait_train$Hybrid%in%colnames(geno),]
# calculate GRM
geno <- geno[,colnames(geno)%in% trait_train$Hybrid |colnames(geno)%in% trait_test$Hybrid]
#geno <- geno[,colnames(geno)%in% rownames(VIM_800)]

GRM_train <- f.GRMcalc(geno)
# heatmap(GRM_train)
# GRM_train <- f.snp(geno, "UARadj")

# we combine in the same df the trait and the genetic and ev. the environmental information pro GIDs (set if they are factor or numeric)
# all_data <- as.data.frame(matrix(NA,nrow(trait_train),ncol(GRM_train)+ncol(trait_train)))
# colnames(all_data)[1:ncol(trait_train)] <- c("Env", "Year", "Location", "Hybrid", "TRAIT")
# colnames(all_data)[(1+ncol(trait_train)):ncol(all_data)] <- colnames(GRM_train)
# all_data$Hybrid <- trait_train$Hybrid
# all_data$TRAIT <- trait_train$Twt_kg_m3
# all_data$Env <- trait_train$Env
# all_data$Year <- trait_train$Year
# all_data$Location <- trait_train$Field_Location

# all_data_1 <- all_data_1[order(all_data_1$Hybrid),]
# methode in cannabis script
# for (NR in 1:nrow(all_data)){
#   print(NR)
#   singleGID <- all_data$Hybrid[NR]
#   all_data[(1+ncol(trait_train)):ncol(all_data)] <- GRM_train[rownames(GRM_train) == singleGID, ]
# }

# # "faster" method if you have replicates
# un_GID <- unique(all_data$Hybrid)
# for (i in 1:length(un_GID)) {
#   print(i)
#   all_data[match(un_GID[i],all_data$Hybrid),(1+ncol(trait_train)):ncol(all_data)] <- GRM_train[rownames(GRM_train)==un_GID[i],]
#   all_data[all_data$Hybrid == all_data$Hybrid[match(un_GID[i],all_data$Hybrid)], 6:ncol(all_data)] <- all_data[match(un_GID[i],all_data$Hybrid), 6:ncol(all_data)]
# }

# other method, way faster
GRM1 <- as.data.frame(GRM_train)
GRM1$Hybrid <- rownames(GRM1)
all_data_1 <- list(trait_train,GRM1)
all_data_1 <- all_data_1 %>% reduce(left_join, by = "Hybrid")
all_data_1 <- na.omit(all_data_1)
# check if by same GID we have same rows (geno info)
all_data_1 <- all_data_1[order(all_data_1$Hybrid),]

colnames(all_data_1)[1] <- "env"
all_env <- all_env[,-(c(1,9))] # remove v1 and year
all_data_2 <- list(all_data_1,all_env)
all_data_2 <- all_data_2 %>% reduce(left_join, by = "env")
any(is.na(all_data_2))
# all_data_2 <- na.omit(all_data_2)

# check if by same Env we have same rows (Env info)
# all_data_2 <- all_data_2[order(all_data_2$env),]

all_data_2 <- janitor::clean_names(all_data_2)
#all_data_2 <- all_data_2[,colnames(all_data_2)%in%rownames(VIM_800)|colnames(all_data_2)%in%c("env", "year", "field_location","twt_kg_m3")]

# all_data <- all_data[order(all_data$Env),]
# empty_env <- as.data.frame(matrix(NA, nrow = nrow(all_data), ncol = ncol(all_env)))
# colnames(empty_env) <- colnames(all_env)
# empty_env <- empty_env[,-(1)]
# all_data <- cbind(all_data,empty_env)
# 
# un_Env <- unique(all_data$Env)
# for (i in 1:length(un_Env)) {
#   print(i)
#   all_data[match(un_Env[i],all_data$Env),(1+ncol(trait_train)+ncol(GRM_train)):ncol(all_data)] <- all_env[all_env$Env==un_Env[i],2:ncol(all_env)]
#   all_data[all_data$Env == all_data$Env[match(un_Env[i],all_data$Env)], (1+ncol(trait_train)+ncol(GRM_train)):ncol(all_data)] <- all_data[match(un_Env[i],all_data$Env), (1+ncol(trait_train)+ncol(GRM_train)):ncol(all_data)]
# }

# set factor columns and remove hybrid
all_data_2$env <- as.factor(all_data_2$env)
all_data_2$year <- as.factor(all_data_2$year)
all_data_2$field_location <- as.factor(all_data_2$field_location)


#write.table(all_data_2, 'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/all_data__outliers.csv', sep = ",")
# all_data_2 <- fread(data.table = F,
#                     'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/all_data__outliers.csv')
all_data_2 <- all_data_2[,-(c(1,4))]
#all_data_2$hybrid <- as.factor(all_data_2$hybrid)
all_data_2 <- all_data_2[,-(which(colnames(all_data_2)=="loc"))] # remove it, we already have it
# all_data_2 <- all_data_2[,-(which(colnames(all_data_2)=="year"))] # remove it, if do not make sense

# all_data <- cbind(all_data, as.data.frame(matrix(NA, ncol = ncol(all_env), nrow=nrow(all_data))))
# for (NR in 1:nrow(all_data)){
#   print(NR)
#   singleEnv <- all_data$Env[NR]
#   all_data[(1+ncol(trait_train[,c("Hybrid", "Twt_kg_m3", "Env")])+ncol(GRM_train)):ncol(all_data)] <- all_env[all_env$Env == singleEnv, ]
# }

######## prepare data test --------

#merge all env data togheter

t_all_env <- list(t_weather,t_soil)      
t_all_env <- t_all_env %>% reduce(full_join, by='Env')
t_all_env <- janitor::clean_names(t_all_env)
t_all_env <- na.omit(t_all_env)

# add location column
trait_test$Field_Location <- rep(NA, nrow(trait_test))
for (i in 1:nrow(trait_test)) {
  trait_test$Field_Location[i] <- strsplit(as.character(trait_test$Env),"20")[[i]][1]
}

# add year column
trait_test$year <- 2022


# bind trait with geno informations
t_GRM1 <- as.data.frame(GRM_train)
t_GRM1$Hybrid <- rownames(t_GRM1)
t_all_data_1 <- list(trait_test,t_GRM1)
t_all_data_1 <- t_all_data_1 %>% reduce(left_join, by = "Hybrid")
# check if by same GID we have same rows (geno info)
# t_all_data_1 <- t_all_data_1[order(t_all_data_1$Hybrid),]


colnames(t_all_data_1)[1] <- "env"
t_all_env <- t_all_env[,-(8)] # remove year
t_all_data_2 <- list(t_all_data_1,t_all_env)
t_all_data_2 <- t_all_data_2 %>% reduce(left_join, by = "env")

# check if by same Env we have same rows (Env info)
# t_all_data_2 <- t_all_data_2[order(t_all_data_2$env),]

t_all_data_2 <- janitor::clean_names(t_all_data_2)
#all_data_2 <- all_data_2[,colnames(all_data_2)%in%rownames(VIM_800)|colnames(all_data_2)%in%c("env", "year", "field_location","twt_kg_m3")]

# all_data <- all_data[order(all_data$Env),]
# empty_env <- as.data.frame(matrix(NA, nrow = nrow(all_data), ncol = ncol(all_env)))
# colnames(empty_env) <- colnames(all_env)
# empty_env <- empty_env[,-(1)]
# all_data <- cbind(all_data,empty_env)
# 
# un_Env <- unique(all_data$Env)
# for (i in 1:length(un_Env)) {
#   print(i)
#   all_data[match(un_Env[i],all_data$Env),(1+ncol(trait_train)+ncol(GRM_train)):ncol(all_data)] <- all_env[all_env$Env==un_Env[i],2:ncol(all_env)]
#   all_data[all_data$Env == all_data$Env[match(un_Env[i],all_data$Env)], (1+ncol(trait_train)+ncol(GRM_train)):ncol(all_data)] <- all_data[match(un_Env[i],all_data$Env), (1+ncol(trait_train)+ncol(GRM_train)):ncol(all_data)]
# }

# set factor columns and remove hybrid
t_all_data_2$env <- as.factor(t_all_data_2$env)
t_all_data_2$year <- as.factor(t_all_data_2$year)
t_all_data_2$field_location <- as.factor(t_all_data_2$field_location)


#write.table(all_data_2, 'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/all_data__outliers.csv', sep = ",")
# all_data_2 <- fread(data.table = F,
#                     'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/all_data__outliers.csv')
# t_all_data_2 <- t_all_data_2[,-(c(1,2))]
t_all_data_2 <- t_all_data_2[,-(which(colnames(t_all_data_2)=="loc"))]
# t_all_data_2 <- t_all_data_2[,-(which(colnames(t_all_data_2)=="year"))]

#all_data_2$hybrid <- as.factor(all_data_2$hybrid)

# check if data_train(all_data_2) and data_test(t_all_data_2) have same column names
sum(colnames(t_all_data_2)%in%colnames(all_data_2)) # yes all 4954!
any(is.na(all_data_2))
### train the model ------------

# subset intro training
# set.seed(21)
# # train.Data <- sample(nrow(all_data_2), 2/3 * nrow(all_data_2))
# # Data_train <- all_data_2[train.Data, ]
# # Data_test <- all_data_2[-train.Data, ]
# 
# 
# importance <- 'permutation'
# mtry <- round(ncol(all_data_2)/3)
# num.trees <- 500
# replace <- F
# min.node.size <- 5
# 
# set.seed(21)
# RFmodel_out_w <- ranger(yield_mg_ha ~ ., data=all_data_2, 
#                       importance = importance,
#                       mtry = mtry,
#                       num.trees = num.trees,
#                       replace = replace,
#                       min.node.size = min.node.size,
#                       num.threads = 7)
# Sys.time()
# RFmodel_out_w$r.squared
# RFmodel_out_w$prediction.error
# head(data.frame(V1=sort(RFmodel_out_w$variable.importance, decreasing = TRUE)),20)
# 
# 
# 
# # ## prediction and validation of the single locations
# # res <- data.frame(loc=NA, RMSE=NA)
# # for (i in 1:length(unique(Data_test$field_location))) {
# #   print(paste(i," of ", length(unique(Data_test$field_location))))
# #   loc <- as.vector(unique(Data_test$field_location))[i]
# # pred.iris <- predict(RFmodel_out_w, data = Data_test[Data_test$field_location == loc,-(3)])
# # pred <- pred.iris$predictions
# # plot(pred~Data_test[Data_test$field_location == loc,3])
# # abline(lm(pred~Data_test[Data_test$field_location == loc,3]))
# # #summary(lm(pred~Data_test[Data_test$field_location == as.vector(unique(Data_test$field_location))[i],3]))
# # RMSE <- sqrt(mean((summary(lm(pred~Data_test[Data_test$field_location == loc,3]))$residuals)^2))
# # res <- rbind(res,c(loc,RMSE))
# # }
# # res$RMSE <- as.numeric(res$RMSE)
# # summary(res$RMSE)
# 
# 
# 
# ### predict -----------
# # predire con anno diverso da quelli usati nel modello
# # a <- all_data_2[1:100,]
# # a$year <- 2022
# # a$year <- as.factor(a$year)
# 
# 
# ## prediction and validation
# pred.iris <- predict(RFmodel_out_w, data = t_all_data_2[,-(c(1,2,3))])
# 
# t_all_data_2$yield_mg_ha <- pred.iris$predictions
# 
# submission <- t_all_data_2[,1:3]
# colnames(submission) <- c("Env", "Hybrid", "Yield_Mg_ha")
# 
# write.table(submission, 'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/submission_1_arulrich.csv',
#             sep = ",", col.names = T, row.names = F)




### same with XgBoost -----------
library(xgboost)
# do accept only matrix with only numeric variables
# here year and field location are factors
# prepare train data
all_data_2$year_nr <- as.numeric(all_data_2$year)# integer 1:8
all_data_2$field_location_nr <- as.numeric(all_data_2$field_location)
all_data_2$field_location <- as.character(all_data_2$field_location)
field_translate <- data.frame(loc=all_data_2$field_location, nr_loc=all_data_2$field_location_nr)%>%distinct() # to know which number was for which location

# num_cols <- unlist(lapply(all_data_2, is.numeric))         # Identify numeric columns
# num_cols
# data_num <- all_data_2[ , num_cols]                        # Subset numeric columns of data
# all_data_2 <- data_num

# prepare test data
t_all_data_2$year_nr <- 9
t_all_data_2$field_location_nr <- NA
t_all_data_2$field_location <- as.character(t_all_data_2$field_location)

for (i in 1:nrow(t_all_data_2)) {
  print(i)
  loc <- t_all_data_2$field_location[i]
  loc <- gsub("_", "", loc)
  t_all_data_2$field_location_nr[i] <- field_translate[field_translate$loc==loc, "nr_loc"]
}

# num_cols <- unlist(lapply(t_all_data_2[,-(c(1,2,3))], is.numeric))         # Identify numeric columns
# num_cols
# data_num <- t_all_data_2[ , num_cols]                        # Subset numeric columns of data
# t_all_data_2 <- data_num

# train the model
bstSparse <- xgboost(data = as.matrix(all_data_2[,4:ncol(all_data_2)]), label = all_data_2[,3], max_depth = 6, eta = 0.3,
                     nthread = 7, nrounds = 500, objective = "reg:squarederror")

# prediction
pred <- predict(bstSparse, as.matrix(t_all_data_2[,6:ncol(t_all_data_2)]))

t_all_data_2$yield_mg_ha <- pred

submission <- t_all_data_2[,1:3]
colnames(submission) <- c("Env", "Hybrid", "Yield_Mg_ha")

write.table(submission, 'C:/Users/Ulrich Argeo/OneDrive - Pure Production AG/Documents/ARGEO/Maize_Competition/submission_5_arulrich.csv',
            sep = ",", col.names = T, row.names = F)
