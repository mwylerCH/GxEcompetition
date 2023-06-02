# Maize competition
# training with trait ~ env, year, location, geno, soil (without outliers)

library(tidyverse)
library(data.table)
library(ranger)

FUN <- 'fun_GenomPred.R'

source(FUN)

### load data --------

# raw Training
trait_train <- fread(data.table = F, 
                     'Training_Data-20221128T125754Z-001/Training_Data/1_Training_Trait_Data_2014_2021.csv')
soil_train <- fread(data.table = F, 
                    'Training_Data-20221128T125754Z-001/Training_Data/3_Training_Soil_Data_2015_2021.csv')
weather_train <-fread(data.table = F, 
                      'Training_Data-20221128T125754Z-001/Training_Data/4_Training_Weather_Data_2014_2021.csv')

# clean training
soil <- fread(data.table = F, 
              'Training_Data-20221128T125754Z-001/Training_Data/Train_Soil_Data.csv')
soil <- soil[,-(1)]
weather <-fread(data.table = F, 
                       'Training_Data-20221128T125754Z-001/Training_Data/train_weather.csv')
geno <- fread(data.table = F, 
              'genotype/geno_no_9')
rownames(geno) <- geno[,1]
geno <- geno[,-(1)]

# Data_test
# raw testing
trait_test <- fread(data.table = F, 
                    'Testing_Data-20221128T125744Z-001/Testing_Data/1_Submission_Template_2022.csv')
soil_test <- fread(data.table = F, 
                   'Testing_Data-20221128T125744Z-001/Testing_Data/3_Testing_Soil_Data_2022.csv')
weather_test <-fread(data.table = F, 
                     'Testing_Data-20221128T125744Z-001/Testing_Data/4_Testing_Weather_Data_2022.csv')

# clean testing
_soil <- fread(data.table = F, 
                'Testing_Data-20221128T125744Z-001/Testing_Data/test_soil.csv')
t_soil <- t_soil[,-(1)]
t_weather <-fread(data.table = F, 
                  'Testing_Data-20221128T125744Z-001/Testing_Data/test_weather.csv')
t_weather <- t_weather[,-(1)]


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
GRM_train <- f.GRMcalc(geno)
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


all_data_2 <- all_data_2[,-(c(1,4))]
all_data_2 <- all_data_2[,-(which(colnames(all_data_2)=="loc"))] # remove it, we already have it
# all_data_2 <- all_data_2[,-(which(colnames(all_data_2)=="year"))] # remove it, if do not make sense

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

# set factor columns and remove hybrid
t_all_data_2$env <- as.factor(t_all_data_2$env)
t_all_data_2$year <- as.factor(t_all_data_2$year)
t_all_data_2$field_location <- as.factor(t_all_data_2$field_location)

t_all_data_2 <- t_all_data_2[,-(which(colnames(t_all_data_2)=="loc"))]
# t_all_data_2 <- t_all_data_2[,-(which(colnames(t_all_data_2)=="year"))]

#all_data_2$hybrid <- as.factor(all_data_2$hybrid)

# check if data_train(all_data_2) and data_test(t_all_data_2) have same column names
sum(colnames(t_all_data_2)%in%colnames(all_data_2)) # yes all 4954!
any(is.na(all_data_2))


### train the model ------------

# subset intro training
set.seed(21)
importance <- 'permutation'
mtry <- round(ncol(all_data_2)/3)
num.trees <- 500
replace <- F
min.node.size <- 5

set.seed(21)
RFmodel_out_w <- ranger(yield_mg_ha ~ ., data=all_data_2,
                      importance = importance,
                      mtry = mtry,
                      num.trees = num.trees,
                      replace = replace,
                      min.node.size = min.node.size,
                      num.threads = 7)

### predict -----------

## prediction and validation
pred.iris <- predict(RFmodel_out_w, data = t_all_data_2[,-(c(1,2,3))])

t_all_data_2$yield_mg_ha <- pred.iris$predictions

submission <- t_all_data_2[,1:3]
colnames(submission) <- c("Env", "Hybrid", "Yield_Mg_ha")

write.table(submission, 'submission_1.csv',
            sep = ",", col.names = T, row.names = F)


