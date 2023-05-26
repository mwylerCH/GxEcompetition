### RF functions ------------------------

## from letters (AA, AT, TT, ...) to numbers (1, 0, -1)
f.matrix101 <- function(x){
  for (NR in (2:ncol(x))){
    print(paste0('switch ', colnames(x)[NR]))
    allNucl <- paste0(x[, NR], collapse = '')
    # define major/minor
    countNucl <- as.data.frame(matrix(ncol = 2, nrow = 4))
    countNucl$V1 <- c('A', 'C', 'T', 'G')
    for (nucNR in (1:4)){
      NUCLEOTIDE <- countNucl$V1[nucNR]
      countNucl[countNucl$V1 == NUCLEOTIDE, 2] <- str_count(allNucl, pattern = NUCLEOTIDE)
    }
    #countNucl <- arrange(countNucl, V2)
    countNucl <- countNucl[order(countNucl$V2),] # sort faster
    MAJOR <- countNucl$V1[4]
    MINOR <- countNucl$V1[3]
    # substitute
    x[, NR] <- gsub(paste0(MAJOR, MAJOR), -1, x[, NR])
    x[, NR] <- gsub(paste0(MAJOR, MINOR), 0, x[, NR])
    x[, NR] <- gsub(paste0(MINOR, MAJOR), 0, x[, NR])
    x[, NR] <- gsub(paste0(MINOR, MINOR), 1, x[, NR])
  }
  return(x)
}


## plot about variable importance
f.varImp <- function(model, max = NULL, category = T){
  GeneExpRanger <- model
  MAX <- max
  if (isTRUE(category)){
    COSA <- 'prediction.error'
    # extract  % Var explained by model
    variation <- round(GeneExpRanger[[COSA]], digits = 2)
    variation <- 100*variation
    variation <-paste('Error  ', variation, ' %')
  }else{
    COSA <- 'r.squared'
    # extract  % Var explained by model
    variation <- round(GeneExpRanger[[COSA]], digits = 2)
    variation <-paste('R-squared  ', variation)
  }
  # extract dataframe for plotting
  varData <- GeneExpRanger$variable.importance %>% as.data.frame()
  colnames(varData)[1] <- 'Importance'
  
  # extract  % Var explained by model
  library(ggthemes)
  varData %>%
    as.data.frame() %>% 
    rownames_to_column( "variabile") %>% 
    arrange(desc(Importance)) %>% head(n = MAX) %>% 
    mutate(variabile = factor(variabile, levels = variabile)) %>% 
    ggplot(aes(x= Importance , y=reorder(variabile, desc(variabile)))) +
    geom_point()+
    geom_segment(aes(y=variabile,yend=variabile,x=0,xend=Importance), linetype = 3) +
    labs(y='', x='', subtitle = variation)+  
    theme_tufte(base_family = 'liberation Sans') + 
    theme(axis.text= element_text(color='black'),
          legend.position =  'none',
          plot.subtitle = element_text(size = 10, face='italic'))
  #RFplot+ labs(title = 'Variable Importance \nGene Expression Athal')+ theme(axis.text.y = element_text(size=6))
}


### function for Kinship calculation -------------------------

library(data.table)
library(tidyverse)


### GRM Van Raden 

# input: df GIDs x Markers, values -1, 0, 1
# output: df GIDs x GIDs, numeric values

f.GRMcalc <- function(genoTable){
  # test if is a -1,0,1 matrix
  if (!-1 %in% unique(unlist(genoTable[1,]))){
    stop('Error: matrix need to be -1, 0, 1 coded')
  }
  genoTable <- na.omit(genoTable)
  p <- rowMeans((genoTable+1), na.rm = T)/2 # allelic frequency
  P <- 2*(p-0.5)
  W <- genoTable-P
  # ZtZ <- t(Z) %*% W
  W <- as.matrix(W)
  W <- na.omit(W)
  ZtZ <- Rfast::mat.mult(t(W), W)
  d <- 2*sum(p*(1-p))
  G <- ZtZ/d
  rownames(G) <- colnames(genoTable)
  colnames(G) <- colnames(genoTable)
  return(G)
}



### Matrix distances dist() 

# input: df GIDs x Markers, values -1, 0, 1
# method <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
# output: # output: df GIDs x GIDs, numeric values

f.dist <- function(genoTable, method){
  # test if is a -1,0,1 matrix
  if (!-1 %in% unique(unlist(genoTable[1,]))){
    stop('Error: matrix need to be -1, 0, 1 coded')
  }
  genoTable <- na.omit(genoTable)
  d <- dist(t(genoTable), method = method)
  m_d <- as.matrix(d)
  return(m_d)
}



### PCA 

# input: df GIDs x Markers, values -1, 0, 1
# explained variance from the component v
# method <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
# output: df GIDs x GIDs, numeric values

f.PCA <- function(genoTable, v = 0.9, method = "euclidean"){
  # test if is a -1,0,1 matrix
  if (!-1 %in% unique(unlist(genoTable[1,]))){
    stop('Error: matrix need to be -1, 0, 1 coded')
  }
  genoTable <- na.omit(genoTable)
  my_pca <- prcomp(genoTable)
  my_pca.var <- my_pca$sdev ^ 2
  # Proportion of variance explained from the components
  propve <- my_pca.var / sum(my_pca.var)
  tresh <- which(cumsum(propve) >= v)[1]
  a <- dist(my_pca$rotation[,1:tresh], method = method)
  return(as.matrix(a))
}



### package synbreed 

# Example from GS_Chickpea.pdf
# I looked at the code of the function kin()(Package synbreed) and then I took the part about the calculation of kinship
# because it is difficult/impossible to go into the package formats (kin function!)

# input: df GIDs x Markers, values -1, 0, 1
# method <- c("sm", "sm-smin") => only for homozygous inbred lines
# output: # output: df GIDs x GIDs, numeric values


f.sm <- function(genoTable, method){
  # test if is a -1,0,1 matrix
  if (!-1 %in% unique(unlist(genoTable[1,]))){
    stop('Error: matrix need to be -1, 0, 1 coded')
  }
  genoTable <- na.omit(genoTable)
  genoTable <- genoTable+1
  marker <- t(genoTable)
  marker <- marker - (max(marker, na.rm = TRUE) - 1)
  m <- ncol(marker)
  s <- (tcrossprod(marker) + m)/(2 * m)
  if (method == "sm"){
    return(2*s)
  }
  if (method == "sm-smin") {
    smin <- min(s, na.rm = TRUE)
    s <- (s - smin)/(1 - smin)
    return(2*s)  }
}


### package snpReady

# input: df GIDs x Markers, values -1, 0, 1
# method <- c("VanRaden", "UAR", "UARadj", "GK")
# output: df GIDs x GIDs, numeric values

library(snpReady)
f.snp <- function(genoTable, method){
  # test if is a -1,0,1 matrix
  if (!-1 %in% unique(unlist(genoTable[1,]))){
    stop('Error: matrix need to be -1, 0, 1 coded')
  }
  genoTable <- na.omit(genoTable)
  genoTable <- t(genoTable+1)
  
  # remove monomorphic markers
  mask <- apply(genoTable,2, function(x){as.matrix(summary(x))[1]!=as.matrix(summary(x))[6]})
  genoTable <- genoTable[,mask]
  
  GRM <- G.matrix(genoTable, method=method, format="wide",
                  plot = FALSE)
  return(GRM)
}


### gBLUP functions --------------

# calculate W
f.Wcalc <- function(genoTable){ # the -1, 0, 1 matrix is scaled depending on the allele frequencies
  # test if is a -1,0,1 matrix
  if (!-1 %in% unique(unlist(genoTable[1,]))){
    stop('Error: matrix need to be -1, 0, 1 coded')
  }
  p <- rowMeans((genoTable+1), na.rm = T)/2
  P <- 2*(p-0.5)# deviation from 0.5
  W <- genoTable-P
  W <- as.matrix(W)
  return(W)
}


# calculate d
f.dcalc <- function(genoTable){
  # test if is a -1,0,1 matrix
  if (!-1 %in% unique(unlist(genoTable[1,]))){
    stop('Error: matrix need to be -1, 0, 1 coded')
  }
  p <- rowMeans((genoTable+1), na.rm = T)/2
  # P <- 2*(p-0.5)
  # W <- genoTable-P
  # ZtZ <- t(Z) %*% W
  # W <- as.matrix(W)
  #Z <- na.omit(Z)
  # WtW <- Rfast::mat.mult(t(W), W)
  d <- 2*sum(p*(1-p))
  return(d)
}



### Outliers function --------------
# 
# # remove outliers from a df of a specific trait
# # it is not perfect working this function, needs improvemente
# f.outl <- function(df,trait){
#   if (!length(grep('GID', colnames(df))) == 1){
#     stop('No GID column found')
#   }else{
#     # identify dup GID
#     
#     # subset table only duplicated
#     
#     # calcola sui subset DF
#     df$z <- ave(df[,trait], df$GID, FUN = scale)
#     z_out <- which(df$z >= 3.5 | df$z <= -3.5)
#     df <-  df[-(z_out), ]
#     df$z <- NULL
#     # combine tables (uniq + subset)
#     
#     return(df)
#   }
# }
# 
# f.outl(tunnelLong, 'conc_THCA')
# f.outl(tunnelRaw, 'conc_THCA')
# f.outl(testino, 'conc_THCA')

