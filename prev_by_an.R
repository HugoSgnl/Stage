### Using Code_Simulation program

## Purpose : calculation of the prevalence for each year between 2015 and 2035

# etats : matrix of 10,000 rows and 61 columns. With individuals in rows and ages 35-95 in columns implementing the different states of 0, 1, 2, and 3
# prevalence_by_an : matrix of 61 rows and 4 columns. With ages in rows and prevalence and number alive for each age in columns (for each year)


for(an in 2015:2035){                   # loop over the years
  
  print(an)
  
  int_min <- an - 95                    # calculation of the cohort interval: lower limit
  int_max <- an - 35                    # calculation of the cohort interval: upper limit
  
  prevalence_by_an <- data.frame(matrix(nrow=0,ncol=5))
  
  for(c in int_min:int_max){
    
    A <- an - c                         # calculation of the age of the individual born in c the year 'an'
    
    etats <- read.csv(paste("etats",c,sep="_"))
    etats$X <- NULL
    colnames(etats) <- gsub("X","",colnames(etats))
    
    nb = 0
    if(A>=36){
      for(i in 1:nrow(etats)){
        if(etats[i,A-34]==1 |etats[i,A-34]==3){         # calculation of the prevalence
          nb <- nb+1
        }
      }
      
      vivant <- 0
      for(i in 1:nrow(etats)){                          # calculation of the number of living people
        if(etats[i,A-34] == 1 | etats[i,A-34] == 0){
          vivant = vivant +1
        }
        if((etats[i,A-34] == 2 | etats[i,A-34]==3) & (etats[i,A-34-1]==1 | etats[i,A-34-1]==0)){
          vivant = vivant + 0.5
        }
      }
      ligne <- c(A,c,nb,vivant)
      
      prevalence_by_an <- rbind(prevalence_by_an,ligne)
    }
  }
  prevalence_by_an[,5] <- prevalence_by_an[,3]/prevalence_by_an[,4]*100 
  colnames(prevalence_by_an) <- c("age","cohorte","prevalence","nb_vivants","tx_prev")
  write.csv(prevalence_by_an,file=paste("prevalence_by_an",an,sep="_"))
}

#F

for(an in 2035:2035){                   # loop over the years
  
  print(an)
  
  int_min <- an - 95                    # calculation of the cohort interval: lower limit
  int_max <- an - 35                    # calculation of the cohort interval: upper limit
  
  prevalence_by_an <- data.frame(matrix(nrow=0,ncol=5))
  
  for(c in int_min:int_max){
    
    A <- an - c                         # calculation of the age of the individual born in c the year 'an'
    
    etats <- read.csv(paste("etats",c,sep="_"))
    etats$X <- NULL
    colnames(etats) <- gsub("X","",colnames(etats))
    
    matrice <- read.csv(paste("matrice",c,sep="_"))
    matrice$X <- NULL
    colnames(matrice) <- gsub("X","",colnames(matrice))
    
    nb = 0
    if(A>=36){
      for(i in 1:nrow(etats)){
        if((etats[i,A-34]==1 |etats[i,A-34]==3) & matrice[i,8] == 1){         # calculation of the prevalence
          nb <- nb+1
        }
      }
      
      vivant <- 0
      for(i in 1:nrow(etats)){                          # calculation of the number of living people
        if((etats[i,A-34] == 1 | etats[i,A-34] == 0) & matrice[i,8] == 1 ){
          vivant = vivant +1
        }
        if((etats[i,A-34] == 2 | etats[i,A-34]==3) & (etats[i,A-34-1]==1 | etats[i,A-34-1]==0) & matrice[i,8] == 1){
          vivant = vivant + 0.5
        }
      }
      ligne <- c(A,c,nb,vivant)
      
      prevalence_by_an <- rbind(prevalence_by_an,ligne)
    }
  }
  prevalence_by_an[,5] <- prevalence_by_an[,3]/prevalence_by_an[,4]*100 
  colnames(prevalence_by_an) <- c("age","cohorte","prevalence","nb_vivants","tx_prev")
  write.csv(prevalence_by_an,file=paste("prevalence_f_by_an",an,sep="_"))
}

#AF 

for(an in 2035:2035){                   # loop over the years
  
  print(an)
  
  int_min <- an - 95                    # calculation of the cohort interval: lower limit
  int_max <- an - 35                    # calculation of the cohort interval: upper limit
  
  prevalence_by_an <- data.frame(matrix(nrow=0,ncol=5))
  
  for(c in int_min:int_max){
    
    A <- an - c                         # calculation of the age of the individual born in c the year 'an'
    
    etats <- read.csv(paste("etats",c,sep="_"))
    etats$X <- NULL
    colnames(etats) <- gsub("X","",colnames(etats))
    
    matrice <- read.csv(paste("matrice",c,sep="_"))
    matrice$X <- NULL
    colnames(matrice) <- gsub("X","",colnames(matrice))
    
    nb = 0
    if(A>=36){
      for(i in 1:nrow(etats)){
        if((etats[i,A-34]==1 |etats[i,A-34]==3) & matrice[i,8] == 2){         # calculation of the prevalence
          nb <- nb+1
        }
      }
      
      vivant <- 0
      for(i in 1:nrow(etats)){                          # calculation of the number of living people
        if((etats[i,A-34] == 1 | etats[i,A-34] == 0) & matrice[i,8] == 2){
          vivant = vivant +1
        }
        if((etats[i,A-34] == 2 | etats[i,A-34]==3) & (etats[i,A-34-1]==1 | etats[i,A-34-1]==0) & matrice[i,8] == 2){
          vivant = vivant + 0.5
        }
      }
      ligne <- c(A,c,nb,vivant)
      
      prevalence_by_an <- rbind(prevalence_by_an,ligne)
    }
  }
  prevalence_by_an[,5] <- prevalence_by_an[,3]/prevalence_by_an[,4]*100 
  colnames(prevalence_by_an) <- c("age","cohorte","prevalence","nb_vivants","tx_prev")
  write.csv(prevalence_by_an,file=paste("prevalence_af_by_an",an,sep="_"))
}

#NF 

for(an in 2035:2035){                   # loop over the years
  
  print(an)
  
  int_min <- an - 95                    # calculation of the cohort interval: lower limit
  int_max <- an - 35                    # calculation of the cohort interval: upper limit
  
  prevalence_by_an <- data.frame(matrix(nrow=0,ncol=5))
  
  for(c in int_min:int_max){
    
    A <- an - c                         # calculation of the age of the individual born in c the year 'an'
    
    etats <- read.csv(paste("etats",c,sep="_"))
    etats$X <- NULL
    colnames(etats) <- gsub("X","",colnames(etats))
    
    matrice <- read.csv(paste("matrice",c,sep="_"))
    matrice$X <- NULL
    colnames(matrice) <- gsub("X","",colnames(matrice))
    
    nb = 0
    if(A>=36){
      for(i in 1:nrow(etats)){
        if((etats[i,A-34]==1 |etats[i,A-34]==3) & matrice[i,8] == 0){         # calculation of the prevalence
          nb <- nb+1
        }
      }
      
      vivant <- 0
      for(i in 1:nrow(etats)){                          # calculation of the number of living people
        if((etats[i,A-34] == 1 | etats[i,A-34] == 0) & matrice[i,8] == 0){
          vivant = vivant +1
        }
        if((etats[i,A-34] == 2 | etats[i,A-34]==3) & (etats[i,A-34-1]==1 | etats[i,A-34-1]==0) & matrice[i,8] == 0){
          vivant = vivant + 0.5
        }
      }
      ligne <- c(A,c,nb,vivant)
      
      prevalence_by_an <- rbind(prevalence_by_an,ligne)
    }
  }
  prevalence_by_an[,5] <- prevalence_by_an[,3]/prevalence_by_an[,4]*100 
  colnames(prevalence_by_an) <- c("age","cohorte","prevalence","nb_vivants","tx_prev")
  write.csv(prevalence_by_an,file=paste("prevalence_nf_by_an",an,sep="_"))
}




data_prev_by_an <- data.frame(matrix(c(0),ncol=21,nrow=60))
data_prev_by_an



for(an in 2015:2035){                                # loop over cohorts
  
  # rownames(esperance_vie_tabac) <- read.csv(paste("esperance_vie_f",c,sep="_"))[,2]
  data_prev_by_an[,an-2014] <- read.csv(paste("prevalence_by_an",an,sep="_"))[,4]
  
}
data_prev_by_an
rownames(data_prev_by_an) <- c(36:95)
colnames(data_prev_by_an) <- c(2015:2035)
data_prev_by_an

pop_2015_2035 <- read.csv2("tab_pop_homme_2015_2035.csv")
pop_2015_2035 <- pop_2015_2035[-1,-1]
pop_2015_2035


data <- data_prev_by_an/10000 * pop_2015_2035
data

somme_colonnes <- colSums(data)
somme_colonnes

