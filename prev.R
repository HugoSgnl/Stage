### Using Code_Simulation program

## Purpose : calculation of the prevalence of myocardial infarction for the cohorts from 1920 to 2000 between 35 and 95 years old

# etats : matrix of 10,000 rows and 61 columns. With individuals in rows and ages 35-95 in columns implementing the different states of 0, 1, 2, and 3
# prev : matrix of 61 rows and 2 columns. With ages in rows and number of cases in columns (by cohort)
# vivants : matrix of 61 rows and 2 columns. With the ages in rows and the number of living in columns (by cohort)


## Package :

library(doParallel)
library(foreach)

cl <- makeCluster(20)
registerDoParallel(cl)


# Fonction générale :

foreach(c=1970:2000) %dopar% {                             # loop over cohorts
  
  etats <- read.csv(paste("etats",c,sep="_"))              # read 'matrice des états' for cohort c
  etats$X <- NULL
  colnames(etats) <- gsub("X","",colnames(etats))
  print(etats[2,75])
  
  
  prev <- data.frame(matrix(ncol=3,nrow=0))
  vivants <- data.frame(matrix(ncol=3,nrow=0))
  
  print(c)
  
  for(a in 2:ncol(etats)){                          
    nb = 0
    for(i in 1:nrow(etats)){                               # calculation of the number of patients (alive or deceased)
      if(etats[i,a]==1 |etats[i,a]==3){
        nb <- nb+1
      }
    }
    ligne <- c(a+34,c,nb)
    prev <- rbind(prev,ligne)
    
    vivant <- 0
    for(i in 1:nrow(etats)){                               # calculation of the number of people alive at age a (or dead at age a but alive at age a-1) for cohort c
      if(etats[i,a] == 1 | etats[i,a] == 0){
        vivant = vivant +1
      }
      if((etats[i,a] ==2 | etats[i,a]==3) & (etats[i,a-1]==1 | etats[i,a-1]==0)){
        vivant = vivant + 0.5
      }
    }
    ligne <- c(a+34,c,vivant)
    vivants <- rbind(vivants,ligne)
  }
  colnames(prev) <- c("age_maladie","cohorte","prevalence")
  prev$cohorte <- as.factor(prev$cohorte)
  colnames(vivants) <- c("age","cohorte","nb_vivants")
  write.csv(prev,file = paste("prev_h",c,sep="_"))
  write.csv(vivants,file=paste("vivants_h",c,sep="_"))
  PREV <- data.frame(matrix(ncol=4,nrow=80))
  colnames(PREV) <- c("gen", "f", "af", "nf")
  PREV[,1] <- prev$prevalence
  VIV <- data.frame(matrix(ncol=4,nrow=80))
  colnames(VIV) <- c("gen", "f", "af", "nf")
  VIV[,1] <- vivants[,3]
  
  print("f")
  
  etats <- read.csv(paste("etats",c,sep="_"))              # read 'matrice des états' for cohort c
  etats$X <- NULL
  colnames(etats) <- gsub("X","",colnames(etats))
  
  matrice <- read.csv(paste("matrice",c,sep="_"))              # read 'matrice des états' for cohort c
  matrice$X <- NULL
  colnames(matrice) <- gsub("X","",colnames(matrice))
  
  prev <- data.frame(matrix(ncol=3,nrow=0))
  vivants <- data.frame(matrix(ncol=3,nrow=0))
  
  
  
  for(a in 2:ncol(etats)){                          
    nb = 0
    for(i in 1:nrow(etats)){                               # calculation of the number of patients (alive or deceased)
      # if (matrice[i,7 ==1]){
      
      if(((etats[i,a]==1 |etats[i,a]==3)) & matrice[i,8] == 1){
        nb <- nb+1
      }
      
      # }
      
    }
    ligne <- c(a+34,c,nb)
    prev <- rbind(prev,ligne)
    
    vivant <- 0
    for(i in 1:nrow(etats)){                               # calculation of the number of people alive at age a (or dead at age a but alive at age a-1) for cohort c
      # if (matrice[i,5] == 1 | (matrice[i,5] == 2 & matrice[i,6] > 35)){
      if((etats[i,a] == 1 | etats[i,a] == 0) & (matrice[i,8] == 1 )){
        vivant = vivant +1
      }
      if((etats[i,a] ==2 | etats[i,a]==3) & (etats[i,a-1]==1 | etats[i,a-1]==0) & matrice[i,8] == 1){
        vivant = vivant + 0.5
      } 
    }
    
    # }
    ligne <- c(a+34,c,vivant)
    vivants <- rbind(vivants,ligne)
  }
  colnames(prev) <- c("age_maladie","cohorte","prevalence")
  prev$cohorte <- as.factor(prev$cohorte)
  colnames(vivants) <- c("age","cohorte","nb_vivants")
  write.csv(prev,file = paste("prev_h_f",c,sep="_"))
  write.csv(vivants,file=paste("vivants_h_f",c,sep="_"))
  PREV[,2] <- prev$prevalence
  VIV[,2] <- vivants[,3]
  
  
  print("nf")
  
  etats <- read.csv(paste("etats",c,sep="_"))              # read 'matrice des états' for cohort c
  etats$X <- NULL
  colnames(etats) <- gsub("X","",colnames(etats))
  
  matrice <- read.csv(paste("matrice",c,sep="_"))              # read 'matrice des états' for cohort c
  matrice$X <- NULL
  colnames(matrice) <- gsub("X","",colnames(matrice))
  
  prev <- data.frame(matrix(ncol=3,nrow=0))
  vivants <- data.frame(matrix(ncol=3,nrow=0))
  
  
  
  for(a in 2:ncol(etats)){                          
    nb = 0
    for(i in 1:nrow(etats)){                               # calculation of the number of patients (alive or deceased)
      if((etats[i,a]==1 |etats[i,a]==3) & matrice[i,5] == 0){
        nb <- nb+1
      }
    }
    ligne <- c(a+34,c,nb)
    prev <- rbind(prev,ligne)
    
    vivant <- 0
    for(i in 1:nrow(etats)){                               # calculation of the number of people alive at age a (or dead at age a but alive at age a-1) for cohort c
      if((etats[i,a] == 1 | etats[i,a] == 0) & matrice[i,5] == 0){
        vivant = vivant +1
      }
      if((etats[i,a] ==2 | etats[i,a]==3) & (etats[i,a-1]==1 | etats[i,a-1]==0) & matrice[i,5] == 0){
        vivant = vivant + 0.5
      }
    }
    ligne <- c(a+34,c,vivant)
    vivants <- rbind(vivants,ligne)
  }
  colnames(prev) <- c("age_maladie","cohorte","prevalence")
  prev$cohorte <- as.factor(prev$cohorte)
  colnames(vivants) <- c("age","cohorte","nb_vivants")
  write.csv(prev,file = paste("prev_h_nf",c,sep="_"))
  write.csv(vivants,file=paste("vivants_h_nf",c,sep="_"))
  PREV[,4] <- prev$prevalence
  VIV[,4] <- vivants[,3]
  # POURCENTAGEPREV <- data.frame(matrix(ncol=4,nrow=80))
  
  
  print("af")
  
  etats <- read.csv(paste("etats",c,sep="_"))              # read 'matrice des états' for cohort c
  etats$X <- NULL
  colnames(etats) <- gsub("X","",colnames(etats))
  
  matrice <- read.csv(paste("matrice",c,sep="_"))              # read 'matrice des états' for cohort c
  matrice$X <- NULL
  colnames(matrice) <- gsub("X","",colnames(matrice))
  
  prev <- data.frame(matrix(ncol=3,nrow=0))
  vivants <- data.frame(matrix(ncol=3,nrow=0))
  
  
  
  for(a in 2:ncol(etats)){                          
    nb = 0
    for(i in 1:nrow(etats)){                               # calculation of the number of patients (alive or deceased)
      if((etats[i,a]==1 |etats[i,a]==3) & matrice[i,8] == 2){
        nb <- nb+1
      }
    }
    ligne <- c(a+34,c,nb)
    prev <- rbind(prev,ligne)
    
    vivant <- 0
    for(i in 1:nrow(etats)){                               # calculation of the number of people alive at age a (or dead at age a but alive at age a-1) for cohort c
      if((etats[i,a] == 1 | etats[i,a] == 0) & matrice[i,8] == 2){
        vivant = vivant +1
      }
      if((etats[i,a] ==2 | etats[i,a]==3) & (etats[i,a-1]==1 | etats[i,a-1]==0) & matrice[i,8] == 2){
        vivant = vivant + 0.5
      }
    }
    ligne <- c(a+34,c,vivant)
    vivants <- rbind(vivants,ligne)
  }
  colnames(prev) <- c("age_maladie","cohorte","prevalence")
  prev$cohorte <- as.factor(prev$cohorte)
  colnames(vivants) <- c("age","cohorte","nb_vivants")
  write.csv(prev,file = paste("prev_h_af",c,sep="_"))
  write.csv(vivants,file=paste("vivants_h__af",c,sep="_"))
  # PREV[,3] <- PREV[,1] - (PREV[,2] + PREV[,4])
  # VIV[,3] <-  VIV[,1] - (VIV[,2] + VIV[,4]) 
  PREV[,3] <- prev$prevalence
  VIV[,3] <- vivants[,3]
  write.csv(PREV,file = paste("Prevalence_h",c,sep="_"))
  write.csv(VIV,file=paste("Vivants_h",c,sep="_"))
  }


PREV
VIV
plot(PREV[,2]/VIV[,2]*100, type = "l", col = "red", ylab = "Taux de prévalence (%)", xlab = "Âge", xlim = c(0,51), ylim =c(0,35) ,xaxt="n")
axis(1,at=c(0,10,20,30,40,50,60,70,80), labels=c(35,45,55,65,75,85,95,105,115)) 
lines(PREV[,3]/VIV[,3]*100, type = "l", col = "green")
lines(PREV[,1]/VIV[,1]*100, type = "l", col = "cyan")
lines(PREV[,4]/VIV[,4]*100, type = "l", col = "black")
legend(x="topleft", legend=c("Fumeurs","Anciens fumeurs", "Non fumeurs"), col=c("red","green", "black"), pch=20)

round(PREV[,2]/sum(matrice[,5] == 1)*100,3)

PREV
VIV


