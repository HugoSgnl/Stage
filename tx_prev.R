### Using Code_Simulation and Code_Prevalence programs

## Purpose : compute age-specific prevalence rates for each cohort

# etats : matrix of 10,000 rows and 61 columns. With individuals in rows and ages 35-95 in columns implementing the different states of 0, 1, 2, and 3
# prev_H : matrix of 61 rows and 2 columns. With ages in rows and number of cases in columns (by cohort)
# vivants : matrix of 61 rows and 2 columns. With the ages in rows and the number of living in columns (by cohort)
# taux_prev : matrix of 61 rows and 3 columns. With row and column ages the reference cohort and prevalence rates for each age (by cohort)


## Package :
library(doParallel)
library(foreach)

cl <- makeCluster(20)
registerDoParallel(cl)


foreach(c=1920:2000) %do% {                             # loop over cohorts
  
  print("gen")
  taux_prev <- data.frame(matrix(ncol=3,nrow=0))
  
  prev <- read.csv(paste("prev",c,sep="_"))                # read the prevalence matrix for cohort c
  prev$X <- NULL
  colnames(prev) <- gsub("X","",colnames(prev))
  
  vivants <- read.csv(paste("vivants",c,sep="_"))          # read matrix of alive for cohort c
  vivants$X <- NULL
  colnames(vivants) <- gsub("X","",colnames(vivants))
  
  for(a in 1:nrow(vivants)){                               # compute age-specific prevalence rate for cohort c
    taux_prev[a,1] <- a+34
    taux_prev[a,2] <- c
    taux_prev[a,3] <- (prev[a,3]/vivants[a,3])*100
  }
  colnames(taux_prev) <- c("age","cohorte","taux_prevalence_simulation")
  write.csv(taux_prev,file = paste("taux_prev",c,sep="_"))
  
  print("f")
  taux_prev <- data.frame(matrix(ncol=3,nrow=0))
  
  prev <- read.csv(paste("prev_f",c,sep="_"))                # read the prevalence matrix for cohort c
  prev$X <- NULL
  colnames(prev) <- gsub("X","",colnames(prev))
  
  vivants <- read.csv(paste("vivants_f",c,sep="_"))          # read matrix of alive for cohort c
  vivants$X <- NULL
  colnames(vivants) <- gsub("X","",colnames(vivants))
  
  for(a in 1:nrow(vivants)){                               # compute age-specific prevalence rate for cohort c
    taux_prev[a,1] <- a+34
    taux_prev[a,2] <- c
    taux_prev[a,3] <- (prev[a,3]/vivants[a,3])*100
  }
  colnames(taux_prev) <- c("age","cohorte","taux_prevalence_simulation")
  write.csv(taux_prev,file = paste("taux_prev_f",c,sep="_"))
  
  
  print("af")
  taux_prev <- data.frame(matrix(ncol=3,nrow=0))
  
  prev <- read.csv(paste("prev_af",c,sep="_"))                # read the prevalence matrix for cohort c
  prev$X <- NULL
  colnames(prev) <- gsub("X","",colnames(prev))
  
  vivants <- read.csv(paste("vivants_af",c,sep="_"))          # read matrix of alive for cohort c
  vivants$X <- NULL
  colnames(vivants) <- gsub("X","",colnames(vivants))
  
  for(a in 1:nrow(vivants)){                               # compute age-specific prevalence rate for cohort c
    taux_prev[a,1] <- a+34
    taux_prev[a,2] <- c
    taux_prev[a,3] <- (prev[a,3]/vivants[a,3])*100
  }
  colnames(taux_prev) <- c("age","cohorte","taux_prevalence_simulation")
  write.csv(taux_prev,file = paste("taux_prev_af",c,sep="_"))
  
  print("nf")
  taux_prev <- data.frame(matrix(ncol=3,nrow=0))
  
  prev <- read.csv(paste("prev_nf",c,sep="_"))                # read the prevalence matrix for cohort c
  prev$X <- NULL
  colnames(prev) <- gsub("X","",colnames(prev))
  
  vivants <- read.csv(paste("vivants_nf",c,sep="_"))          # read matrix of alive for cohort c
  vivants$X <- NULL
  colnames(vivants) <- gsub("X","",colnames(vivants))
  
  for(a in 1:nrow(vivants)){                               # compute age-specific prevalence rate for cohort c
    taux_prev[a,1] <- a+34
    taux_prev[a,2] <- c
    taux_prev[a,3] <- (prev[a,3]/vivants[a,3])*100
  }
  colnames(taux_prev) <- c("age","cohorte","taux_prevalence_simulation")
  write.csv(taux_prev,file = paste("taux_prev_nf",c,sep="_"))
}


tx_prev <- data.frame(matrix(c(0),ncol=15,nrow=80))
colnames(tx_prev) <- c("1920_f","1920_af","1920_nf","1940_f","1940_af","1940_nf","1960_f","1960_af","1960_nf","1980_f","1980_af","1980_nf","2000_f","2000_af","2000_nf")
tx_prev
# loop over cohorts

# rownames(esperance_vie_tabac) <- read.csv(paste("esperance_vie_f",c,sep="_"))[,2]
tx_prev[,1] <- read.csv2("taux_prev_h_f_1920", sep = ",")[,4]
tx_prev[,2] <- read.csv2("taux_prev_h_af_1920", sep = ",")[,4]
tx_prev[,3] <- read.csv2("taux_prev_h_nf_1920", sep = ",")[,4]
tx_prev[,4] <- read.csv2("taux_prev_h_f_1940", sep = ",")[,4]
tx_prev[,5] <- read.csv2("taux_prev_h_af_1940", sep = ",")[,4]
tx_prev[,6] <- read.csv2("taux_prev_h_nf_1940", sep = ",")[,4]
tx_prev[,7] <- read.csv2("taux_prev_h_f_1960", sep = ",")[,4]
tx_prev[,8] <- read.csv2("taux_prev_h_af_1960", sep = ",")[,4]
tx_prev[,9] <- read.csv2("taux_prev_h_nf_1960", sep = ",")[,4]
tx_prev[,10] <- read.csv2("taux_prev_h_f_1980", sep = ",")[,4]
tx_prev[,11] <- read.csv2("taux_prev_h_af_1980", sep = ",")[,4]
tx_prev[,12] <- read.csv2("taux_prev_h_nf_1980", sep = ",")[,4]
tx_prev[,13] <- read.csv2("taux_prev_h_f_2000", sep = ",")[,4]
tx_prev[,14] <- read.csv2("taux_prev_h_af_2000", sep = ",")[,4]
tx_prev[,15] <- read.csv2("taux_prev_h_nf_2000", sep = ",")[,4]





plot(tx_prev$`2000_f`,xlab = "Âge", ylab = "Taux de prévalence (%)", type = "l", col = "red", lwd = 3, xaxt= "n")
axis(1,at=c(0,10,20,30,40,50,60,70,80), labels=c(35,45,55,65,75,85,90,100,110))
lines(tx_prev$`2000_af`, col = "blue", lwd = "3")
lines(tx_prev$`2000_nf`, col = "green", lwd = "1")
legend(x="topleft", legend=c("Fumeurs","Anciens fumeurs", "Non fumeurs"), col=c("red","blue","green"), pch=20, bty="n")
title("Hommes nées en 2000")
