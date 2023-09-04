### Using Code_Simulation program
# Attention : life expectancies are calculated for 35 years out of 81 years (from 35 to 115 years)
# The simulation code must be adapted to this case

## Purpose : compute life expectancy and healthy life expectancy for each cohort between 1920 and 2000

# etats : matrix of 10,000 rows and 61 columns. With individuals in rows and ages 35-95 in columns implementing the different states of 0, 1, 2, and 3
# survivants_sains : matrix with 61 rows and 3 columns. With the ages in rows and the cohort and number of healthy survivors (living healthy person) in columns.
# survivants : matrix with 61 rows and 3 columns. With the ages in line and the cohort and the number of survivors (healthy or having had a heart attack) in the column.
# esperance_vie_bonne_sante : matrix with 1 line and 2 columns. With the cohort and the number of additional years of life in good health after 35 years.
# esperance_vie : matrix with 1 line and 2 columns. With the cohort and the number of additional years of life after 35 years.


## Package :
library(doParallel)
library(foreach)

cl <- makeCluster(20)
registerDoParallel(cl)


## Fonction gÃ©nÃ©rale :
esp <- foreach(co=1920:2000, .verbose = TRUE) %dopar% {                   # loop over cohorts
  
  print(co)
  
  survivants_sains <- data.frame(matrix(ncol=3,nrow=0))
  survivants_sains[1,1]<- 35
  survivants_sains[1,2] <- co
  survivants_sains[1,3] <- nb_people
  
  esperance_vie_bonne_sante <- data.frame(matrix(ncol=2,nrow=0))
  
  survivants <- data.frame(matrix(ncol=3,nrow=0))
  survivants[1,1]<- 35
  survivants[1,2] <- co
  survivants[1,3] <- nb_people
  
  esperance_vie <- data.frame(matrix(ncol=2,nrow=0))
  
  etats <- read.csv(paste("etats",co,sep="_"))
  etats$X <- NULL
  colnames(etats) <- gsub("X","",colnames(etats))
  
  
  for(age in 2:ncol(etats)){
    vivant <- 0
    for(i in 1:nrow(etats)){                    # calculation of the number of healthy individuals at age a (or dead at age a but alive healthy at age a-1)
      if(etats[i,age] == 0){
        vivant = vivant +1
      }
      if(etats[i,age] == 2 & etats[i,age-1]== 0){
        vivant = vivant + 0.5
      }
    }
    ligne <- c(age+34,co,vivant)
    survivants_sains <- rbind(survivants_sains,ligne)
  }
  
  somme <- 0
  for(age in 2:nrow(survivants_sains)){
    somme <- somme + survivants_sains[age,3]    # calculation of the sum of the number of healthy individuals for each age a of the cohort c
  }
  e <- 1/2 + somme/nb_people                       # calculation of healthy life expectancy
  ligne <- c(co,e)
  esperance_vie_bonne_sante <- rbind(esperance_vie_bonne_sante,ligne)
  
  for(age in 2:ncol(etats)){
    vivant <- 0
    for(i in 1:nrow(etats)){                    # calculation of the number of individuals living healthy or sick at age a or dead at age a but alive (healthy or sick) at age a-1
      if(etats[i,age] == 0 | etats[i,age]== 1){
        vivant = vivant +1
      }
      if((etats[i,age] == 2 | etats[i,age]== 3) & (etats[i,age-1]== 1 | etats[i,age-1]== 0)){
        vivant = vivant + 0.5
      }
    }
    ligne <- c(age+34,co,vivant)
    survivants <- rbind(survivants,ligne)
  }
  
  somme <- 0
  for(age in 2:nrow(survivants)){
    somme <- somme + survivants[age,3]          # calculation of the sum of the number of survivors (sick or healthy) for each age a of the cohort c
  }
  e <- 1/2 + somme/nb_people                        # life expectancy calculation
  ligne <- c(co,e)
  esperance_vie <- rbind(esperance_vie,ligne)
  
  colnames(survivants_sains) <- c("age","cohorte","nb_sains")
  colnames(esperance_vie_bonne_sante) <- c("Cohorte","esperance de vie en bonne sante")
  colnames(survivants) <- c("age","cohorte","nb_vivants")
  colnames(esperance_vie) <- c("Cohorte","esperance_de_vie")
  
  esperance_vie$esperance_vie_bonne_sante <- esperance_vie_bonne_sante[,2]
  write.csv(esperance_vie, file = paste("esperance_vie",co,sep="_"))
}
names(esp) <- 2000:2000
esp












esp_f <- foreach(co=1920:2000, .verbose = TRUE) %dopar% {                   # loop over cohorts
  
  print(co)
  
  matrice <- read.csv(paste("matrice",co,sep="_"))
  matrice$X <- NULL
  colnames(matrice) <- gsub("X","",colnames(matrice))
  
  nb_people <- sum(matrice[,8] == 1)
  print(nb_people)
  
  survivants_sains <- data.frame(matrix(ncol=3,nrow=0))
  survivants_sains[1,1]<- 35
  survivants_sains[1,2] <- co
  survivants_sains[1,3] <- nb_people
  
  esperance_vie_bonne_sante <- data.frame(matrix(ncol=2,nrow=0))
  
  survivants <- data.frame(matrix(ncol=3,nrow=0))
  survivants[1,1]<- 35
  survivants[1,2] <- co
  survivants[1,3] <- nb_people
  
  esperance_vie <- data.frame(matrix(ncol=2,nrow=0))
  
  etats <- read.csv(paste("etats",co,sep="_"))
  etats$X <- NULL
  colnames(etats) <- gsub("X","",colnames(etats))
  
  
  
  
  for(age in 2:ncol(etats)){
    vivant <- 0
    for(i in 1:nrow(etats)){                    # calculation of the number of healthy individuals at age a (or dead at age a but alive healthy at age a-1)
      if(etats[i,age] == 0 & matrice[i,8] == 1){
        vivant = vivant +1
      }
      if(etats[i,age] == 2 & etats[i,age-1]== 0  & matrice[i,8] == 1){
        vivant = vivant + 0.5
      }
    }
    ligne <- c(age+34,co,vivant)
    survivants_sains <- rbind(survivants_sains,ligne)
  }
  
  somme <- 0
  for(age in 2:nrow(survivants_sains)){
    somme <- somme + survivants_sains[age,3]    # calculation of the sum of the number of healthy individuals for each age a of the cohort c
  }
  e <- 1/2 + somme/nb_people                       # calculation of healthy life expectancy
  ligne <- c(co,e)
  esperance_vie_bonne_sante <- rbind(esperance_vie_bonne_sante,ligne)
  
  for(age in 2:ncol(etats)){
    vivant <- 0
    for(i in 1:nrow(etats)){                    # calculation of the number of individuals living healthy or sick at age a or dead at age a but alive (healthy or sick) at age a-1
      if((etats[i,age] == 0  & matrice[i,8] == 1) | (etats[i,age]== 1 & matrice[i,8] == 1)){
        vivant = vivant +1
      }
      if((etats[i,age] == 2 | etats[i,age]== 3) & (etats[i,age-1]== 1 | etats[i,age-1]== 0)  & matrice[i,8] == 1){
        vivant = vivant + 0.5
      }
    }
    ligne <- c(age+34,co,vivant)
    survivants <- rbind(survivants,ligne)
  }
  
  somme <- 0
  for(age in 2:nrow(survivants)){
    somme <- somme + survivants[age,3]          # calculation of the sum of the number of survivors (sick or healthy) for each age a of the cohort c
  }
  e <- 1/2 + somme/nb_people                        # life expectancy calculation
  ligne <- c(co,e)
  esperance_vie <- rbind(esperance_vie,ligne)
  
  colnames(survivants_sains) <- c("age","cohorte","nb_sains")
  colnames(esperance_vie_bonne_sante) <- c("Cohorte","esperance de vie en bonne sante")
  colnames(survivants) <- c("age","cohorte","nb_vivants")
  colnames(esperance_vie) <- c("Cohorte","esperance_de_vie")
  
  esperance_vie$esperance_vie_bonne_sante <- esperance_vie_bonne_sante[,2]
  write.csv(esperance_vie, file = paste("esperance_vie_f",co,sep="_"))
}
names(esp_f) <- 1920:2000
esp_f











esp_af <- foreach(co=1920:2000, .verbose = TRUE) %dopar% {                   # loop over cohorts
  
  print(co)
  
  matrice <- read.csv(paste("matrice",co,sep="_"))
  matrice$X <- NULL
  colnames(matrice) <- gsub("X","",colnames(matrice))
  
  nb_people <- sum(matrice[,8] == 2)
  print(nb_people)
  
  survivants_sains <- data.frame(matrix(ncol=3,nrow=0))
  survivants_sains[1,1]<- 35
  survivants_sains[1,2] <- co
  survivants_sains[1,3] <- nb_people
  
  esperance_vie_bonne_sante <- data.frame(matrix(ncol=2,nrow=0))
  
  survivants <- data.frame(matrix(ncol=3,nrow=0))
  survivants[1,1]<- 35
  survivants[1,2] <- co
  survivants[1,3] <- nb_people
  
  esperance_vie <- data.frame(matrix(ncol=2,nrow=0))
  
  etats <- read.csv(paste("etats",co,sep="_"))
  etats$X <- NULL
  colnames(etats) <- gsub("X","",colnames(etats))
  
  
  
  
  for(age in 2:ncol(etats)){
    vivant <- 0
    for(i in 1:nrow(etats)){                    # calculation of the number of healthy individuals at age a (or dead at age a but alive healthy at age a-1)
      if(etats[i,age] == 0 & matrice[i,8] == 2){
        vivant = vivant +1
      }
      if(etats[i,age] == 2 & etats[i,age-1]== 0  & matrice[i,8] == 2){
        vivant = vivant + 0.5
      }
    }
    ligne <- c(age+34,co,vivant)
    survivants_sains <- rbind(survivants_sains,ligne)
  }
  
  somme <- 0
  for(age in 2:nrow(survivants_sains)){
    somme <- somme + survivants_sains[age,3]    # calculation of the sum of the number of healthy individuals for each age a of the cohort c
  }
  e <- 1/2 + somme/nb_people                       # calculation of healthy life expectancy
  ligne <- c(co,e)
  esperance_vie_bonne_sante <- rbind(esperance_vie_bonne_sante,ligne)
  
  for(age in 2:ncol(etats)){
    vivant <- 0
    for(i in 1:nrow(etats)){                    # calculation of the number of individuals living healthy or sick at age a or dead at age a but alive (healthy or sick) at age a-1
      if((etats[i,age] == 0  & matrice[i,8] == 2) | (etats[i,age]== 1 & matrice[i,8] == 2)){
        vivant = vivant +1
      }
      if((etats[i,age] == 2 | etats[i,age]== 3) & (etats[i,age-1]== 1 | etats[i,age-1]== 0)  & matrice[i,8] == 2){
        vivant = vivant + 0.5
      }
    }
    ligne <- c(age+34,co,vivant)
    survivants <- rbind(survivants,ligne)
  }
  
  somme <- 0
  for(age in 2:nrow(survivants)){
    somme <- somme + survivants[age,3]          # calculation of the sum of the number of survivors (sick or healthy) for each age a of the cohort c
  }
  e <- 1/2 + somme/nb_people                        # life expectancy calculation
  ligne <- c(co,e)
  esperance_vie <- rbind(esperance_vie,ligne)
  
  colnames(survivants_sains) <- c("age","cohorte","nb_sains")
  colnames(esperance_vie_bonne_sante) <- c("Cohorte","esperance de vie en bonne sante")
  colnames(survivants) <- c("age","cohorte","nb_vivants")
  colnames(esperance_vie) <- c("Cohorte","esperance_de_vie")
  
  esperance_vie$esperance_vie_bonne_sante <- esperance_vie_bonne_sante[,2]
  write.csv(esperance_vie, file = paste("esperance_vie_af",co,sep="_"))
}

names(esp_af) <- 2000:2000
esp_af






esp_nf <- foreach(co=1920:2000, .verbose = TRUE) %dopar% {                   # loop over cohorts
  
  print(co)
  
  matrice <- read.csv(paste("matrice",co,sep="_"))
  matrice$X <- NULL
  colnames(matrice) <- gsub("X","",colnames(matrice))
  
  nb_people <- sum(matrice[,8] == 0)
  print(nb_people)
  
  survivants_sains <- data.frame(matrix(ncol=3,nrow=0))
  survivants_sains[1,1]<- 35
  survivants_sains[1,2] <- co
  survivants_sains[1,3] <- nb_people
  
  esperance_vie_bonne_sante <- data.frame(matrix(ncol=2,nrow=0))
  
  survivants <- data.frame(matrix(ncol=3,nrow=0))
  survivants[1,1]<- 35
  survivants[1,2] <- co
  survivants[1,3] <- nb_people
  
  esperance_vie <- data.frame(matrix(ncol=2,nrow=0))
  
  etats <- read.csv(paste("etats",co,sep="_"))
  etats$X <- NULL
  colnames(etats) <- gsub("X","",colnames(etats))
  
  
  
  
  for(age in 2:ncol(etats)){
    vivant <- 0
    for(i in 1:nrow(etats)){                    # calculation of the number of healthy individuals at age a (or dead at age a but alive healthy at age a-1)
      if(etats[i,age] == 0 & matrice[i,8] == 0){
        vivant = vivant +1
      }
      if(etats[i,age] == 2 & etats[i,age-1]== 0  & matrice[i,8] == 0){
        vivant = vivant + 0.5
      }
    }
    ligne <- c(age+34,co,vivant)
    survivants_sains <- rbind(survivants_sains,ligne)
  }
  
  somme <- 0
  for(age in 2:nrow(survivants_sains)){
    somme <- somme + survivants_sains[age,3]    # calculation of the sum of the number of healthy individuals for each age a of the cohort c
  }
  e <- 1/2 + somme/nb_people                       # calculation of healthy life expectancy
  ligne <- c(co,e)
  esperance_vie_bonne_sante <- rbind(esperance_vie_bonne_sante,ligne)
  
  for(age in 2:ncol(etats)){
    vivant <- 0
    for(i in 1:nrow(etats)){                    # calculation of the number of individuals living healthy or sick at age a or dead at age a but alive (healthy or sick) at age a-1
      if((etats[i,age] == 0  & matrice[i,8] == 0) | (etats[i,age]== 1 & matrice[i,8] == 0)){
        vivant = vivant +1
      }
      if((etats[i,age] == 2 | etats[i,age]== 3) & (etats[i,age-1]== 1 | etats[i,age-1]== 0)  & matrice[i,8] == 0){
        vivant = vivant + 0.5
      }
    }
    ligne <- c(age+34,co,vivant)
    survivants <- rbind(survivants,ligne)
  }
  
  somme <- 0
  for(age in 2:nrow(survivants)){
    somme <- somme + survivants[age,3]          # calculation of the sum of the number of survivors (sick or healthy) for each age a of the cohort c
  }
  e <- 1/2 + somme/nb_people                        # life expectancy calculation
  ligne <- c(co,e)
  esperance_vie <- rbind(esperance_vie,ligne)
  
  colnames(survivants_sains) <- c("age","cohorte","nb_sains")
  colnames(esperance_vie_bonne_sante) <- c("Cohorte","esperance de vie en bonne sante")
  colnames(survivants) <- c("age","cohorte","nb_vivants")
  colnames(esperance_vie) <- c("Cohorte","esperance_de_vie")
  
  esperance_vie$esperance_vie_bonne_sante <- esperance_vie_bonne_sante[,2]
  write.csv(esperance_vie, file = paste("esperance_vie_nf",co,sep="_"))
}
names(esp_nf) <- 2000:2000
esp_nf


esperance_vie_tabac <- data.frame(matrix(c(0),ncol=6,nrow=81))
colnames(esperance_vie_tabac) <- c("ev_F","ev_bs_F","ev_AF","ev_bs_AF","ev_NF","ev_bs_NF")
esperance_vie_tabac



for(c in 1920:2000){                                # loop over cohorts

  # rownames(esperance_vie_tabac) <- read.csv(paste("esperance_vie_f",c,sep="_"))[,2]
  esperance_vie_tabac[c-1919,1] <- read.csv(paste("esperance_vie_f",c,sep="_"))[,3]
  esperance_vie_tabac[c-1919,2] <- read.csv(paste("esperance_vie_f",c,sep="_"))[,4]
  esperance_vie_tabac[c-1919,3] <- read.csv(paste("esperance_vie_af",c,sep="_"))[,3]
  esperance_vie_tabac[c-1919,4] <- read.csv(paste("esperance_vie_af",c,sep="_"))[,4]
  esperance_vie_tabac[c-1919,5] <- read.csv(paste("esperance_vie_nf",c,sep="_"))[,3]
  esperance_vie_tabac[c-1919,6] <- read.csv(paste("esperance_vie_nf",c,sep="_"))[,4]
  
}

esperance_vie_tabac
rownames(esperance_vie_tabac) <- 1920:2000

plot(esperance_vie_tabac$ev_NF, ylim = c(40,65),xlab = "Année de naissance", ylab = "Années supplémentaires", type = "l", col = "green", xaxt="n", lwd = 3)
axis(1,at=c(0,10,20,30,40,50,60,70,80), labels=c(1920,1930,1940,1950,1960,1970,1980,1990,2000))
lines(esperance_vie_tabac$ev_AF, col = "blue", lwd = "3")
lines(esperance_vie_tabac$ev_F, col = "red", lwd = "3")
lines(esperance_vie_tabac$ev_bs_F, col = "red", type = "l")
lines(esperance_vie_tabac$ev_bs_AF, col = "blue", type = "l")
lines(esperance_vie_tabac$ev_bs_NF, col = "green", type = "l")
legend(x="topleft", legend=c("Fumeurs","Anciens fumeurs", "Non fumeurs"), col=c("red","blue","green"), pch=20, bty="n")
legend("bottomright", legend=c("Esp.vie", "Esp.vie bonne santé"), lty=1, lwd=c(3,1), 
       text.font=1, bty="n")
title("Femmes")
