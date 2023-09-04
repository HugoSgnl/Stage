### Using Code_Simulation program

## Purpose : to recover the prevalences by age for each cohort for the French population

# Pop_35 : matrix listing the number of people aged 35 for each birth cohort (1 matrix for each sex)
# proba : matrix with 61 rows and 3 columns. With the ages in rows and the P01 and the reference cohort in columns
# etats : matrix with 10,000 rows and 61 columns. With individuals in rows and ages 35-95 in columns implementing the different states of 0, 1, 2, and 3
# PREV_POP : matrix with 61 lines and 3 columns. With the ages in rows and the prevalences for each age as well as the reference cohort in columns


## Data 
Pop_35 <- read.csv(file="population_fr_35ans_cohorte_homme.csv",sep=";",dec=",")
cl <- makeCluster(20)
registerDoParallel(cl)

### Programme

## Calcul des probabilités de passer de l'état 0 à 35 ans à l'état 1 à l'age a : P01

foreach(c=1920:2000, .verbose = TRUE) %dopar% {                               # loops over cohorts
  print(c)
  
  proba <- data.frame(matrix(ncol=3,nrow=0))
  
  
  etats <- read.csv(paste("etats",c,sep="_"))
  etats$X <- NULL
  colnames(etats) <- gsub("X","",colnames(etats))
  
  for(age in 35:95){
    nb <- 0
    for(i in 1:nrow(etats)){                      # calculation of the prevalence
      if(etats[i,age-34] == 1){
        nb = nb +1
      }
      if(etats[i,age-34] ==3){
        nb = nb + 0.5
      }
    }
    p <- nb/10000                                   # probability calculation P01
    ligne <- c(age,c,p)
    proba <- rbind(proba,ligne)
  }
  colnames(proba) <- c("age","cohorte","nb_1_3")
  write.csv(proba,file=paste("proba",c,sep="_"))
}


foreach(c=1920:1920, .verbose = TRUE) %dopar% {                               # loop over cohorts
  
  PREV_POP <- data.frame(matrix(nrow=0,ncol=3))
  
  proba <- read.csv(paste("proba",c,sep="_"))       # reading the matrix containing the probabilities
  proba$X <- NULL
  colnames(proba) <- gsub("X","",colnames(proba))
  
  for(age in 35:95){
    pop <- proba[age-34,3]*Pop_35[c-1919,2]         # calculation of the number of sick people at each age a for cohort c
    
    ligne <- c(age,c,pop)
    PREV_POP <- rbind(PREV_POP,ligne)
  }
  colnames(PREV_POP) <- c("age","cohorte","prevalence")
  write.csv(PREV_POP,file=paste("PREV_POP",c,sep="_"))
}


prev_pop_fra <- data.frame(matrix(c(0),ncol=15,nrow=61))
colnames(prev_pop_fra) <- c("1920_f","1920_af","1920_nf","1940_f","1940_af","1940_nf","1960_f","1960_af","1960_nf","1980_f","1980_af","1980_nf","2000_f","2000_af","2000_nf")
prev_pop_fra
                               # loop over cohorts
  
  # rownames(esperance_vie_tabac) <- read.csv(paste("esperance_vie_f",c,sep="_"))[,2]
  prev_pop_fra[,1] <- read.csv2("PREV_POP_f_1920", sep = ",")[,4]
  prev_pop_fra[,2] <- read.csv2("PREV_POP_af_1920", sep = ",")[,4]
  prev_pop_fra[,3] <- read.csv2("PREV_POP_nf_1920", sep = ",")[,4]
  prev_pop_fra[,4] <- read.csv2("PREV_POP_f_1940", sep = ",")[,4]
  prev_pop_fra[,5] <- read.csv2("PREV_POP_af_1940", sep = ",")[,4]
  prev_pop_fra[,6] <- read.csv2("PREV_POP_nf_1940", sep = ",")[,4]
  prev_pop_fra[,7] <- read.csv2("PREV_POP_f_1960", sep = ",")[,4]
  prev_pop_fra[,8] <- read.csv2("PREV_POP_af_1960", sep = ",")[,4]
  prev_pop_fra[,9] <- read.csv2("PREV_POP_nf_1960", sep = ",")[,4]
  prev_pop_fra[,10] <- read.csv2("PREV_POP_f_1980", sep = ",")[,4]
  prev_pop_fra[,11] <- read.csv2("PREV_POP_af_1980", sep = ",")[,4]
  prev_pop_fra[,12] <- read.csv2("PREV_POP_nf_1980", sep = ",")[,4]
  prev_pop_fra[,13] <- read.csv2("PREV_POP_f_2000", sep = ",")[,4]
  prev_pop_fra[,14] <- read.csv2("PREV_POP_af_2000", sep = ",")[,4]
  prev_pop_fra[,15] <- read.csv2("PREV_POP_nf_2000", sep = ",")[,4]
  




plot(prev_pop_fra$`2000_af`,xlab = "Âge", ylab = "Nombre de cas en France", type = "l", col = "blue", lwd = 3, xaxt= "n")
axis(1,at=c(0,10,20,30,40,50,60), labels=c(35,45,55,65,75,85,90))
lines(prev_pop_fra$`2000_f`, col = "red", lwd = "3")
lines(prev_pop_fra$`2000_nf`, col = "black", lwd = "1")
legend(x="topleft", legend=c("Fumeurs","Anciens fumeurs", "Non fumeurs"), col=c("red","blue","black"), pch=20, bty="n")
title("Femmes nées en 2000")


plot(prev_pop_fra$`1980_af`,xlab = "Âge", ylab = "Nombre de cas en France", type = "l", col = "blue", lwd = 3, xaxt= "n")
axis(1,at=c(0,10,20,30,40,50,60), labels=c(35,45,55,65,75,85,90))
lines(prev_pop_fra$`1980_f`, col = "red", lwd = "3")
lines(prev_pop_fra$`1980_nf`, col = "green", lwd = "1")
legend(x="topleft", legend=c("Fumeurs","Anciens fumeurs", "Non fumeurs"), col=c("red","blue","green"), pch=20, bty="n")
title("Femmes nées en 1980")
