### Using Code_Simulation program

## Purpose : to recover the prevalences by age for each cohort for the French population

# Pop_35 : matrix listing the number of people aged 35 for each birth cohort (1 matrix for each sex)
# proba : matrix with 61 rows and 3 columns. With the ages in rows and the P01 and the reference cohort in columns
# etats : matrix with 10,000 rows and 61 columns. With individuals in rows and ages 35-95 in columns implementing the different states of 0, 1, 2, and 3
# PREV_POP : matrix with 61 lines and 3 columns. With the ages in rows and the prevalences for each age as well as the reference cohort in columns


## Data 
Pop_35 <- read.csv(file="population_fr_35ans_femme.csv",sep=";",dec=",")
cl <- makeCluster(20)
registerDoParallel(cl)

### Programme

## Calcul des probabilités de passer de l'état 0 à 35 ans à l'état 1 à l'age a : P01

foreach(c=1920:2000, .verbose = TRUE) %dopar% {                               # loops over cohorts
  print(c)
  
  proba <- data.frame(matrix(ncol=3,nrow=0))
  
  matrice <- read.csv(paste("matrice",c,sep="_"))
  matrice$X <- NULL
  colnames(matrice) <- gsub("X","",colnames(matrice))
  
  etats <- read.csv(paste("etats",c,sep="_"))
  etats$X <- NULL
  colnames(etats) <- gsub("X","",colnames(etats))
  
  for(age in 35:95){
    nb <- 0
    for(i in 1:nrow(etats)){                      # calculation of the prevalence
      if((etats[i,age-34] == 1) & (matrice[i,7] == 1)){
        nb = nb +1
      }
      if((etats[i,age-34] ==3) & (matrice[i,7] == 1)){
        nb = nb + 0.5
      }
    }
    p <- nb/10000                                   # probability calculation P01
    ligne <- c(age,c,p)
    proba <- rbind(proba,ligne)
  }
  colnames(proba) <- c("age","cohorte","nb_1_3")
  write.csv(proba,file=paste("proba_f",c,sep="_"))
}


foreach(c=1920:2000, .verbose = TRUE) %dopar% {                               # loop over cohorts
  
  PREV_POP <- data.frame(matrix(nrow=0,ncol=3))
  
  proba <- read.csv(paste("proba_f",c,sep="_"))       # reading the matrix containing the probabilities
  proba$X <- NULL
  colnames(proba) <- gsub("X","",colnames(proba))
  
  for(age in 35:95){
    pop <- proba[age-34,3]*Pop_35[c-1919,2]         # calculation of the number of sick people at each age a for cohort c
    
    ligne <- c(age,c,pop)
    PREV_POP <- rbind(PREV_POP,ligne)
  }
  colnames(PREV_POP) <- c("age","cohorte","prevalence")
  write.csv(PREV_POP,file=paste("PREV_POP_f",c,sep="_"))
}
