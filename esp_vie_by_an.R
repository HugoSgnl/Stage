### Using Code_Simulation program

## Purpose : Compute life expectancy at age 35 and healthy life expectancy at age 35 for a given year

# survivants_an : matrix with 61 rows and 3 columns. With the ages in line and the cohort that will have the age in the given year and the number of survivors (healthy or having had a heart attack) in the column.
# esperance_vie_an : matrix with 1 row and 3 columns. With the cohort, the number of additional years of life after age 35 and the number of additional years of life in good health after age 35 (the latter is added at the end).
# survivants_sains_an : matrix with 61 rows and 3 columns. With the ages in rows and the cohort that will age in the given year and the number of healthy survivors (person alive and in good health) in columns.
# esperance_vie_bonne_sante_an : matrix with 1 line and 2 columns. With the cohort and the number of additional years of life in good health after 35 years.
# etats : matrix of 10,000 rows and 61 columns. With individuals in rows and ages 35-95 in columns implementing the different states of 0, 1, 2, and 3.


annee <- c(2035)                    # to be varied according to the desired year
for(an in annee){                   # loop over the years
  
  print(an)
  
  survivants_an <- data.frame(matrix(ncol=3,nrow=0))
  esperance_vie_an <- data.frame(matrix(ncol=2,nrow=0))
  
  survivants_sains_an <- data.frame(matrix(ncol=3,nrow=0))
  esperance_vie_bonne_sante_an <- data.frame(matrix(ncol=2,nrow=0))
  
  int_min <- an - 115                # calculation of the interval on the cohorts: lower limit
  int_max <- an - 35                # calculation of the interval on the cohorts: upper limit
  
  for(c in int_min:int_max){        # loop over cohorts
    
    etats <- read.csv(paste("etats",c,sep="_"))
    etats$X <- NULL
    colnames(etats) <- gsub("X","",colnames(etats))
    
    A <- an - c
    vivant <- 0
    vivant_sain <- 0
    
    if(A>=36){
      for(i in 1:nrow(etats)){
        if(etats[i,A-34] == 0 | etats[i,A-34]==1){
          vivant = vivant +1
        }
        if((etats[i,A-34] ==2 | etats[i,A-34]==3) & (etats[i,A-34-1]==1 | etats[i,A-34-1]==0)){
          vivant = vivant + 0.5
        }
        if(etats[i,A-34] == 0){
          vivant_sain = vivant_sain +1
          
        }
        if(etats[i,A-34] ==2 & etats[i,A-34-1]==0){
          vivant_sain = vivant_sain + 0.5
        }
      }
      ligne <- c(A,c,vivant)
      survivants_an <- rbind(survivants_an,ligne)
      
      ligne <- c(A,c,vivant_sain)
      survivants_sains_an <- rbind(survivants_sains_an,ligne)
    }
  }
  ligne2 <- c(35,2000,10000)
  survivants_sains_an <- rbind(survivants_sains_an,ligne2)
  
  ligne3 <- c(35,2000,10000)
  survivants_an <- rbind(survivants_an,ligne3)
  
  somme <- 0
  for(a in 1:nrow(survivants_an)){
    somme <- somme + survivants_an[a,3]
  }
  e <- 1/2 + somme/10000                 # calculation of life expectancy for the year 'an'
  ligne <- c(an,e)
  esperance_vie_an <- rbind(esperance_vie_an,ligne)
  colnames(esperance_vie_an) <- c("annee","esperance_de_vie")
  
  somme_sain <- 0
  for(a in 1:nrow(survivants_sains_an)){
    somme_sain <- somme_sain + survivants_sains_an[a,3]
  }
  e <- 1/2 + somme_sain/10000           # calculation of healthy life expectancy for the year 'an'
  ligne <- c(an,e)
  esperance_vie_bonne_sante_an <- rbind(esperance_vie_bonne_sante_an,ligne)
  
  esperance_vie_an$esperance_vie_bonne_sante <- esperance_vie_bonne_sante_an[,2]
  
  write.csv(esperance_vie_an,file = paste("esperance_vie_an_femme",an,sep="_"))
  
}

