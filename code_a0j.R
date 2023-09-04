library(dplyr)
nb_people <- 100

matrice <- matrix(c(0),                                           # creation for each cohort of the matrix of individuals
                  nrow = nb_people,
                  ncol = 7)
colnames(matrice) <- c("age","cohorte","age_maladie","age_deces", "tabac", "age_arret", "statut_tab_when_inf")

n <- 1
m <- nb_people
for(ind in n:m){
  matrice[ind,2] <- co
  matrice[ind,1] <- 35
  matrice[ind,7] <- NA
}
matrice <- as.data.frame(matrice)

etats <- matrix(c(0),                                                         # creation for each cohort of the state matrix
                nrow = nb_people,
                ncol = horizon)
colnames(etats) <- c(35:115)

tabac <- matrix(c(0),                                                         # creation for each cohort of the tobacco matrix
                nrow = nb_people,
                ncol = horizon)
colnames(tabac) <- c(35:115)



prop <- matrix(c(0), nrow = 3, ncol = 81)
rownames(prop) <- c("F", "AF", "NF")
prop

prop_f <- prop_tabac$prop_tabac_35[prop_tabac$ï..cohorte == 1920]               #for each cohort, get the proportion of smokers
prop_nf <- prop_non_fumeur$prop_nf[prop_non_fumeur$ï..cohorte == 1920]

prop[1,1] <- prop_f
prop[2,1] <- 1 - (prop_f+prop_nf)
prop[3,1] <- prop_nf
prop

A01 <- matrix(c(0), nrow = 3, ncol = 81)
rownames(A01) <- c("a01F","a01AF", "a01NF")
A01[1,1] <- a01[1,1]*4.2
A01[2,1] <- a01[1,1]*2.1
A01[3,1] <- a01[1,1]
A01

A02 <- matrix(c(0), nrow = 3, ncol = 81)
rownames(A02) <- c("a02F","a02AF", "a02NF")
A02[1,1] <- a02[1,1]*2.82
A02[2,1] <- a02[1,1]*1.7
A02[3,1] <- a02[1,1]
A02

f <- matrix(c(0), nrow = 1, ncol = 81)
af <- matrix(c(0), nrow = 1, ncol = 81)
nf <- matrix(c(0), nrow = 1, ncol = 81)
nb_people_v <- matrix(c(0), nrow = 1, ncol = 81)

for(i in 1:nrow(tabac)){                                                      #proportion de fumeurs à 35 ans,
  alea_SMOKE <- runif(1,0,1)
  if (alea_SMOKE <= prop_f){
    tabac[i] <- 1
    
  }
  if ((alea_SMOKE > prop_f) & (alea_SMOKE <= (prop_f + prop_nf))){
    tabac[i] <- 2
    matrice[i,6] <- 35
  }
}

f[,1] <- sum(tabac[,1] == 1)
f[,1]

for (a in 2:horizon){                                                         #loop for ages
  print(a +34)
  
  
  for (i in 1:nrow(tabac)){                                                   #probabilité d'arrêter de fumer
    
    age_arret <- matrice[i,1]
    alea_XSMOKE <- runif(1,0,1)
    
    if (tabac[i,a-1] == 1){
      if (alea_XSMOKE <= 0.01){
        tabac[i,a] <- 2
        matrice[i,6] <- age_arret + 1
      }
      else{
        tabac[i,a] <- 1
      }
    }
    if (tabac[i,a-1] == 2){
      tabac[i,a] <- 2
    }
    matrice[i,5] <- tabac[i,ncol(tabac)]
  }
  
  
  for (i in 1:nrow(etats)){                                                   #loop over individuals
    annee_naiss <- matrice[i,2]
    age_i <- matrice[i,1]


    v_incidence <- as.numeric(incidence[a-1,co-1919])                         # incidence rate recovery
    mortalite <- quotient_morta[a-1,co-1919]                                  # recovery of the mortality quotient

    incidence_F <- A01[1,a-1]                             #incidence rate recovery for smokers
    mortalite_F <-A02[1,a-1]                                        #recovery of the mortaliy qutotient for smokers

    incidence_NF <- A01[3,a-1]                          #incidence rate recovery for non smokers
    mortalite_NF <- A02[3,a-1]                                       #recovery of the mortaliy qutotient for non smokers

    incidence_AF <- A01[2,a-1]                           #incidence rate recovery for ex smokers
    mortalite_AF <- A02[2,a-1]                                       #recovery of the mortaliy qutotient for ex smokers

    alea <- runif(1,0,1)
    if (etats[i,a-1] == 0){                                                   # state 0 : healthy living

      if (tabac[i,a-1] == 1 ){                                                # tobacco 1 : smokers

        f[,a] <- f[,a] + 1
        if (alea <= incidence_F){                                                   # transition : having a myocardial infarction
          etats[i,a] <- 1
          matrice[i,3] <- age_i
          matrice[i,7] <- 1

          alea2 <- runif(1,0,1)
          if (alea2 <= mortalite_F*RR1_H){                                    # transition : transition to state 3: die in the first year
            etats[i,a] <- 3
            matrice[i,4] <- age_i
          }
        }

        alea3 <- runif(1,0,1)

        if ((alea3 <= mortalite_F) & (alea > incidence_F)){                       # transition : die without having had a myocardial infarction
          etats[i,a] <- 2
          matrice[i,4] <- age_i
        }
        if ((alea3 > mortalite_F) & (alea > incidence_F)){                        # stay healthy
          etats[i,a] <- 0
        }
      }

      if (tabac[i,a-1] == 2 ){                                                # tobacco 2 : ex smokers

        af[,a] <- af[,a] +1
        if (alea <= incidence_AF){                                                  # transition : having a myocardial infarction
          etats[i,a] <- 1
          matrice[i,3] <- age_i
          matrice[i,7] <- 2

          alea2 <- runif(1,0,1)
          if (alea2 <= mortalite_AF*RR1_H){                                   # transition : transition to state 3: die in the first year
            etats[i,a] <- 3
            matrice[i,4] <- age_i
          }
        }

        alea3 <- runif(1,0,1)
        if ((alea3 <= mortalite_AF) & (alea > incidence_AF)){                     # transition : die without having had a myocardial infarction
          etats[i,a] <- 2
          matrice[i,4] <- age_i
        }
        if ((alea3 > mortalite_AF) & (alea > incidence_AF)){                      # stay healthy
          etats[i,a] <- 0
        }
      }

      if (tabac[i,a-1] == 0 ){                                                # tobacco 0 : non smokers

        nf[,a] <- nf[,a] + 1
        if (alea <= incidence_NF){                                                  # transition : having a myocardial infarction
          etats[i,a] <- 1
          matrice[i,3] <- age_i
          matrice[i,7] <- 0

          alea2 <- runif(1,0,1)
          if (alea2 <= mortalite_NF*RR1_H){                                   # transition : transition to state 3: die in the first year
            etats[i,a] <- 3
            matrice[i,4] <- age_i
          }
        }

        alea3 <- runif(1,0,1)
        if ((alea3 <= mortalite_NF) & (alea > incidence_NF)){                     # transition : die without having had a myocardial infarction
          etats[i,a] <- 2
          matrice[i,4] <- age_i
        }
        if ((alea3 > mortalite_NF) & (alea > incidence_NF)){                      # stay healthy
          etats[i,a] <- 0
        }
      }

    }


    if (etats[i,a-1] == 1){                                                   # state 1 : alive having had a myocardial infarction

      if (tabac[i,a-1] == 1) {

        f[,a] <- f[,a] + 1
        morta_F <- mortalite_F*RR2_H
        if (alea > morta_F){                                                  # stay alive but sick
          etats[i,a] <- 1
        }
        if (alea <= morta_F){                                                 # transition : die having had a myocardial infarction after the first year
          etats[i,a] <- 2
          matrice[i,4] <- age_i
        }

      }

      if (tabac[i,a-1] == 2) {

        af[,a] <- af[,a] + 1
        morta_AF <- mortalite_AF*RR2_H
        if (alea > morta_AF){                                                 # stay alive but sick
          etats[i,a] <- 1
        }
        if (alea <= morta_AF){                                                # transition : die having had a myocardial infarction after the first year
          etats[i,a] <- 2
          matrice[i,4] <- age_i
        }

      }

      if (tabac[i,a-1] == 0) {

        nf[,a] <- nf[,a] + 1
        morta_NF <- mortalite_NF*RR2_H
        if (alea > morta_NF){                                                 # stay alive but sick
          etats[i,a] <- 1
        }
        if (alea <= morta_NF){                                                # transition : die having had a myocardial infarction after the first year
          etats[i,a] <- 2
          matrice[i,4] <- age_i
        }

      }

    }

    if (etats[i,a-1] == 2){                                                   # state 2 : die
      etats[i,a] <- 2
    }

    if (etats[i,a-1] == 3){                                                   # transition : the individual who died of a myocardial infarction during the first year passes to simple death the following year
      etats[i,a] <- 2
    }

    if (matrice[i,1]<=115 & etats[i,a]!= 2){                                  # aging of the individual
      matrice[i,1] <- matrice[i,1] + 1
    }
  }
  #prop

  
  # nb_people_0 <- as.numeric(length(etats[which(etats[,a] == 0)]))
  # nb_people_1 <- as.numeric(length(etats[which(etats[,a] == 1)]))
  nb_people_v[,a] <- sum(etats[,a] !=2) 
  

  # if((tabac[i,a] == 1) && (etats[i,a] != 2)) {
  #   f <- f +1
  # }
  # else{
  #   f <- f
  # }
  m_comb <- cbind(tabac, etats)
  
  # Compter le nombre d'éléments égaux à 1 dans la première colonne de m1 qui sont également égaux à 2 dans la première colonne de m2
  f[,a] <- sum(m_comb[,a] == 1 & m_comb[,a+81] != 2)
  af[,a] <- sum(m_comb[,a] == 2 & m_comb[,a+81] != 2)
  nf[,a] <- sum(m_comb[,a] == 0 & m_comb[,a+81] != 2)
  # f[,a] <- sum(tabac[which(tabac[i,a] == 1) & etats[i,a] != 2])
  # af <- as.numeric(length(tabac[which(tabac[,a] == 2) & etats[,a] != 2]))
  # nf <- as.numeric(length(tabac[which(tabac[,a] == 0) & etats[,a] != 2]))
  prop[1,a] <- f[,a]/nb_people_v[,a]
  prop[2,a] <- af[,a]/nb_people_v[,a]
  prop[3,a] <- nf[,a]/nb_people_v[,a]
  
  #a0j
  
  # for (a in 2:81) {
    A02[3,a] <- a02[a,1]/(prop[1,a]*2.82+prop[2,a]*1.7+prop[3,a])
    A02[1,a] <- A02[3,a]*2.82
    A02[2,a] <- A02[3,a]*1.7
  # }
  
  for (a in 2:5) {
    A01[3,a] <- a01[a,1]/(prop[1,a]*4.2+prop[2,a]*2.1+prop[3,a])
    A01[1,a] <- A01[3,a]*4.2
    A01[2,a] <- A01[3,a]*2.1
  }
  for (a in 6:15) {
    A01[3,a] <- a01[a,1]/(prop[1,a]*3+prop[2,a]*1.6+prop[3,a])
    A01[1,a] <- A01[3,a]*3
    A01[2,a] <- A01[3,a]*1.6
  }
  for (a in 16:25) {
    A01[3,a] <- a01[a,1]/(prop[1,a]*2.5+prop[2,a]*1.6+prop[3,a])
    A01[1,a] <- A01[3,a]*2.5
    A01[2,a] <- A01[3,a]*1.6
  }
  for (a in 26:35) {
    A01[3,a] <- a01[a,1]/(prop[1,a]*2.2+prop[2,a]*1.5+prop[3,a])
    A01[1,a] <- A01[3,a]*2.2
    A01[2,a] <- A01[3,a]*1.5
  }
  for (a in 36:81) {
    A01[3,a] <- a01[a,1]/(prop[1,a]*1.7+prop[2,a]*1.5+prop[3,a])
    A01[1,a] <- A01[3,a]*1.7
    A01[2,a] <- A01[3,a]*1.5
  }
  

}

list("matrice" = matrice, "etats" = etats, "tabac" = tabac, "prop" = prop, "A01"= A01, "A02" = A02)
#write.csv(matrice, file = paste("matrice",co,sep="_"))
#write.csv(etats, file = paste("etats",co,sep="_"))


names(res) <- 1920:1920
list_etats <- lapply(res, "[[", "etats")
list_matrice <- lapply(res, "[[", "matrice")
list_prop <- lapply(res, "[[", "prop")



plot(A01[1,])
plot(a01[,1])
plot(A02[1,])
plot(a02[,1])
table(etats[,60])
sum(prop[,20])






# Définition des données
x <- c(2,10,20,30,60)
y <- c(4.2,3,2.5,2.2,1.7)

OR <- matrix(c(0), nrow=2, ncol= 81)
rownames(OR) <- c("F", "AF")
OR

# Interpolation linéaire pour x = 40
for (i in 1:60){
  OR[1,i] <- approx(x, y, xout = i)$y 
}
OR
OR[1,1] <- 4.20
for (i in 60:81){
  OR[1,i] <- 1.7
}
OR


y <- c(2.1,1.6,1.6,1.5,1.5)

# Interpolation linéaire pour x = 40
for (i in 1:25){
  OR[2,i] <- approx(x, y, xout = i)$y 
}
OR
OR[2,1] <- 2.1
for (i in 25:81){
  OR[2,i] <- 1.5
}
OR


plot(OR[1,], ylim = c(1,4.5), xlim=c(0,65), type="l", lwd = 3, col = "#a5a58d", xaxt = "n", xlab = "Âge", ylab = "Risques relatifs")
axis(1,at=c(0,10,20,30,40,50,60,70,80), labels=c(35,45,55,65,75,85,95,105,115))
lines(OR[2,], lwd = 3 ,col="#bf7343")
title("Interpolation linéaire")
legend(x="topright", legend=c("Fumeurs","Anciens fumeurs"), col=c("#a5a58d","#bf7343"), pch=20, bty="n")

