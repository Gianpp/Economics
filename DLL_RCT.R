# Questo file replica il paper presente nel file README (l'impatto di un intervento di DDL su risultati degli studenti basati su ITBS e Logramos)

# STEP 0: caricamento delle librerie

library(QuantPsyc) # per standardizzare i risultati dei test (Make.Z) 
library(tableone) # per la creazione delle tavole di equivalenza di base (TableOne) 
library(survey) # per la creazione delle tavole di equivalenza di base
library(nlme) # per adattare modelli a effetti-misti (lme)
library(texreg) # per convertire gli output dei modelli statistici in R in tabelle di testo (screenreg) 

# ---------------------------------------
# STEP 1: leggere i dati

# 1) settare la working directory 

setwd("~/Desktop/Master Data Science/Economic and Financial Data Science/parte2/Progetto Economics")

# 2) leggere i dati: .csv-file 

DLL_itbs <- read.csv("DLL_itbs.csv")
DLL_lgrm <- read.csv("DLL_lgrm.csv")

# STEP 2: Calcolare l'attrition

# 1) Creare una nuova colonna che confronta due colonne "pre" e "post" per ogni categoria

# 1.1) per l'itbs

DLL_itbs$match_READING <- ifelse(is.na(DLL_itbs$Posttest_READING) | is.na(DLL_itbs$Pretest_READING), 1, 0)
DLL_itbs$match_LANGUAGE <- ifelse(is.na(DLL_itbs$Posttest_LANGUAGE) | is.na(DLL_itbs$Pretest_LANGUAGE), 1, 0)
DLL_itbs$match_VOCABULARY <- ifelse(is.na(DLL_itbs$Posttest_VOCABULARY) | is.na(DLL_itbs$Pretest_VOCABULARY), 1, 0)
DLL_itbs$match_TOTAL <- ifelse(is.na(DLL_itbs$Posttest_ELA.TOTAL) | is.na(DLL_itbs$Pretest_ELA.TOTAL), 1, 0)

# 1.2) per il Logramos

DLL_lgrm$match_READING <- ifelse(is.na(DLL_lgrm$Posttest_READING) | is.na(DLL_lgrm$Pretest_READING), 1, 0)
DLL_lgrm$match_LANGUAGE <- ifelse(is.na(DLL_lgrm$Posttest_LANGUAGE) | is.na(DLL_lgrm$Pretest_LANGUAGE), 1, 0)
DLL_lgrm$match_VOCABULARY <- ifelse(is.na(DLL_lgrm$Posttest_VOCABULARY) | is.na(DLL_lgrm$Pretest_VOCABULARY), 1, 0)
DLL_lgrm$match_TOTAL <- ifelse(is.na(DLL_lgrm$Posttest_ELA.TOTAL) | is.na(DLL_lgrm$Pretest_ELA.TOTAL), 1, 0)

# 2) Calcola l'attrition complessiva per ogni categoria

# 2.1) per l'itbs

attrition_itbsREADING <- sum(DLL_itbs$match_READING==1) / nrow(DLL_itbs) * 100
attrition_itbsLANGUAGE <- sum(DLL_itbs$match_LANGUAGE==1) / nrow(DLL_itbs) * 100
attrition_itbsVOCABULARY <- sum(DLL_itbs$match_VOCABULARY==1) / nrow(DLL_itbs) * 100
attrition_itbsTOTAL <- sum(DLL_itbs$match_TOTAL==1) / nrow(DLL_itbs) * 100

# 2.2) per il Logramos

attrition_lgrmREADING <- sum(DLL_lgrm$match_READING==1) / nrow(DLL_lgrm) * 100
attrition_lgrmLANGUAGE <- sum(DLL_lgrm$match_LANGUAGE==1) / nrow(DLL_lgrm) * 100
attrition_lgrmVOCABULARY <- sum(DLL_lgrm$match_VOCABULARY==1) / nrow(DLL_lgrm) * 100
attrition_lgrmTOTAL <- sum(DLL_lgrm$match_TOTAL==1) / nrow(DLL_lgrm) * 100

# 3) Creazione del dataframe per raccogliere i risultati

# 3.1) Creazione del dataframe vuoto

attrition_df <- data.frame(
  stringsAsFactors = FALSE,
  Reading = numeric(),
  Language = numeric(),
  Vocabulary = numeric(),
  Total = numeric()
)

# 3.2) Aggiunta delle righe e dei valori

attrition_df <- rbind(attrition_df, c(attrition_itbsREADING, attrition_itbsLANGUAGE, attrition_itbsVOCABULARY, attrition_itbsTOTAL))
attrition_df <- rbind(attrition_df, c(attrition_lgrmREADING, attrition_lgrmLANGUAGE, attrition_lgrmVOCABULARY, attrition_lgrmTOTAL))

# 3.3) Impostazione delle intestazioni delle righe e delle colonne

rownames(attrition_df) <- c("itbs", "lgrm")
colnames(attrition_df) <-c("READING", "LANGUAGE", "VOCABULARY", "TOTAL")

# 3.4) Stampa del dataframe

attrition_df

# 4) Calcolare l'attrition differenziale 

# 4.1) Calcolo attrition gruppo trattati, scala itbs per ogni categoria

treat_itbsREADING <- sum(DLL_itbs$T_assignment == 1 & DLL_itbs$match_READING == 1) / sum(DLL_itbs$T_assignment ==1) *100
treat_itbsLANGUAGE <- sum(DLL_itbs$T_assignment == 1 & DLL_itbs$match_LANGUAGE == 1) / sum(DLL_itbs$T_assignment == 1) * 100
treat_itbsVOCABULARY <- sum(DLL_itbs$T_assignment == 1 & DLL_itbs$match_VOCABULARY == 1) / sum(DLL_itbs$T_assignment == 1) * 100
treat_itbsTOTAL <- sum(DLL_itbs$T_assignment == 1 & DLL_itbs$match_TOTAL == 1) / sum(DLL_itbs$T_assignment == 1) * 100

# 4.2) Calcolo attrition gruppo di controllo, scala itbs per ogni categoria

contr_itbsREADING <- sum(DLL_itbs$T_assignment == 0 & DLL_itbs$match_READING == 1) / sum(DLL_itbs$T_assignment == 0) * 100
contr_itbsLANGUAGE <- sum(DLL_itbs$T_assignment == 0 & DLL_itbs$match_LANGUAGE == 1) / sum(DLL_itbs$T_assignment == 0) * 100
contr_itbsVOCABULARY <- sum(DLL_itbs$T_assignment == 0 & DLL_itbs$match_VOCABULARY == 1) / sum(DLL_itbs$T_assignment == 0) * 100
contr_itbsTOTAL <- sum(DLL_itbs$T_assignment == 0 & DLL_itbs$match_TOTAL == 1) / sum(DLL_itbs$T_assignment == 0) * 100

# 4.3) differenza tra trattati e non trattati scala itbs
diff_itbsREADING <- abs(treat_itbsREADING - contr_itbsREADING)
diff_itbsLANGUAGE <- abs(treat_itbsLANGUAGE - contr_itbsLANGUAGE)
diff_itbsVOCABULARY <- abs(treat_itbsVOCABULARY - contr_itbsVOCABULARY)
diff_itbsTOTAL <- abs(treat_itbsTOTAL - contr_itbsTOTAL)

# 4.4) Calcolo attrition gruppo trattati, scala Logramos per ogni categoria

treat_lgrmREADING <- sum(DLL_lgrm$T_assignment == 1 & DLL_lgrm$match_READING == 1) / sum(DLL_lgrm$T_assignment ==1) *100
treat_lgrmLANGUAGE <- sum(DLL_lgrm$T_assignment == 1 & DLL_lgrm$match_LANGUAGE == 1) / sum(DLL_lgrm$T_assignment == 1) * 100
treat_lgrmVOCABULARY <- sum(DLL_lgrm$T_assignment == 1 & DLL_lgrm$match_VOCABULARY == 1) / sum(DLL_lgrm$T_assignment == 1) * 100
treat_lgrmTOTAL <- sum(DLL_lgrm$T_assignment == 1 & DLL_lgrm$match_TOTAL == 1) / sum(DLL_lgrm$T_assignment == 1) * 100

# 4.5) Calcolo attrition gruppo di controllo, scala Logramos per ogni categoria

contr_lgrmREADING <- sum(DLL_lgrm$T_assignment == 0 & DLL_lgrm$match_READING == 1) / sum(DLL_lgrm$T_assignment == 0) * 100
contr_lgrmLANGUAGE <- sum(DLL_lgrm$T_assignment == 0 & DLL_lgrm$match_LANGUAGE == 1) / sum(DLL_lgrm$T_assignment == 0) * 100
contr_lgrmVOCABULARY <- sum(DLL_lgrm$T_assignment == 0 & DLL_lgrm$match_VOCABULARY == 1) / sum(DLL_lgrm$T_assignment == 0) * 100
contr_lgrmTOTAL <- sum(DLL_lgrm$T_assignment == 0 & DLL_lgrm$match_TOTAL == 1) / sum(DLL_lgrm$T_assignment == 0) * 100

# 4.6) differenza tra trattati e non trattati scala Logramos

diff_lgrmREADING <- abs(treat_lgrmREADING - contr_lgrmREADING)
diff_lgrmLANGUAGE <- abs(treat_lgrmLANGUAGE - contr_lgrmLANGUAGE)
diff_lgrmVOCABULARY <- abs(treat_lgrmVOCABULARY - contr_lgrmVOCABULARY)
diff_lgrmTOTAL <- abs(treat_lgrmTOTAL - contr_lgrmTOTAL)

# 4.7) Raccolta dei risultati nel dataframe

# Creazione del dataframe vuoto

diff_attrition_df <- data.frame(
  stringsAsFactors = FALSE,
  Reading = numeric(),
  Language = numeric(),
  Vocabulary = numeric(),
  Total = numeric()
)

# Aggiunta delle righe e dei valori
diff_attrition_df <- rbind(diff_attrition_df, c(diff_itbsREADING, diff_itbsLANGUAGE, diff_itbsVOCABULARY, diff_itbsTOTAL))
diff_attrition_df <- rbind(diff_attrition_df, c(diff_lgrmREADING, diff_lgrmLANGUAGE, diff_lgrmVOCABULARY, diff_lgrmTOTAL))

# Impostazione delle intestazioni delle righe e delle colonne
rownames(diff_attrition_df) <- c("itbs", "lgrm")
colnames(diff_attrition_df) <-c("READING", "LANGUAGE", "VOCABULARY", "TOTAL")

# Stampa del dataframe
diff_attrition_df

# STEP 5: Creazione della funzione 'cleaning'

cleaning_itbs_lgrm <- function (DATA_a,POSTTESTM, PRETESTM) { 
    
    # a. standardize test scores 
    DATA_a.z<- data.frame(Make.Z(DATA_a[,c("Posttest_READING", "Posttest_LANGUAGE", "Posttest_VOCABULARY", "Posttest_ELA.TOTAL", 
                                           "Pretest_READING", "Pretest_LANGUAGE", "Pretest_VOCABULARY", "Pretest_ELA.TOTAL")]))
    
    # b. rename Z-scores 
    names(DATA_a.z)[1:8] <- c('Posttest_READING_z', 'Posttest_LANGUAGE_z', 'Posttest_VOCABULARY_z', 'Posttest_ELA.TOTAL_z', 
                              'Pretest_READING_z', 'Pretest_LANGUAGE_z', 'Pretest_VOCABULARY_z', 'Pretest_ELA.TOTAL_z')
    
    # c. merge with raw dataset 
    DATA_b<-cbind(DATA_a, DATA_a.z) 
    
    # d. delete cases where pre or posttest are missing 
    DATA_c<-DATA_b[is.na(DATA_b[, POSTTESTM])==FALSE & is.na(DATA_b[, PRETESTM])==FALSE, ] 
    
    return(DATA_c)
}

# STEP 6: Applicazione della funzione alle varie categorie

# 1) per l'itbs

itbs_READING <- cleaning_itbs_lgrm(DLL_itbs, "Posttest_READING_z", "Pretest_READING_z")
itbs_LANGUAGE <- cleaning_itbs_lgrm(DLL_itbs, "Posttest_LANGUAGE_z", "Pretest_LANGUAGE_z")
itbs_VOCABULARY <- cleaning_itbs_lgrm(DLL_itbs, "Posttest_VOCABULARY_z", "Pretest_VOCABULARY_z")
itbs_ELA.TOTAL <- cleaning_itbs_lgrm(DLL_itbs, "Posttest_ELA.TOTAL_z", "Pretest_ELA.TOTAL_z")

# 2) per il Logramos

lgrm_READING <- cleaning_itbs_lgrm(DLL_lgrm, "Posttest_READING_z", "Pretest_READING_z")
lgrm_LANGUAGE <- cleaning_itbs_lgrm(DLL_lgrm, "Posttest_LANGUAGE_z", "Pretest_LANGUAGE_z")
lgrm_VOCABULARY <- cleaning_itbs_lgrm(DLL_lgrm, "Posttest_VOCABULARY_z", "Pretest_VOCABULARY_z")
lgrm_ELA.TOTAL <- cleaning_itbs_lgrm(DLL_lgrm, "Posttest_ELA.TOTAL_z", "Pretest_ELA.TOTAL_z")

# ---------------------------------------
# STEP 7: Visualizziamo la composizione iniziale e finale dei gruppi

# 1) Composizione iniziale tramite assegnazione casuale 

table(DLL_itbs$Group) 
table(DLL_lgrm$Group)

# 2) Composizione finale 

# 2.1) itbs

table(itbs_READING$Group)
table(itbs_LANGUAGE$Group)
table(itbs_VOCABULARY$Group)
table(itbs_ELA.TOTAL$Group)

# 2.2) logramos 

table(lgrm_READING$Group)
table(lgrm_LANGUAGE$Group)
table(lgrm_VOCABULARY$Group)
table(lgrm_ELA.TOTAL$Group)

# ---------------------------------------
# STEP 4: Verifica dell'equivalenza di base: 

# 1) selezione delle colonne pre test 

(cov.nam_itbs_lgrm <- c('Pretest_READING', 'Pretest_LANGUAGE', 'Pretest_VOCABULARY', 'Pretest_ELA.TOTAL')) 

# 2) creazione delle tabelle

# 2.1) itbs

itbs_read_by_trt <- CreateTableOne(vars = cov.nam_itbs_lgrm, data = itbs_READING, strata = "Group", test = FALSE)
itbs_read_by_trt_smd <- data.frame(print(itbs_read_by_trt, showALLevels = TRUE, smd = TRUE))

itbs_lang_by_trt <- CreateTableOne(vars = cov.nam_itbs_lgrm, data = itbs_LANGUAGE, strata = "Group", test = FALSE)
itbs_lang_by_trt_smd <- data.frame(print(itbs_lang_by_trt, showALLevels = TRUE, smd = TRUE))

itbs_voca_by_trt <- CreateTableOne(vars = cov.nam_itbs_lgrm, data = itbs_VOCABULARY, strata = "Group", test = FALSE)
itbs_voca_by_trt_smd <- data.frame(print(itbs_voca_by_trt, showALLevels = TRUE, smd = TRUE))

itbs_ttl_by_trt <- CreateTableOne(vars = cov.nam_itbs_lgrm, data = itbs_ELA.TOTAL, strata = "Group", test = FALSE)
itbs_ttl_by_trt_smd <- data.frame(print(itbs_ttl_by_trt, showALLevels = TRUE, smd = TRUE))

# 2.2) logramos

lgrm_read_by_trt <- CreateTableOne(vars = cov.nam_itbs_lgrm, data = lgrm_READING, strata = "Group", test = FALSE)
lgrm_read_by_trt_smd <- data.frame(print(lgrm_read_by_trt, showALLevels = TRUE, smd = TRUE))

lgrm_lang_by_trt <- CreateTableOne(vars = cov.nam_itbs_lgrm, data = lgrm_LANGUAGE, strata = "Group", test = FALSE)
lgrm_lang_by_trt_smd <- data.frame(print(lgrm_lang_by_trt, showALLevels = TRUE, smd = TRUE))

lgrm_voca_by_trt <- CreateTableOne(vars = cov.nam_itbs_lgrm, data = lgrm_VOCABULARY, strata = "Group", test = FALSE)
lgrm_voca_by_trt_smd <- data.frame(print(lgrm_voca_by_trt, showALLevels = TRUE, smd = TRUE))

lgrm_ttl_by_trt <- CreateTableOne(vars = cov.nam_itbs_lgrm, data = lgrm_ELA.TOTAL, strata = "Group", test = FALSE)
lgrm_ttl_by_trt_smd <- data.frame(print(lgrm_ttl_by_trt, showALLevels = TRUE, smd = TRUE))

# STEP 5: Stima dell'outcome 

# 1) creazione della funzione per modelli con intercetta casuale (random intercept)

DLL_RI_itbs_lgrm <- function (DATA1, DATA2, DATA3, DATA4){
    
    M1<-lme(Posttest_READING_z ~ T_assignment + Pretest_READING_z, random = ~ 1 | SCHOOLID, data = DATA1, control=list(opt="optim"))
    M2<-lme(Posttest_LANGUAGE_z ~ T_assignment + Pretest_LANGUAGE_z, random = ~ 1 | SCHOOLID, data = DATA2, control=list(opt="optim"))
    M3<-lme(Posttest_VOCABULARY_z ~ T_assignment + Pretest_VOCABULARY_z, random = ~ 1 | SCHOOLID, data = DATA3, control=list(opt="optim"))
    M4<-lme(Posttest_ELA.TOTAL_z ~ T_assignment + Pretest_ELA.TOTAL_z, random = ~ 1 | SCHOOLID, data = DATA4, control=list(opt="optim"))
    
    modellist<-list(M1, M2, M3, M4)
    results <- screenreg (modellist, digits = 3, 
                          custom.model.names = c("READING", "LANGUAGE", "VOCABULARY", "TOTAL"))
    return (results)
}

# 2) Applicazione della funzione e visualizzazione dei risultati

# 2.1) itbs

(itbs_RI <- DLL_RI_itbs_lgrm(itbs_READING,  itbs_LANGUAGE,  itbs_VOCABULARY,  itbs_ELA.TOTAL))

# 2.2) logramos

(lgrm_RI <- DLL_RI_itbs_lgrm(lgrm_READING,  lgrm_LANGUAGE,  lgrm_VOCABULARY,  lgrm_ELA.TOTAL))
